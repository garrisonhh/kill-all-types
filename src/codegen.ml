module IR = struct
  type t =
    (* special *)
    | Debug
    (* functions *)
    | SysCall of int
    | Call
    | DefBegin of string
    | DefEnd
    | LamBegin
    | LamEnd of int (* nargs *)
    | Param of int * int (* n, total *)
    (* integral math *)
    | IAdd
    | ISub
    | IDiv
    | IMul
    | IMod
    (* values *)
    | IPush of int
    | LPush of string
    (* if *)
    | If of string (* else lbl *)
    | Else of string * string (* else lbl, final lbl *)
    | EndIf of string (* final lbl *)

  let to_string op =
    let spf = Printf.sprintf in
    match op with
    | Debug -> "debug"
    | SysCall n -> spf "syscall %d" n
    | Call -> "call"
    | DefBegin name -> spf "begin def %s" name
    | DefEnd -> "end def"
    | LamBegin -> "begin lambda"
    | LamEnd nargs -> spf "end lambda (%d args)" nargs
    | Param (n, tot) -> spf "param %d/%d" (n + 1) tot
    | IAdd -> "iadd"
    | ISub -> "isub"
    | IMul -> "imul"
    | IDiv -> "idiv"
    | IMod -> "imod"
    | IPush n -> spf "ipush %d" n
    | LPush lbl -> spf "lpush %s" lbl
    | If _ -> "if"
    | Else (_, _) -> "else"
    | EndIf _ -> "endif"
end

module ASM = struct
  type reg = Rax | Rbx | Rdx

  type t =
    | Comment of string
    | Hack of string * string (* TODO remove *)
    | Label of string
    | IPush of int
    | LPush of string (* push a label as a ptr *)
    | DropN of int
    | Zero of reg
    | Pop of reg
    | Push of reg
    | IAdd of reg * reg
    | ISub of reg * reg
    | IMul of reg (* multiply reg by rax *)
    | IDiv of reg
    | Call of reg
    | SysCall
    | Ret
    | Jump of string
    | Test of reg
    | JumpIf of string
    | JumpIfN of string

  let to_string op =
    let reg_to_string r =
      "%" ^ match r with Rax -> "rax" | Rbx -> "rbx" | Rdx -> "rdx"
    in
    let indent = String.make 8 ' ' in
    let spf = Printf.sprintf in
    let fmt_op inst_str arg_str =
      indent
      ^
      if String.equal arg_str "" then inst_str
      else spf "%-8s%s" inst_str arg_str
    in
    let inst name args =
      args |> List.map reg_to_string |> String.concat ", " |> fmt_op name
    in
    match op with
    | Comment s -> indent ^ "// " ^ s
    | Hack (s, args) -> fmt_op s args
    | Label s -> spf "%s:" s
    | IPush n -> fmt_op "push" @@ spf "$%d" n
    | LPush s -> fmt_op "push" @@ spf "$%s" s
    | DropN n -> fmt_op "add" @@ spf "$%d, %%rsp" (8 * n)
    | Zero r -> inst "xor" [ r; r ]
    | Pop r -> inst "pop" [ r ]
    | Push r -> inst "push" [ r ]
    | IAdd (src, dst) -> inst "add" [ src; dst ]
    | ISub (src, dst) -> inst "sub" [ src; dst ]
    | IMul r -> inst "mul" [ r ]
    | IDiv r -> inst "div" [ r ]
    | Call r -> fmt_op "call" @@ spf "*%s" @@ reg_to_string r
    | SysCall -> fmt_op "syscall" ""
    | Ret -> fmt_op "ret" ""
    | Jump lbl -> fmt_op "jmp" lbl
    | Test r -> inst "test" [ r; r ]
    | JumpIf lbl -> fmt_op "jnz" lbl
    | JumpIfN lbl -> fmt_op "jz" lbl
end

module Builder = struct
  module StrMap = Map.Make (String)

  type t = { label : int; params : int StrMap.t; ir : IR.t array }

  let init : t = { label = 0; params = StrMap.empty; ir = [||] }

  let next_label b =
    let label = Printf.sprintf "__label%d" b.label in
    (label, { b with label = b.label + 1 })

  let set_params params b = { b with params }
  let drop_params b = { b with params = StrMap.empty }
  let get_param name b = StrMap.find_opt name b.params
  let nparams b = StrMap.bindings b.params |> List.length

  let append_slice (ops : IR.t array) (b : t) : t =
    { b with ir = Array.append b.ir ops }

  let append (op : IR.t) (b : t) : t = append_slice [| op |] b
end

let compile_error msg =
  Printf.eprintf "error: %s" msg;
  exit 1

let call_to (name : string) : IR.t array = [| LPush name; Call |]

let lower_symbol (sym : string) (b : Builder.t) : IR.t array =
  match Builder.get_param sym b with
  | Some n -> [| Param (n, Builder.nparams b) |]
  | None -> call_to sym

let rec build_call (group : Astnode.t list) (b : Builder.t) : Builder.t =
  assert (List.length group > 0);
  let head, tail = (List.hd group, List.tl group) in
  match head with
  | Symbol "def" ->
      (* compile def *)
      assert (List.length tail > 0);
      let sym, body = (List.hd tail, List.tl tail) in
      b
      |> Builder.append (DefBegin (Astnode.sym sym))
      |> build_values body
      |> Builder.append DefEnd
  | Symbol "lambda" ->
      (* compile lambda *)
      assert (List.length tail > 0);
      let params, body = (List.hd tail, List.tl tail) in
      let param_map =
        Astnode.group params
        |> List.mapi (fun i sym -> (Astnode.sym sym, i))
        |> List.to_seq
        |> Builder.StrMap.of_seq
      in
      b
      |> Builder.set_params param_map
      |> Builder.append LamBegin
      |> build_values body
      |> (fun b -> Builder.append (LamEnd (Builder.nparams b)) b)
      |> Builder.drop_params
  | Symbol "if" ->
      let cond, ift, iff =
        match tail with
        | [ cond; ift; iff ] -> (cond, ift, iff)
        | _ -> assert false
      in
      let else_lbl, b = Builder.next_label b in
      let final_lbl, b = Builder.next_label b in
      b
      |> build_value cond
      |> Builder.append (If else_lbl)
      |> build_value ift
      |> Builder.append (Else (else_lbl, final_lbl))
      |> build_value iff
      |> Builder.append (EndIf final_lbl)
  | Symbol s ->
      (* compile call *)
      let slice : IR.t array =
        match s with
        | "do" -> [||]
        | "+" -> [| IAdd |]
        | "-" -> [| ISub |]
        | "*" -> [| IMul |]
        | "/" -> [| IDiv |]
        | "mod" -> [| IMod |]
        | "debug" -> [| Debug |]
        | "syscall" -> [| SysCall (List.length tail - 1) |]
        | sym -> lower_symbol sym b
      in
      List.fold_left (fun b node -> build_value node b) b tail
      |> Builder.append_slice slice
  | node -> build_value node b

and build_value (node : Astnode.t) (b : Builder.t) : Builder.t =
  match node with
  | Symbol sym -> Builder.append_slice (lower_symbol sym b) b
  | Integer n -> Builder.append (IPush n) b
  | Group xs -> build_call xs b

and build_values (nodes : Astnode.t list) (b : Builder.t) : Builder.t =
  List.fold_left (fun b node -> build_value node b) b nodes

let compile (b : Builder.t) : ASM.t array =
  let compile_op (op : IR.t) : ASM.t array =
    match op with
    | Debug -> [| LPush "__debug"; Pop Rax; Call Rax; Pop Rax |]
    | SysCall n ->
        let setups : ASM.t array =
          [|
            Hack ("pop", "%r9");
            Hack ("pop", "%r8");
            Hack ("pop", "%r10");
            Hack ("pop", "%rdx");
            Hack ("pop", "%rsi");
            Hack ("pop", "%rdi");
          |]
        in
        Array.concat [ Array.sub setups (6 - n) n; [| Pop Rax; SysCall |] ]
    | Call -> [| Pop Rax; Call Rax; Push Rax |]
    | DefBegin name -> [| Label name |]
    | DefEnd -> [| Ret |]
    | LamBegin -> [| Hack ("push", "%rbp"); Hack ("mov", "%rsp, %rbp") |]
    | LamEnd nargs ->
        [|
          (* ret setup *)
          Pop Rax;
          Hack ("pop", "%rbp");
          (* delete params *)
          Pop Rbx;
          DropN nargs;
          Push Rbx;
        |]
    | Param (n, tot) ->
        let offset = (1 + tot - n) * 8 in
        [| Hack ("push", Printf.sprintf "+%d(%%rbp)" offset) |]
    | IAdd -> [| Pop Rbx; Pop Rax; IAdd (Rbx, Rax); Push Rax |]
    | ISub -> [| Pop Rbx; Pop Rax; ISub (Rbx, Rax); Push Rax |]
    | IMul -> [| Pop Rax; Pop Rbx; IMul Rbx; Push Rax |]
    | IDiv -> [| Zero Rdx; Pop Rbx; Pop Rax; IDiv Rbx; Push Rax |]
    | IMod -> [| Zero Rdx; Pop Rbx; Pop Rax; IDiv Rbx; Push Rdx |]
    | IPush n -> [| IPush n |]
    | LPush s -> [| LPush s |]
    | If else_lbl -> [| Pop Rax; Test Rax; JumpIfN else_lbl |]
    | Else (else_lbl, final_lbl) -> [| Jump final_lbl; Label else_lbl |]
    | EndIf final_lbl -> [| Label final_lbl |]
  in
  let compile_op_commented op =
    Array.append [| ASM.Comment (IR.to_string op) |] @@ compile_op op
  in
  Array.map compile_op_commented b.ir |> Array.to_list |> Array.concat

let read_asm_file filename =
  Util.read_file filename |> String.split_on_char '\n' |> Array.of_list

let header = read_asm_file "templates/header.S"

let generate (target : string) (nodes : Astnode.t list) : unit =
  let built = build_values nodes Builder.init in
  let asm = Array.map ASM.to_string @@ compile built in
  let source =
    Array.concat [ header; asm ] |> Array.to_list |> String.concat "\n"
  in
  let chan = open_out target in
  Printf.fprintf chan "%s\n" source;
  close_out chan
