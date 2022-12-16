module IR = struct
  type t =
    (* special *)
    | Debug
    (* functions *)
    | SysCall of int
    | Call of int
    | DefBegin of string | DefEnd
    | LamBegin | LamEnd
    | Param of int * int (* n, total *)
    (* integral math *)
    | IAdd | ISub | IDiv | IMul | IMod
    (* values *)
    | IPush of int | LPush of string

    let to_string op =
      let spf = Printf.sprintf in
      match op with
      | Debug -> "debug"
      | SysCall (n) -> spf "syscall %d" n
      | Call (nargs) -> spf "call %d" nargs
      | DefBegin (name) -> spf "begin def %s" name
      | DefEnd -> "end def"
      | LamBegin -> "begin lambda"
      | LamEnd -> "end lambda"
      | Param (n, tot) -> spf "param %d/%d" (n + 1) tot
      | IAdd -> "iadd"
      | ISub -> "isub"
      | IMul -> "imul"
      | IDiv -> "idiv"
      | IMod -> "imod"
      | IPush (n) -> spf "ipush %d" n
      | LPush (lbl) -> spf "lpush %s" lbl
end

module ASM = struct
  type reg =
    | Rax | Rbx | Rcx | Rdx

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

  let to_string op =
    let reg_to_string r =
      "%" ^ match r with
      | Rax -> "rax"
      | Rbx -> "rbx"
      | Rcx -> "rcx"
      | Rdx -> "rdx"
    in
    let indent = String.make 8 ' ' in
    let spf = Printf.sprintf in
    let fmt_op inst_str arg_str =
      indent ^
      if String.equal arg_str "" then
        inst_str
      else
        spf "%-8s%s" inst_str arg_str
    in
    let inst name args =
      args
      |> List.map reg_to_string
      |> String.concat ", "
      |> fmt_op name
    in
    match op with
    | Comment (s)     -> indent ^ "// " ^ s
    | Hack (s, args)  -> fmt_op s args
    | Label (s)       -> spf "%s:" s
    | IPush (n)       -> fmt_op "push" @@ spf "$%d" n
    | LPush (s)       -> fmt_op "push" @@ spf "$%s" s
    | DropN (n)       -> fmt_op "add" @@ spf "$%d, %%rsp" (8 * n)
    | Zero (r)        -> inst "xor" [r; r]
    | Pop (r)         -> inst "pop"  [r]
    | Push (r)        -> inst "push" [r]
    | IAdd (src, dst) -> inst "add"  [src; dst]
    | ISub (src, dst) -> inst "sub"  [src; dst]
    | IMul (r)        -> inst "mul"  [r]
    | IDiv (r)        -> inst "div"  [r]
    | Call (r)        -> fmt_op "call" @@ spf "*%s" @@ reg_to_string r
    | SysCall         -> fmt_op "syscall" ""
    | Ret             -> fmt_op "ret" ""
end

module Builder = struct
  module StrMap = Map.Make(String)

  type t = {
    params: int StrMap.t;
    ir: IR.t array;
  }

  let init: t = {
    params = StrMap.empty;
    ir = [||];
  }

  let set_params params b =
    { b with params = params }

  let drop_params b =
    { b with params = StrMap.empty }

  let get_param name b =
    StrMap.find_opt name b.params

  let nparams b =
    StrMap.bindings b.params
    |> List.length

  let append_slice (ops: IR.t array) (b: t): t =
    { b with ir = Array.append b.ir ops }

  let append (op: IR.t) (b: t): t =
    append_slice [|op|] b
end

let compile_error msg =
  Printf.eprintf "error: %s" msg;
  exit 1

let call_to (name: string) (nargs: int): IR.t array =
  [|LPush (name); Call (nargs)|]

let lower_symbol (sym: string) (nargs: int) (b: Builder.t): IR.t array =
  match Builder.get_param sym b with
  | Some (n) -> [|Param (n, Builder.nparams b)|]
  | None -> call_to sym nargs

let rec build_call (group: Astnode.t list) (b: Builder.t): Builder.t =
  assert (List.length group > 0);
  let head, tail = List.hd group, List.tl group in
  match head with
  | Symbol ("def") -> begin
    (* compile def *)
    assert (List.length tail > 0);
    let sym, body = List.hd tail, List.tl tail in
    b
    |> Builder.append (DefBegin (Astnode.sym sym))
    |> build_values body
    |> Builder.append DefEnd
  end
  | Symbol ("lambda") -> begin
    (* compile lambda *)
    assert (List.length tail > 0);
    let params, body = List.hd tail, List.tl tail in
    let param_map =
      Astnode.group params
      |> List.mapi (fun i sym -> (Astnode.sym sym), i)
      |> List.to_seq
      |> Builder.StrMap.of_seq
    in
    b
    |> Builder.set_params param_map
    |> Builder.append LamBegin
    |> build_values body
    |> Builder.append LamEnd
    |> Builder.drop_params
  end
  | Symbol (s) -> begin
    (* compile call *)
    let slice: IR.t array =
      match s with
      | "do"      -> [||]
      | "+"       -> [|IAdd|]
      | "-"       -> [|ISub|]
      | "*"       -> [|IMul|]
      | "/"       -> [|IDiv|]
      | "mod"     -> [|IMod|]
      | "debug"   -> [|Debug|]
      | "syscall" -> [|SysCall ((List.length tail) - 1)|]
      | sym       -> lower_symbol sym (List.length tail) b
    in
    List.fold_left (fun b node -> build_value node 0 b) b tail
    |> Builder.append_slice slice
  end
  | node -> build_value node (List.length tail) b

and build_value (node: Astnode.t) (nargs: int) (b: Builder.t): Builder.t =
  match node with
  | Symbol (sym) -> Builder.append_slice (lower_symbol sym nargs b) b
  | Integer (n) -> Builder.append (IPush n) b
  | Group (xs) -> build_call xs b

and build_values (nodes: Astnode.t list) (b: Builder.t): Builder.t =
  List.fold_left (fun b node -> build_value node 0 b) b nodes

let compile (b: Builder.t): ASM.t array =
  let compile_op (op: IR.t): ASM.t array =
    match op with
    | Debug -> [|
      LPush ("__debug");
      Pop (Rax);
      Call (Rax);
      Pop (Rax);
    |]
    | SysCall (n) ->
      let setups: ASM.t array = [|
        Hack ("pop", "%r9");
        Hack ("pop", "%r8");
        Hack ("pop", "%r10");
        Hack ("pop", "%rdx");
        Hack ("pop", "%rsi");
        Hack ("pop", "%rdi");
      |] in
      Array.concat [(Array.sub setups (6 - n) n); [|Pop (Rax); SysCall|]]
    | Call (nargs) -> [|
      Pop (Rax);
      Call (Rax);
      DropN (nargs);
      Push (Rax);
    |]
    | DefBegin (name) -> [|Label (name)|]
    | DefEnd -> [|Ret|]
    | LamBegin -> [|
      Hack ("push", "%rbp");
      Hack ("mov", "%rsp, %rbp");
    |]
    | LamEnd -> [|
      Pop (Rax);
      Hack ("pop", "%rbp");
    |]
    | Param (n, tot) ->
      let offset = (1 + tot - n) * 8 in
      [|Hack ("push", Printf.sprintf "+%d(%%rbp)" offset)|]
    | IAdd -> [|
      Pop (Rbx);
      Pop (Rax);
      IAdd (Rbx, Rax);
      Push (Rax);
    |]
    | ISub -> [|
      Pop (Rbx);
      Pop (Rax);
      ISub (Rbx, Rax);
      Push (Rax);
    |]
    | IMul -> [|
      Pop (Rax);
      Pop (Rbx);
      IMul (Rbx);
      Push (Rax);
    |]
    | IDiv -> [|
      Zero (Rdx);
      Pop (Rbx);
      Pop (Rax);
      IDiv (Rbx);
      Push (Rax);
    |]
    | IMod -> [|
      Zero (Rdx);
      Pop (Rbx);
      Pop (Rax);
      IDiv (Rbx);
      Push (Rdx);
    |]
    | IPush (n) -> [|IPush (n)|]
    | LPush (s) -> [|LPush (s)|]
  in
  let compile_op_commented op =
    Array.append [|ASM.Comment (IR.to_string op)|] @@ compile_op op
  in
  Array.map compile_op_commented b.ir
  |> Array.to_list
  |> Array.concat

let read_asm_file filename =
  Util.read_file filename
  |> String.split_on_char '\n'
  |> Array.of_list

let header = read_asm_file "templates/header.S"

let generate (target: string) (nodes: Astnode.t list): unit =
  let built = build_values nodes Builder.init in
  let asm = Array.map ASM.to_string @@ compile built in
  let source =
    Array.concat [header; asm]
    |> Array.to_list
    |> String.concat "\n"
  in
  let chan = open_out target in
  Printf.fprintf chan "%s\n" source;
  close_out chan;
