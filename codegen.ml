module IR = struct
  type t =
    (* special *)
    | Debug
    (* functions *)
    | SysCall of int
    | Call
    | FnBegin of string
    | FnEnd
    (* integral math *)
    | IAdd | ISub | IDiv | IMul | IMod
    (* values *)
    | IPush of int | LPush of string

    let to_string op =
      let spf = Printf.sprintf in
      match op with
      | Debug -> "debug"
      | SysCall (n) -> spf "syscall %d" n
      | Call -> "call"
      | FnBegin (name) -> spf "begin fn %s" name
      | FnEnd -> "end fn"
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
    | Zero of reg
    | Pop of reg
    | Push of reg
    | IAdd of reg * reg
    | ISub of reg * reg
    | IMul of reg (* multiply reg by rax *)
    | IDiv of reg
    | RawCall of reg
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
    | Zero (r)        -> inst "xor" [r; r]
    | Pop (r)         -> inst "pop"  [r]
    | Push (r)        -> inst "push" [r]
    | IAdd (src, dst) -> inst "add"  [src; dst]
    | ISub (src, dst) -> inst "sub"  [src; dst]
    | IMul (r)        -> inst "mul"  [r]
    | IDiv (r)        -> inst "div"  [r]
    | RawCall (r)     -> fmt_op "call" @@ spf "*%s" @@ reg_to_string r
    | SysCall         -> fmt_op "syscall" ""
    | Ret             -> fmt_op "ret" ""
end

module Builder = struct
  type t =
    IR.t array

  let append (op: IR.t) (b: t): t =
    Array.append b [|op|]

  let append_slice (ops: IR.t array) (b: t): t =
    Array.append b ops
end

let compile_error msg =
  Printf.eprintf "error: %s" msg;
  exit 1

let call_to (name: string): IR.t array =
  [|LPush (name); Call|]

let rec build_call (group: Astnode.t list) (b: Builder.t): Builder.t =
  assert (List.length group > 0);
  let head, tail = List.hd group, List.tl group in
  match head with
  | Symbol ("def") -> begin
    assert (List.length tail > 1);
    let sym, body = List.hd tail, List.tl tail in
    let name =
      match sym with
      | Symbol (name) -> name
      | _ -> compile_error "def must have a symbol as the first argument"
    in
    b
    |> Builder.append (FnBegin (name))
    |> build_values body
    |> Builder.append FnEnd
  end
  | Symbol (s) -> begin
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
      | sym       -> call_to sym
    in
    List.fold_left (Fun.flip build_value) b tail
    |>  Builder.append_slice slice
  end
  | _ -> compile_error "attempted to call uncallable"

and build_value (node: Astnode.t) (b: Builder.t): Builder.t =
  match node with
  | Symbol (sym) -> Builder.append_slice (call_to sym) b
  | Integer (n) -> Builder.append (IPush n) b
  | Group (xs) -> build_call xs b

and build_values (nodes: Astnode.t list) (b: Builder.t): Builder.t =
  List.fold_left (Fun.flip build_value) b nodes

let compile (b: Builder.t): ASM.t array =
  let compile_op (op: IR.t): ASM.t array =
    match op with
    | Debug -> [|
      LPush ("__debug");
      Pop (Rax);
      RawCall (Rax);
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
    | Call -> [|
      Pop (Rax);
      RawCall (Rax);
      Push (Rax);
    |]
    | FnBegin (name) -> [|
      Label (name);
      Hack ("push", "%rbp");
      Hack ("mov", "%rsp, %rbp");
    |]
    | FnEnd -> [|
      (* returns value in rax *)
      Pop (Rax);
      Hack ("pop", "%rbp");
      Ret;
    |]
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
  Array.map compile_op_commented b
  |> Array.to_list
  |> Array.concat

let read_asm_file filename =
  Util.read_file filename
  |> String.split_on_char '\n'
  |> Array.of_list

let header = read_asm_file "templates/header.S"

let generate (target: string) (nodes: Astnode.t list): unit =
  let built = build_values nodes [||] in
  let asm = Array.map ASM.to_string @@ compile built in
  let source =
    Array.concat [header; asm]
    |> Array.to_list
    |> String.concat "\n"
  in
  let chan = open_out target in
  Printf.fprintf chan "%s\n" source;
  close_out chan;
