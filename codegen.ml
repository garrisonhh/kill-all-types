module IR = struct
  type t =
    (* special *)
    | Debug
    (* syscalls *)
    | Syscall1 | Syscall2 | Syscall3 | Syscall4 | Syscall5 | Syscall6
    (* integral math *)
    | IAdd | ISub | IDiv | IMul | IMod
    (* values *)
    | IPush of int
end

module ASM = struct
  type reg = Rax | Rbx

  type t =
    | IPush of int
    | LPush of string (* push a label as a ptr *)
    | Pop of reg
    | Push of reg
    | IAdd of reg * reg
    | ISub of reg * reg
    | IMul of reg (* multiply reg by rax *)
    | Rawcall of reg
    | Syscall

  let to_string op =
    let reg_to_string r =
      "%" ^ match r with
      | Rax -> "rax"
      | Rbx -> "rbx"
    in
    let inst name args =
      args
      |> List.map reg_to_string
      |> String.concat ", "
      |> Printf.sprintf "%s %s" name
    in
    (String.make 8 ' ') ^ match op with
    | IPush (n)       -> Printf.sprintf "push $%d" n
    | LPush (s)       -> Printf.sprintf "push $%s" s
    | Pop (r)         -> inst "pop"  [r]
    | Push (r)        -> inst "push" [r]
    | IAdd (src, dst) -> inst "add"  [src; dst]
    | ISub (src, dst) -> inst "sub"  [src; dst]
    | IMul (r)        -> inst "mul"  [r]
    | Rawcall (r)     -> Printf.sprintf "call *%s" @@ reg_to_string r
    | _ -> assert false
end

module Builder = struct
  type t =
    IR.t array

  let append (op: IR.t) (b: t): t =
    Array.append b [|op|]

  let append_slice (ops: IR.t array) (b: t): t =
    Array.append b ops
end

let build_call_to (node: Astnode.t) (nargs: int) (b: Builder.t): Builder.t =
  match node with
  | Symbol (s) -> begin
    match s with
    | "*"     -> Builder.append IMul b
    | "+"     -> Builder.append IAdd b
    | "debug" -> Builder.append Debug b
    | _ -> assert false
  end
  | _ -> assert false

let rec build_value (node: Astnode.t) (b: Builder.t): Builder.t =
  match node with
  | Symbol (_) -> assert false
  | Integer (n) -> Builder.append (IPush n) b
  | Group (xs) ->
    match xs with
    | [] -> assert false
    | head :: tail ->
      List.fold_left (Fun.flip build_value) b tail
      |> build_call_to head (List.length tail)

let compile (b: Builder.t): ASM.t array =
  let compile_op (op: IR.t): ASM.t array =
    match op with
    | IPush (n) -> [|IPush (n)|]
    | IMul -> [|
      Pop (Rax);
      Pop (Rbx);
      IMul (Rbx);
      Push (Rax);
    |]
    | IAdd -> [|
      Pop (Rax);
      Pop (Rbx);
      IAdd (Rbx, Rax);
      Push (Rax);
    |]
    | Debug -> [|
      LPush ("__kat__debug");
      Pop (Rax);
      Rawcall (Rax);
    |]
    | _ -> assert false
  in
  Array.map compile_op b
  |> Array.to_list
  |> Array.concat

let read_asm_file filename =
  Util.read_file filename
  |> String.split_on_char '\n'
  |> Array.of_list

let header = read_asm_file "templates/header.S"
let footer = read_asm_file "templates/footer.S"

let generate (target: string) (ast: Astnode.t): unit =
  let built = build_value ast [||] in
  let asm = Array.map ASM.to_string @@ compile built in
  let source =
    Array.concat [header; asm; footer]
    |> Array.to_list
    |> String.concat "\n"
  in
  let chan = open_out target in
  Printf.fprintf chan "%s\n" source;
  close_out chan;
