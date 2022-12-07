(* parser =================================================================== *)

module AstNode = struct
  (* TODO literals vs. idents *)
  type t =
  | Symbol of string
  | Integer of int
  | Group of t list

  let rec to_string = function
  | Symbol (s) -> s
  | Integer (n) -> (Int.to_string n)
  | Group (children) ->
    let inner = String.concat " " (List.map to_string children) in
    "(" ^ inner ^ ")"
end

module CharSet = Set.Make (Char)

let ascii_range start stop =
  let start_code = Char.code start in
  let stop_code = Char.code stop in
  assert (start_code <= stop_code);
  let chr x = Char.chr (x + start_code) in
  List.init (stop_code + 1 - start_code) chr

let parse_char_list name chars =
  let char_parser c = Parser.exact (String.init 1 (Fun.const c)) in
  let parsers = List.map (fun c -> lazy (char_parser c)) chars in
  Parser.one_of name parsers

let parse_digit =
  parse_char_list "digit" (ascii_range '0' '9')

(*
let parse_alpha =
  let alphabet = List.init 26 (fun x -> Char.chr (x + (Char.code 'a'))) in
  let char_parser c = Parser.exact (String.init 1 (fun _ -> c)) in
  let alpha_parsers = List.map (fun c -> lazy (char_parser c)) alphabet in
  Parser.one_of "alphabetical" alpha_parsers
*)

let parse_int =
  let parse_int_aux = Parser.repeating parse_digit in
  let convert_int ds = AstNode.Integer (int_of_string (String.concat "" ds)) in
  let parser = Parser.map convert_int parse_int_aux in
  Parser.rename "integer" parser

let parse_symbol =
  let symbolic =
    let printable = ascii_range '!' '~' in
    let non_symbolic = CharSet.of_seq (String.to_seq "()[]{}") in
    List.filter (fun c -> not @@ CharSet.mem c non_symbolic) printable
  in
  let parser = Parser.repeating (parse_char_list "symbolic" symbolic) in
  parser
  |> Parser.map (fun xs -> AstNode.Symbol (String.concat "" xs))
  |> Parser.rename "symbol"

let parse_group parse_expr =
  let left = Parser.exact "(" in
  let right = Parser.(spaces *> exact ")") in
  let expr = Parser.(spaces *> parse_expr) in
  let group_parser = Parser.(rename "group" (left *> many expr <* right)) in
  Parser.map (fun x -> AstNode.Group x) group_parser

(* TODO figure out why the fuck I have to jump through so many hoops to
   get this thing to work *)
let rec parse_expr_aux () =
  let parsers = [
    lazy parse_int;
    lazy parse_symbol;
    lazy (parse_group (parse_expr_aux ()));
  ] in
  Parser.one_of "expression" parsers

let parse_expr = parse_expr_aux ()

let parse (program: string): (AstNode.t, Parser.error) result =
  let result = Parser.parse parse_expr program in
  result

(* compilation cycle ======================================================== *)

let read_file filename =
  let chan = open_in filename in
  really_input_string chan (in_channel_length chan)

let compile filename target =
  Printf.printf "compiling file %s to %s\n" filename target;
  let program = read_file filename in
  match parse program with
  | Error ({msg; pos}) -> Printf.eprintf "error at %d: %s\n" pos msg
  | Ok (ast) ->
    Printf.printf "successfully parsed ast:\n%s\n" (AstNode.to_string ast);
  failwith "TODO compile ast"

(* cli ====================================================================== *)

let () =
  let usage = "kat [OPTIONS] INPUT" in
  let file = ref "" in
  let target = ref "katout.S" in
  let anon input_file = file := input_file in
  let specs =
    [("-o", Arg.Set_string target, "set target (default katout.S)")]
  in
  Arg.parse specs anon usage;
  compile !file !target
