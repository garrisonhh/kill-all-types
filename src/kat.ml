(* parser =================================================================== *)

module Parser = Parser.Make (struct
  type t = char

  let equal = Char.equal
  let to_string = Char.escaped
end)

module CharSet = Set.Make (Char)

let ascii_range start stop =
  let start_code = Char.code start in
  let stop_code = Char.code stop in
  assert (start_code <= stop_code);
  let chr x = Char.chr (x + start_code) in
  List.init (stop_code + 1 - start_code) chr

let char_choice chars = Parser.choice_all @@ List.map Parser.exactly chars
let chars_to_string chars = String.of_seq @@ List.to_seq chars
let digit_parser = char_choice @@ ascii_range '0' '9'

let symbol_parser =
  let printable = ascii_range '!' '~' in
  let non_symbolic = CharSet.of_seq (String.to_seq "()[]{}") in
  let symbols =
    List.filter (fun c -> not @@ CharSet.mem c non_symbolic) printable
  in
  char_choice symbols
  |> Parser.at_least 1
  |> Parser.map chars_to_string
  |> Parser.map (fun s -> Astnode.Symbol s)

let int_parser =
  Parser.at_least 1 digit_parser
  |> Parser.map chars_to_string
  |> Parser.map int_of_string
  |> Parser.map (fun n -> Astnode.Integer n)

let spaces =
  char_choice [ ' '; '\n'; '\r'; '\t' ]
  |> Parser.many
  |> Parser.map chars_to_string

let rec group_parser seq =
  let elem = Parser.chainr spaces expr_parser in
  let parser =
    Parser.(exactly '(' *> many elem <* spaces <* exactly ')')
    |> Parser.map (fun grp -> Astnode.Group grp)
  in
  parser seq

and expr_parser seq =
  let parser = Parser.choice_all [ int_parser; symbol_parser; group_parser ] in
  parser seq

let parse (program : string) : (Astnode.t list, Parser.error) result =
  let program_parser = Parser.(many (spaces *> expr_parser)) in
  let result = Parser.parse (String.to_seq program) program_parser in
  result

(* compilation cycle ======================================================== *)

let compile filename target =
  Printf.printf "compiling file %s to %s\n" filename target;
  let program = Util.read_file filename in
  match parse program with
  | Error { msg; _ } -> Printf.eprintf "error: %s\n" msg
  | Ok nodes ->
      Printf.printf "successfully parsed ast:\n%s\n"
        (String.concat "\n" (List.map Astnode.to_string nodes));
      Codegen.generate target nodes

(* cli ====================================================================== *)

let () =
  let usage = "kat [OPTIONS] INPUT" in
  let file = ref "" in
  let target = ref "katout.S" in
  let anon input_file = file := input_file in
  let specs =
    [ ("-o", Arg.Set_string target, "set target (default katout.S)") ]
  in
  Arg.parse specs anon usage;
  compile !file !target
