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
