module Stream = struct
  type t = {
    str: string;
    pos: int;
    len: int;
  }

  let eof = char_of_int 0

  let from (str: string): t =
    { str; pos = 0; len = String.length str; }

  (** returns next character, or eof at end of stream *)
  let peek (strm: t): char =
    if strm.pos = strm.len then eof else String.get strm.str strm.pos

  (** returns next n characters, or as many as possible up to n *)
  let npeek (n: int) (strm: t): string =
    let len = min n (strm.len - strm.pos) in
    String.sub strm.str strm.pos len

  (** moves the stream up n characters *)
  let ndrop (n: int) (strm: t): t =
    { strm with pos = min strm.len (strm.pos + n) }

  (** moves the stream 1 character *)
  let drop (strm: t): t =
    ndrop 1 strm

(*
  let next (strm: t): t * char =
    drop strm, peek strm

  let nnext (n: int) (strm: t): t * string =
    ndrop n strm, npeek n strm
*)
end

type error = {
  msg: string;
  pos: int;
}

type 'a parser_result =
  (Stream.t * 'a, error) result

type 'a parser = {
  name: string;
  run: Stream.t -> 'a parser_result;
}

(** the canonical full parse cycle *)
let parse (p: 'a parser) (s: string): ('a, error) result =
  match p.run (Stream.from s) with
  | Ok (_, value) -> Ok (value)
  | Error (e) -> Error (e)

let make name run: 'a parser =
  { name; run }

let fail (msg: string) (strm: Stream.t): 'a parser_result =
  Error ({ msg; pos = strm.pos })

let map (f: 'a -> 'b) (p: 'a parser): 'b parser =
  let run' strm = Result.map (fun (strm, x) -> (strm, f x)) (p.run strm) in
  make p.name run'

let map_error (f: error -> error) (p: 'a parser): 'a parser =
  let run' strm = Result.map_error f (p.run strm) in
  make p.name run'

let rename (name: string) (p: 'a parser): 'a parser =
  map_error (fun e -> {e with msg = "expected " ^ name}) {p with name}

let bind (f: 'a -> 'b parser) (p: 'a parser): 'b parser =
  let run' = fun strm ->
    match p.run strm with
    | Ok (strm', data) -> (f data).run strm'
    | Error (e) -> Error (e)
  in
  make p.name run'

(** chain parsers; discarding first result *)
let ( *> ) (p1: 'a parser) (p2: 'b parser): 'b parser =
  let run strm =
    match p1.run strm with
    | Ok (strm', _) -> p2.run strm'
    | Error (e) -> Error (e)
  in
  let name =
    (* this shouldn't ever be used afaik, just for debugging *)
    Printf.sprintf "(%s *> %s)" p1.name p2.name
  in
  make name run

(** chain parsers; parsing both results *)
let ( <*> ) (p1: 'a parser) (p2: 'b parser): ('a * 'b) parser =
  let run strm =
    match p1.run strm with
    | Error (e) -> Error (e)
    | Ok (strm', a) ->
      match p2.run strm' with
      | Error (e) -> Error (e)
      | Ok (strm'', b) -> Ok (strm'', (a, b))
  in
  let name =
    (* this shouldn't ever be used afaik, just for debugging *)
    Printf.sprintf "(%s *> %s)" p1.name p2.name
  in
  make name run

(** chain but discard second result *)
let ( <* ) p1 p2 =
  let run strm =
    match p1.run strm with
    | Ok (strm', data) -> (map (fun _ -> data) p2).run strm'
    | Error (e) -> Error (e)
  in
  let name =
    (* this shouldn't ever be used afaik, just for debugging *)
    Printf.sprintf "(%s <* %s)" p1.name p2.name
  in
  make name run

(** parse as many of a thing as possible *)
let many (p: 'a parser): 'a list parser =
  let rec get_many arr strm: 'a list parser_result =
    match p.run strm with
    | Error (_) -> Ok (strm, Array.to_list arr)
    | Ok (strm', data) ->
      get_many (Array.append arr [|data|]) strm'
  in
  make ("many of " ^ p.name) (get_many [||])

(** match a string *)
let exact (s: string): string parser =
  let len = String.length s in
  let run = fun strm ->
    if s = Stream.npeek len strm then
      let strm' = Stream.ndrop len strm in
      Ok (strm', s)
    else
      let msg = Printf.sprintf "expected '%s'" s in
      fail msg strm
  in
  make s run

let spaces: string parser =
  let run strm =
    let rec get_spaces spaces strm =
      let c = Stream.peek strm in
      match c with
      | ' ' | '\n' | '\t' | '\r' -> get_spaces "" (Stream.drop strm)
      | _ -> Ok (strm, "")
    in
    get_spaces "" strm
  in
  make "spaces" run

let one_of (name: string) (parsers: 'a parser lazy_t list): 'a parser =
  assert ((List.length parsers) > 0);
  let rec first_choice parsers strm: 'a parser_result =
    match parsers with
    | [] -> fail ("expected " ^ name) strm
    | p :: parsers' ->
      match (Lazy.force p).run strm with
      | Ok (strm', data) ->
        Ok (strm', data)
      | _ -> first_choice parsers' strm
  in
  make name (first_choice parsers)

let series_of (name: string) (parsers: 'a parser lazy_t list): 'a list parser =
  assert ((List.length parsers) > 0);
  let rec parse_all arr parsers strm: 'a list parser_result =
    match parsers with
    | [] -> Ok (strm, Array.to_list arr)
    | p :: parsers' ->
      match (Lazy.force p).run strm with
      | Ok (strm', data) ->
        let arr' = Array.append arr [|data|] in
        parse_all arr' parsers' strm'
      | Error (e) -> Error (e)
  in
  make name (parse_all [||] parsers)

(** parse at least one of a thing *)
let repeating (p: 'a parser): 'a list parser =
  let parser = map (fun (x, xs) -> x :: xs) (p <*> many p) in
  rename ("at least one of " ^ p.name) parser