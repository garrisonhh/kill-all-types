(** a monadic parser combinator lib *)

module type Token = sig
  type t

  val equal : t -> t -> bool
  val to_string : t -> string
end

module Make (Tok : Token) = struct
  type error = {
    token : Tok.t option; (* None represents end of stream *)
    msg : string;
  }

  type tok_seq = Tok.t Seq.t
  type 'out parse_result = ('out * tok_seq, error) result
  type 'out t = tok_seq -> 'out parse_result

  let make_error token msg : error = { token; msg }

  (* combinators *)

  let wrap (f : tok_seq -> 'out * tok_seq) : 'out t = fun seq -> Ok (f seq)

  let map (f : 'a -> 'b) (p : 'a t) : 'b t =
   fun seq ->
    match p seq with Error e -> Error e | Ok (out, seq') -> Ok (f out, seq')

  let map_error (f : error -> error) (p : 'a t) : 'a t =
   fun seq ->
    match p seq with Ok (x, seq') -> Ok (x, seq') | Error e -> Error (f e)

  let rename (name : string) (p : 'a t) : 'a t =
    let err_msg = Printf.sprintf "expected %s" name in
    map_error (fun e -> { e with msg = err_msg }) p

  let bind (f : 'a -> ('b, error) result) (p : 'a t) : 'b t =
   fun seq ->
    match p seq with
    | Error e -> Error e
    | Ok (out, seq') -> (
        match f out with Error e -> Error e | Ok out' -> Ok (out', seq'))

  (* any number of things that match p *)
  let many (p : 'a t) : 'a list t =
    let rec collect xs seq =
      match p seq with
      | Error _ -> Ok (List.rev xs, seq)
      | Ok (x, seq') -> collect (x :: xs) seq'
    in
    collect []

  (* same as many, but expects some number of elements *)
  let at_least (n : int) (p : 'a t) : 'a list t =
    let aux xs =
      let len = List.length xs in
      if len >= n then Ok xs
      else
        let msg = Printf.sprintf "expected %d elements, found %d" n len in
        Error (make_error (List.nth_opt xs 0) msg)
    in
    bind aux @@ many p

  let chain_all (parsers : 'a t list) : 'a list t =
    let rec aux xs parsers seq =
      match parsers with
      | [] -> Ok (List.rev xs, seq)
      | p :: parsers' -> (
          match p seq with
          | Ok (x, seq') -> aux (x :: xs) parsers' seq'
          | Error e -> Error e)
    in
    aux [] parsers

  let chain (leftp : 'a t) (rightp : 'b t) : ('a * 'b) t =
   fun seq ->
    match leftp seq with
    | Error e -> Error e
    | Ok (outl, seq') -> (
        match rightp seq' with
        | Ok (outr, seq'') -> Ok ((outl, outr), seq'')
        | Error e -> Error e)

  let chainl leftp rightp = map fst @@ chain leftp rightp
  let chainr leftp rightp = map snd @@ chain leftp rightp

  let choice_all (parsers : 'a t list) : 'a t =
    let rec aux parsers seq =
      match parsers with
      | [] ->
          (* this is a terrible error message, but you can augment it with
               rename and positionally aware tokens *)
          Error (make_error None "syntax error")
      | p :: parsers' -> (
          match p seq with
          | Ok (out, seq') -> Ok (out, seq')
          | Error _ -> aux parsers' seq)
    in
    aux parsers

  let choice (leftp : 'a t) (rightp : 'a t) : 'a t =
    choice_all [ leftp; rightp ]

  let ( >>= ) = Fun.flip bind
  let ( <*> ) = chain
  let ( <* ) = chainl
  let ( *> ) = chainr
  let ( <|> ) = choice

  (* useful generic parsers *)

  let any : Tok.t t =
   fun seq ->
    let maybe = Seq.uncons seq in
    let err = make_error None "unexpectedly reached end of file" in
    Option.to_result ~none:err maybe

  let exactly tok : Tok.t t =
    let err_msg = Printf.sprintf "expected %s" @@ Tok.to_string tok in
    let expect_tok found =
      if Tok.equal tok found then Ok tok
      else Error (make_error (Some found) err_msg)
    in
    any >>= expect_tok

  (** pipeline for parser from start to finish *)
  let parse (seq : tok_seq) (p : 'out t) : ('out, error) result =
    match p seq with Ok (out, _) -> Ok out | Error e -> Error e
end
