module type Token = sig
  type t

  val equal : t -> t -> bool
  val to_string : t -> string
end

module Make : functor (Tok : Token) -> sig
  type error = { token : Tok.t option; msg : string }
  type tok_seq = Tok.t Seq.t
  type 'out parse_result = ('out * tok_seq, error) result
  type 'out t = tok_seq -> 'out parse_result

  val make_error : Tok.t option -> string -> error
  val wrap : (tok_seq -> 'out * tok_seq) -> 'out t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map_error : (error -> error) -> 'a t -> 'a t
  val rename : string -> 'a t -> 'a t
  val bind : ('a -> ('b, error) result) -> 'a t -> 'b t
  val many : 'a t -> 'a list t
  val at_least : int -> Tok.t t -> Tok.t list t
  val chain_all : 'a t list -> 'a list t
  val chain : 'a t -> 'b t -> ('a * 'b) t
  val chainl : 'a t -> 'b t -> 'a t
  val chainr : 'a t -> 'b t -> 'b t
  val choice_all : 'a t list -> 'a t
  val choice : 'a t -> 'a t -> 'a t
  val ( >>= ) : Tok.t t -> (Tok.t -> (Tok.t, error) result) -> Tok.t t
  val ( <*> ) : 'a t -> 'b t -> ('a * 'b) t
  val ( <* ) : 'a t -> 'b t -> 'a t
  val ( *> ) : 'a t -> 'b t -> 'b t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val any : Tok.t t
  val exactly : Tok.t -> Tok.t t
  val parse : tok_seq -> 'out t -> ('out, error) result
end
