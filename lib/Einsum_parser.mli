exception Error of string

module Token : sig
  type t = Str of string | Arrow | Comma | Eof

  val pp : Format.formatter -> t -> unit
end

val lex_unsafe : string -> Token.t list
val lex : string -> (Token.t list, string) result
val group_unsafe : Token.t list -> Einops.Group.t * Token.t list
val groups_unsafe : Token.t list -> Einops.Group.t list * Token.t list
val rewrite_unsafe : Token.t list -> Einops.Rewrite.t
val parse_unsafe : string -> Einops.Rewrite.t
val parse : string -> (Einops.Rewrite.t, string) result
