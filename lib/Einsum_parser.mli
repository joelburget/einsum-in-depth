exception Error of string

module Friendly : sig
  val parse : string -> (Einops.Rewrite.t, string) result
end

module Original : sig
  val parse : string -> (Einops.Rewrite.t, string) result
end
