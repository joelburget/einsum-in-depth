module Friendly : sig
  val parse : string -> (Einops.Rewrite.t, string) result
end

module Original : sig
  val parse : string -> (Einops.Rewrite.t, string) result
end

val parse :
  string ->
  (Einops.Rewrite.t, string) result * (Einops.Rewrite.t, string) result
