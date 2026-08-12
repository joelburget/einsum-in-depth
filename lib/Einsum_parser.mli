module Friendly : sig
  val parse : string -> (Einops.Rewrite.t, string) result
end

module Original : sig
  val parse : string -> (Einops.Rewrite.t, string) result
end

val parse :
  string ->
  (Einops.Rewrite.t, string) result * (Einops.Rewrite.t, string) result

val prefers_original_notation : string -> bool
(** Whether the input looks like compact NumPy einsum notation: every input
    and output group is a single token, and at least one token contains
    multiple index characters. *)
