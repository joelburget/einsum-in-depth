open Tensor_playground.Einops

val draw_unary_contraction : Unary_contraction.t -> Brr.El.t

val draw_binary_contraction :
  string list -> string list -> Binary_contraction.t -> Brr.El.t
