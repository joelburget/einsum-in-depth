open Tensor_playground.Einops

val draw_unary_contraction :
  Colors.edge_attributes -> Unary_contraction.t -> Brr.El.t

val draw_binary_contraction :
  Colors.edge_attributes ->
  string list ->
  string list ->
  Binary_contraction.t ->
  Brr.El.t

val draw_einsum :
  Colors.edge_attributes -> string list list -> string list -> Brr.El.t
