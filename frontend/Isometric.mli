module EdgeAnimation : sig
  type edge = Height | Width
  type direction = Expand | Contract
  type t = edge * direction
end

module Tensor : sig
  type t = Jv.t

  val is_valid : string list -> bool
end

module Scene : sig
  val render :
    ?scale:float ->
    ?height:int ->
    ?width:int ->
    (* ?animate_edges:(string * EdgeAnimation.direction) list -> *)
    edge_attributes:Colors.edge_attributes ->
    string list list ->
    string list ->
    Brr.El.t
end
