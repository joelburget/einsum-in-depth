module Tensor : sig
  type t = Jv.t

  val create :
    edge_attributes:Colors.edge_attributes -> left:float -> string list -> t

  val is_valid : string list -> bool
end

module Scene : sig
  val render :
    ?scale:float ->
    ?height:int ->
    ?width:int ->
    edge_attributes:Colors.edge_attributes ->
    string list list ->
    string list ->
    Brr.El.t
end
