module Plane_view : sig
  type t = Jstr.t

  val top : t
  val front : t
  val side : t
end = struct
  type t = Jstr.t

  let top = Jstr.v "TOP"
  let front = Jstr.v "FRONT"
  let side = Jstr.v "SIDE"
end

module Isometric_rectangle : sig
  type t

  include Jv.CONV with type t := t

  type opts

  val opts :
    ?height:int ->
    ?width:int ->
    ?top:int ->
    ?left:int ->
    ?right:int ->
    ?plane_view:Plane_view.t ->
    ?fill_color:string ->
    unit ->
    opts

  val create : ?opts:opts -> unit -> t
end = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  type opts = Jv.t

  let opts ?height ?width ?top ?left ?right ?plane_view ?fill_color () =
    let o = Jv.obj [||] in
    Jv.Int.set_if_some o "height" height;
    Jv.Int.set_if_some o "width" width;
    Jv.Int.set_if_some o "top" top;
    Jv.Int.set_if_some o "left" left;
    Jv.Int.set_if_some o "right" right;
    Jv.Jstr.set_if_some o "planeView" plane_view;
    Jv.set_if_some o "fillColor" (Option.map Jv.of_string fill_color);
    o

  let isometric = Jv.get Jv.global "isometric"
  let jobj = Jv.get isometric "IsometricRectangle"
  let create ?(opts = Jv.undefined) () = Jv.new' jobj [| opts |]
end

module Isometric_canvas : sig
  type t

  include Jv.CONV with type t := t

  type opts

  val opts :
    ?container:Brr.El.t ->
    ?background_color:string ->
    ?scale:float ->
    ?height:int ->
    ?width:int ->
    unit ->
    opts

  val create : ?opts:opts -> unit -> t
  val add_child : t -> Isometric_rectangle.t -> unit
end = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  type opts = Jv.t

  let opts ?container ?background_color ?scale ?height ?width () =
    let o = Jv.obj [||] in
    Jv.set_if_some o "container" (Option.map Brr.El.to_jv container);
    Jv.set_if_some o "backgroundColor"
      (Option.map Jv.of_string background_color);
    Jv.Float.set_if_some o "scale" scale;
    Jv.Int.set_if_some o "height" height;
    Jv.Int.set_if_some o "width" width;
    o

  let isometric = Jv.get Jv.global "isometric"
  let jobj = Jv.get isometric "IsometricCanvas"
  let create ?(opts = Jv.undefined) () = Jv.new' jobj [| opts |]

  let add_child o child =
    ignore @@ Jv.call o "addChild" [| Isometric_rectangle.to_jv child |]
end
