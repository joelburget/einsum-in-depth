let classes = Frontend_util.classes
let default_height = 320
let default_width = 500
let fill_color = "#ccc"
let isometric = Jv.get Jv.global "isometric"

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

module Rectangle : sig
  type t = Jv.t

  include Jv.CONV with type t := Jv.t

  type opts = Jv.t

  val opts :
    ?height:float ->
    ?width:float ->
    ?top:float ->
    ?left:float ->
    ?right:float ->
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
    Jv.Float.set_if_some o "height" height;
    Jv.Float.set_if_some o "width" width;
    Jv.Float.set_if_some o "top" top;
    Jv.Float.set_if_some o "left" left;
    Jv.Float.set_if_some o "right" right;
    Jv.Jstr.set_if_some o "planeView" plane_view;
    Jv.set_if_some o "fillColor" (Option.map Jv.of_string fill_color);
    o

  let jobj = Jv.get isometric "IsometricRectangle"
  let create ?(opts = Jv.undefined) () = Jv.new' jobj [| opts |]
end

let copy_object obj =
  let global_object = Jv.get Jv.global "Object" in
  Jv.call global_object "assign" [| obj |]

module Text : sig
  type t = Jv.t

  include Jv.CONV with type t := t

  type opts = Jv.t

  val opts :
    ?font_size:float ->
    ?fill_color:string ->
    ?stroke_width:float ->
    ?top:float ->
    ?left:float ->
    ?right:float ->
    ?plane_view:Plane_view.t ->
    string ->
    opts

  val create : ?opts:opts -> unit -> t
end = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  type opts = Jv.t

  let opts ?(font_size = 18.) ?(fill_color = "#666") ?(stroke_width = 0.) ?top
      ?left ?right ?plane_view text =
    let o = Jv.obj [||] in
    Jv.Float.set o "fontSize" font_size;
    Jv.Jstr.set o "fontWeight" (Jstr.v "500");
    Jv.Jstr.set o "fillColor" (Jstr.v fill_color);
    Jv.Float.set o "strokeWidth" stroke_width;
    Jv.Float.set_if_some o "top" top;
    Jv.Float.set_if_some o "left" left;
    Jv.Float.set_if_some o "right" right;
    Jv.Jstr.set_if_some o "planeView" plane_view;
    Jv.Jstr.set o "text" (Jstr.v text);
    o

  let jobj = Jv.get isometric "IsometricText"
  let create ?(opts = Jv.undefined) () = Jv.new' jobj [| opts |]
end

module Group : sig
  type t

  include Jv.CONV with type t := t

  type opts = Jv.t

  val create : ?top:float -> ?left:float -> ?right:float -> Jv.t list -> opts
end = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  type opts = Jv.t

  let jobj = Jv.get isometric "IsometricGroup"

  let create ?(top = 0.) ?(left = 0.) ?(right = 0.) children =
    let opts = Jv.obj [||] in
    Jv.Float.set opts "top" top;
    Jv.Float.set opts "left" left;
    Jv.Float.set opts "right" right;
    let group = Jv.new' jobj [| opts |] in
    let _ = Jv.call group "addChildren" (Array.of_list children) in
    group
end

module Canvas : sig
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
  val add_child : t -> Rectangle.t -> unit
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

  let jobj = Jv.get isometric "IsometricCanvas"
  let create ?(opts = Jv.undefined) () = Jv.new' jobj [| opts |]
  let add_child o child = ignore @@ Jv.call o "addChild" [| child |]
end

module Cube : sig
  type t = Jv.t
  type labels = string * string * string

  val create :
    edge_attributes:Colors.edge_attributes ->
    left:float ->
    fill_color:string ->
    labels:labels ->
    t
end = struct
  type t = Jv.t
  type labels = string * string * string

  let create ~edge_attributes ~left:left_pos ~fill_color ~labels:(a, b, c) =
    let get_length i =
      let label = match i with 0 -> a | 1 -> b | 2 -> c | _ -> assert false in
      (Hashtbl.find edge_attributes label).Colors.length
    in
    let l_depth, r_depth, height = (get_length 0, get_length 1, get_length 2) in
    Fmt.pr "l_depth: %f, r_depth: %f, height: %f@." l_depth r_depth height;
    let faces =
      [ Plane_view.top; Plane_view.front; Plane_view.side ]
      |> List.mapi (fun i plane_view ->
             let face_opts =
               match i with
               | 0 ->
                   Rectangle.opts ~height:l_depth ~width:r_depth ~top:height
                     ~fill_color ~plane_view ()
               | 1 ->
                   Rectangle.opts ~height ~width:l_depth ~right:r_depth
                     ~fill_color ~plane_view ()
               | _ ->
                   Rectangle.opts ~height ~width:r_depth ~left:l_depth
                     ~fill_color ~plane_view ()
             in

             Rectangle.create ~opts:face_opts ())
    in
    let labels =
      [ a; b; c ]
      |> List.mapi (fun i label ->
             let Colors.{ color; length = _ } =
               Hashtbl.find edge_attributes label
             in
             let top, left, right =
               match i with
               | 0 -> (* top left *) (height *. 1.25, l_depth *. 0.65, 0.)
               | 1 ->
                   (* top right *)
                   (height *. 1.25, 0., r_depth *. 0.5)
               | 2 -> (* left *) (height *. 0.5, l_depth *. 1.2, 0.)
               | _ -> assert false
             in
             Text.create
               ~opts:(Text.opts ~top ~left ~right ~fill_color:color label)
               ())
    in
    Group.create ~left:left_pos ~top:(left_pos /. 2.) (faces @ labels)
end

module Tensor : sig
  type t = Jv.t

  val create :
    edge_attributes:Colors.edge_attributes -> left:float -> string list -> t

  val is_valid : string list -> bool
end = struct
  type t = Jv.t

  let is_valid = function
    | [] | [ _ ] | [ _; _ ] | [ _; _; _ ] -> true
    | _ -> false

  let create ~edge_attributes ~left = function
    | [ dim1 ] ->
        let Colors.{ length = l_depth; color } =
          Hashtbl.(find edge_attributes dim1)
        in
        let children =
          [
            Rectangle.create
              ~opts:
                (Rectangle.opts ~height:l_depth ~width:0.
                   ~plane_view:Plane_view.top ~fill_color ())
              ();
            Text.create
              ~opts:
                (Text.opts ~left:(l_depth *. 0.5) ~right:(-1.4)
                   ~fill_color:color dim1)
              ();
          ]
        in
        Group.create ~left ~top:(left /. 2.) children
    | [ dim1; dim2 ] ->
        let Colors.(
              ( { length = l_depth; color = color1 },
                { length = r_depth; color = color2 } )) =
          Hashtbl.(find edge_attributes dim1, find edge_attributes dim2)
        in
        let children =
          [
            Rectangle.create
              ~opts:
                (Rectangle.opts ~height:l_depth ~width:r_depth
                   ~plane_view:Plane_view.top ~fill_color ())
              ();
            Text.create
              ~opts:
                (Text.opts ~left:(l_depth *. 0.5) ~right:(r_depth *. -0.5)
                   ~fill_color:color1 dim1)
              ();
            Text.create
              ~opts:
                (Text.opts ~left:(l_depth *. -0.5) ~right:(r_depth *. 0.5)
                   ~fill_color:color2 dim2)
              ();
          ]
        in
        Group.create ~left ~top:(left /. 2.) children
    | [ a; b; c ] ->
        Cube.create ~edge_attributes ~left ~fill_color ~labels:(a, b, c)
    | [] -> Text.create ~opts:(Text.opts ~left ~top:(left /. 2.) "(scalar)") ()
    | invalid ->
        failwith (Fmt.str "Invalid tensor: %a" Fmt.(list string) invalid)
end

module Scene : sig
  val render :
    ?scale:float ->
    ?height:int ->
    ?width:int ->
    edge_attributes:Colors.edge_attributes ->
    string list list ->
    Brr.El.t
end = struct
  let render ?(scale = 10.) ?(height = default_height) ?(width = default_width)
      ~edge_attributes tensors =
    let container =
      Brr.El.div
        ~at:
          (classes "mx-auto"
          @ Brr.At.
              [
                style
                  (Jstr.v
                     (Fmt.str "height: %dpx; width: %dpx;" default_height
                        default_width));
              ])
        []
    in
    let background_color =
      if Colors.prefers_dark () then "#00000080" else "#fff"
    in
    let canvas =
      Canvas.create
        ~opts:
          (Canvas.opts ~container ~background_color ~scale ~height ~width ())
        ()
    in

    let n_tensors = List.length tensors in
    let segment_width = 20. in
    let array_midpoint = Float.of_int (n_tensors - 1) /. 2. in

    List.iteri
      (fun i tensor ->
        let left_pos = segment_width *. (array_midpoint -. Float.of_int i) in
        Canvas.add_child canvas
          (Tensor.create ~edge_attributes ~left:left_pos tensor))
      tensors;
    container
end
