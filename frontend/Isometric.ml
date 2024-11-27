let default_height = 320
let default_width = 500
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

module Isometric_rectangle : sig
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

module Isometric_text : sig
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

  let opts ?(font_size = 15.) ?(fill_color = "#666") ?(stroke_width = 0.) ?top
      ?left ?right ?plane_view text =
    let o = Jv.obj [||] in
    Jv.Float.set o "fontSize" font_size;
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

module Isometric_group : sig
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

  let jobj = Jv.get isometric "IsometricCanvas"
  let create ?(opts = Jv.undefined) () = Jv.new' jobj [| opts |]
  let add_child o child = ignore @@ Jv.call o "addChild" [| child |]
end

module Isometric_cube : sig
  type t = Jv.t
  type labels = string * string * string

  val create : left:float -> labels:labels -> t
end = struct
  type t = Jv.t
  type labels = string * string * string

  let create ~left:left_pos ~labels:(a, b, c) =
    let faces =
      [
        ("top", Plane_view.top);
        ("right", Plane_view.front);
        ("left", Plane_view.side);
      ]
      |> List.map (fun (side_name, plane_view) ->
             let face_opts =
               let o =
                 Isometric_rectangle.opts ~height:1. ~width:1. ~plane_view ()
               in
               Jv.Int.set o side_name 1;
               o
             in
             Isometric_rectangle.create ~opts:face_opts ())
    in
    let texts =
      [ a; b; c ]
      |> List.mapi (fun i label ->
             let top, left, right =
               match i with
               | 0 -> (1.25, 0.65, 0.)
               | 1 -> (1.75, 0.5, 1.15)
               | 2 -> (1., 1.75, 0.5)
               | _ -> assert false
             in

             Isometric_text.create
               ~opts:(Isometric_text.opts ~top ~left ~right label)
               ())
    in
    Isometric_group.create ~left:left_pos ~top:(left_pos /. 2.) (faces @ texts)
end

module Isometric_tensor : sig
  type t = Jv.t

  val create : left:float -> string list -> t
end = struct
  type t = Jv.t

  let create ~left = function
    | [ dim1 ] ->
        let children =
          [
            Isometric_rectangle.create
              ~opts:
                (Isometric_rectangle.opts ~height:1. ~width:0. ~top:0. ~right:0.
                   ~left:0. ~plane_view:Plane_view.top ())
              ();
            Isometric_text.create
              ~opts:(Isometric_text.opts ~top:0.75 ~left:1.25 ~right:0.5 dim1)
              ();
          ]
        in
        Isometric_group.create ~left ~top:(left /. 2.) children
    | [ dim1; dim2 ] ->
        let children =
          [
            Isometric_rectangle.create
              ~opts:
                (Isometric_rectangle.opts ~height:1. ~width:1. ~top:0. ~right:0.
                   ~left:0. ~plane_view:Plane_view.top ())
              ();
            Isometric_text.create
              ~opts:(Isometric_text.opts ~top:0.75 ~left:1.25 ~right:0.5 dim1)
              ();
            Isometric_text.create
              ~opts:(Isometric_text.opts ~top:0.75 ~left:0.5 ~right:1.25 dim2)
              ();
          ]
        in
        Isometric_group.create ~left ~top:(left /. 2.) children
    | [ a; b; c ] -> Isometric_cube.create ~left ~labels:(a, b, c)
    | [] ->
        Isometric_text.create
          ~opts:(Isometric_text.opts ~left ~top:(left /. 2.) "(scalar)")
          ()
    | invalid ->
        failwith (Fmt.str "Invalid tensor: %a" Fmt.(list string) invalid)
end

module Isometric_scene : sig
  val render :
    ?scale:float -> ?height:int -> ?width:int -> string list list -> Brr.El.t
end = struct
  let render ?scale ?(height = default_height) ?(width = default_width) tensors
      =
    let container = Brr.El.div [] in
    let canvas =
      Isometric_canvas.create
        ~opts:
          (Isometric_canvas.opts ~container ~background_color:"#ccc" ?scale
             ~height ~width ())
        ()
    in

    let n_tensors = List.length tensors in
    let segment_width = 3. in
    let array_midpoint = Float.of_int (n_tensors - 1) /. 2. in

    Fmt.pr "Tensors: %a@."
      Fmt.(brackets (list (brackets (list string))))
      tensors;
    tensors
    |> List.iteri (fun i tensor ->
           let left_pos = segment_width *. (array_midpoint -. Float.of_int i) in
           Isometric_canvas.add_child canvas
             (Isometric_tensor.create ~left:left_pos tensor));
    container
end
