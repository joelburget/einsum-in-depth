open Tensor_playground
module String_set = Set.Make (String)

let default_height = 160
let default_width = 200
let fill_class_name = "fill-slate-500"
let isometric () = Jv.get Jv.global "isometric"
let classes = Frontend_util.classes
let duration = Jv.of_float 2.
let array_max arr = Array.fold_left max min_float arr

let get_class_name edge_attributes label =
  let Colors.{ fill_classes; _ } = Hashtbl.find edge_attributes label in
  fill_classes

module EdgeAnimation = struct
  type edge = Height | Width
  type direction = Expand | Contract
  type t = edge * direction
end

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

  module Animate_dimensions : sig
    type t = {
      width : float;
      height : float;
      top : float;
      left : float;
      right : float;
    }

    (* val pp : t Fmt.t *)
    (* val to_jv : t -> Jv.t *)
  end

  include Jv.CONV with type t := Jv.t

  type opts = Jv.t

  val opts :
    ?top:float ->
    ?left:float ->
    ?right:float ->
    ?plane_view:Plane_view.t ->
    ?class_name:string ->
    width:float ->
    height:float ->
    unit ->
    opts

  val create : opts:opts -> Animate_dimensions.t array -> unit -> t
end = struct
  type t = Jv.t

  module Animate_dimensions = struct
    type t = {
      width : float;
      height : float;
      top : float;
      left : float;
      right : float;
    }

    let pp =
      Fmt.(
        braces
          (record ~sep:semi
             [
               field "width" (fun t -> t.width) float;
               field "height" (fun t -> t.height) float;
               field "top" (fun t -> t.top) float;
               field "left" (fun t -> t.left) float;
               field "right" (fun t -> t.right) float;
             ]))

    let to_jv { width; height; top; left; right } =
      Jv.obj
        [|
          ("width", Jv.of_float width);
          ("height", Jv.of_float height);
          ("top", Jv.of_float top);
          ("left", Jv.of_float left);
          ("right", Jv.of_float right);
        |]
  end

  include (Jv.Id : Jv.CONV with type t := t)

  type opts = Jv.t

  let opts ?top ?left ?right ?plane_view ?class_name ~width ~height () =
    let o = Jv.obj [||] in
    Jv.Float.set_if_some o "top" top;
    Jv.Float.set_if_some o "left" left;
    Jv.Float.set_if_some o "right" right;
    Jv.Jstr.set_if_some o "planeView" plane_view;
    Jv.set_if_some o "className" (Option.map Jv.of_string class_name);
    Jv.Float.set o "width" width;
    Jv.Float.set o "height" height;
    o

  let jobj () = Jv.get (isometric ()) "IsometricRectangle"

  let animate_dimensions values face =
    if Array.length values = 1 then face
    else (
      Fmt.pr "@[animate_dimensions: [@[%a@]]@]@."
        Fmt.(list ~sep:semi Animate_dimensions.pp)
        (Array.to_list values);
      Jv.call face "animateDimensions"
        [| Jv.of_array Animate_dimensions.to_jv values; duration |])

  let add_anim side_name values face =
    let anim_opts =
      Jv.obj
        [|
          ("property", Jv.of_string side_name);
          ("duration", duration);
          ("values", Jv.of_array Jv.of_float values);
        |]
    in
    Jv.call face "addAnimation" [| anim_opts |]

  let create ~opts values () =
    Fmt.(
      pr "@[Rectangle.create [%a]@]@."
        (array ~sep:semi Animate_dimensions.pp)
        values);
    Jv.new' (jobj ()) [| opts |]
    |> add_anim "top" (Array.map (fun x -> x.Animate_dimensions.top) values)
    |> animate_dimensions values
end

module Point : sig
  type t = { left : float; right : float; top : float }

  val zero : t
  (* val negate : t -> t *)
end = struct
  type t = { left : float; right : float; top : float }

  let zero = { left = 0.; right = 0.; top = 0. }

  (* let negate { left; right; top } = *)
  (*   { left = -.left; right = -.right; top = -.top } *)
end

module Path : sig
  type t = Jv.t

  val create : ?stroke_color:string -> Point.t -> Point.t -> t
end = struct
  open Point

  type t = Jv.t

  let jobj () = Jv.get (isometric ()) "IsometricPath"

  let create ?(stroke_color = "red") path_start path_end =
    let opts = Jv.obj [| ("strokeColor", Jv.of_string stroke_color) |] in
    let piece = Jv.new' (jobj ()) [| opts |] in
    (* Fmt.( *)
    (*   pr "Path.create: %f, %f, %f -> %f, %f, %f@." path_start.top *)
    (*     path_start.left path_start.right path_end.top path_end.left *)
    (*     path_end.right); *)
    let start =
      Fmt.str "M%f %f %f L%f %f %f" path_start.left path_start.right
        path_start.top path_start.left path_start.right path_start.top
    in
    let finish =
      Fmt.str "M%f %f %f L%f %f %f" path_start.left path_start.right
        path_start.top path_end.left path_end.right path_end.top
    in
    let anim_opts =
      Jv.obj
        [|
          ("property", Jv.of_string "path");
          ("duration", duration);
          ("values", Jv.of_array Jv.of_string [| start; finish |]);
        |]
    in
    let _ = Jv.call piece "addAnimation" [| anim_opts |] in
    piece
end

module Text : sig
  type t = Jv.t

  include Jv.CONV with type t := t

  type opts = Jv.t

  val opts :
    ?font_size:float ->
    ?class_name:string ->
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

  let opts ?(font_size = 18.) ?(class_name = "fill-slate-500")
      ?(stroke_width = 0.) ?top ?left ?right ?plane_view text =
    let o = Jv.obj [||] in
    Jv.Float.set o "fontSize" font_size;
    Jv.Jstr.set o "fontWeight" (Jstr.v "500");
    Jv.Jstr.set o "className" (Jstr.v class_name);
    Jv.Float.set o "strokeWidth" stroke_width;
    Jv.Float.set_if_some o "top" top;
    Jv.Float.set_if_some o "left" left;
    Jv.Float.set_if_some o "right" right;
    Jv.Jstr.set_if_some o "planeView" plane_view;
    Jv.Jstr.set o "text" (Jstr.v text);
    o

  let jobj () = Jv.get (isometric ()) "IsometricText"
  let create ?(opts = Jv.undefined) () = Jv.new' (jobj ()) [| opts |]
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

  let jobj () = Jv.get (isometric ()) "IsometricGroup"

  let create ?(top = 0.) ?(left = 0.) ?(right = 0.) children =
    let opts = Jv.obj [||] in
    Jv.Float.set opts "top" top;
    Jv.Float.set opts "left" left;
    Jv.Float.set opts "right" right;
    let group = Jv.new' (jobj ()) [| opts |] in
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

  let jobj () = Jv.get (isometric ()) "IsometricCanvas"
  let create ?(opts = Jv.undefined) () = Jv.new' (jobj ()) [| opts |]
  let add_child o child = ignore @@ Jv.call o "addChild" [| child |]
end

(* let filter_map f lst = *)
(*   List.fold_left *)
(*     (fun acc x -> match f x with Some x -> x :: acc | None -> acc) *)
(*     [] lst *)

module Cube : sig
  type t = Jv.t

  val create :
    height:string * float array ->
    width:string * float array ->
    depth:string * float array ->
    draw_diag:bool ->
    edge_attributes:Colors.edge_attributes ->
    class_name:string ->
    unit ->
    t
end = struct
  type t = Jv.t

  let create ~height:(height_label, height) ~width:(width_label, width)
      ~depth:(depth_label, depth) ~draw_diag ~edge_attributes ~class_name () =
    let max_height = array_max height in
    let max_width = array_max width in
    let max_depth = array_max depth in
    let faces =
      Rectangle.
        [
          create
            (Array.map2
               (fun w_as_h d_as_w ->
                 Animate_dimensions.
                   {
                     height = w_as_h;
                     width = d_as_w;
                     top = 0.;
                     left = 0.;
                     right = 0.;
                   })
               width depth)
            ~opts:
              (opts ~top:max_height ~class_name ~plane_view:Plane_view.top
                 ~width:max_depth ~height:max_width ())
            ();
          create
            (Array.map2
               (fun height width ->
                 Animate_dimensions.
                   { height; width; top = 0.; left = 0.; right = 0. })
               height width)
            ~opts:
              (opts ~right:max_depth ~class_name ~plane_view:Plane_view.front
                 ~width:max_width ~height:max_height ())
            ();
          create
            (Array.map2
               (fun height d_as_w ->
                 Animate_dimensions.
                   { height; width = d_as_w; top = 0.; left = 0.; right = 0. })
               height depth)
            ~opts:
              (opts ~left:max_width ~class_name ~plane_view:Plane_view.side
                 ~width:max_depth ~height:max_height ())
            ();
        ]
    in
    let get_class_name = get_class_name edge_attributes in
    let labels =
      [
        Text.create
          ~opts:
            (Text.opts ~top:(max_height *. 1.25) ~left:(max_width *. 0.65)
               ~right:0.
               ~class_name:(get_class_name height_label)
               height_label)
          ();
        Text.create
          ~opts:
            (Text.opts ~top:(max_height *. 1.25) ~left:0.
               ~right:(max_depth *. 0.5)
               ~class_name:(get_class_name depth_label)
               depth_label)
          ();
        Text.create
          ~opts:
            (Text.opts ~top:(max_height *. 0.5) ~left:(max_width *. 1.2)
               ~right:0.
               ~class_name:(get_class_name width_label)
               width_label)
          ();
      ]
    in
    (* Fmt.( *)
    (*   pr "Cube.create max_height: %f, max_width: %f, max_depth: %f@." max_height *)
    (*     max_width max_depth); *)
    let paths =
      if draw_diag then
        if height_label = width_label && width_label = depth_label then
          [
            Path.create
              Point.{ left = 0.; right = max_width; top = max_height }
              Point.{ left = max_depth; right = 0.; top = 0. };
          ] (* TODO: really these three should animate planes *)
        else if height_label = width_label then
          [
            Path.create
              Point.{ left = 0.; right = max_width; top = max_height }
              Point.{ left = max_depth; right = 0.; top = max_height };
          ]
        else if height_label = depth_label then
          [
            Path.create
              Point.{ left = max_depth; right = max_width; top = 0. }
              Point.{ left = max_depth; right = 0.; top = max_height };
          ]
        else if width_label = depth_label then
          [
            Path.create
              Point.{ left = 0.; right = max_width; top = 0. }
              Point.{ left = max_depth; right = max_width; top = max_height };
          ]
        else []
      else []
    in
    Group.create (faces @ labels @ paths)
end

module Tensor : sig
  type t = Jv.t

  val create :
    draw_diag:bool ->
    edge_attributes:Colors.edge_attributes ->
    (string * float array) list ->
    t
end = struct
  type t = Jv.t

  let create ~draw_diag ~edge_attributes =
    let get_class_name = get_class_name edge_attributes in
    function
    | [ (dim1, height) ] ->
        let children =
          [
            Rectangle.create
              ~opts:
                (Rectangle.opts ~plane_view:Plane_view.top
                   ~class_name:fill_class_name ~width:0.
                   ~height:(array_max height) ())
              (Array.map
                 (fun height ->
                   Rectangle.Animate_dimensions.
                     { height; width = 0.; top = 0.; left = 0.; right = 0. })
                 height)
              ();
            (* Path *)
            Text.create
              ~opts:
                (Text.opts
                   ~left:(array_max height *. 0.5)
                   ~right:(-1.4) ~class_name:(get_class_name dim1) dim1)
              ();
          ]
        in
        Group.create children
    | [ (dim1, height); (dim2, width) ] ->
        Fmt.(
          pr "@[Tensor.create rectangle (%s, [%a]) (%s, [%a])@]@." dim1
            (array ~sep:semi float) height dim2 (array ~sep:semi float) width);
        let children =
          [
            Rectangle.create
              ~opts:
                (Rectangle.opts ~plane_view:Plane_view.top
                   ~class_name:fill_class_name ~width:(array_max width)
                   ~height:(array_max height) ())
              (Array.map2
                 (fun height width ->
                   Rectangle.Animate_dimensions.
                     { height; width; top = 0.; left = 0.; right = 0. })
                 height width)
              ();
            Text.create
              ~opts:
                (Text.opts
                   ~left:(array_max height *. 0.5)
                   ~right:(array_max width *. -0.5)
                   ~class_name:(get_class_name dim1) dim1)
              ();
            Text.create
              ~opts:
                (Text.opts
                   ~left:(array_max height *. -0.5)
                   ~right:(array_max width *. 0.5)
                   ~class_name:(get_class_name dim2) dim2)
              ();
          ]
        in
        let paths =
          (* Fmt.( *)
          (*   pr "Tensor.create left: %f, right: %f@." (array_max height) *)
          (*     (array_max width)); *)
          if draw_diag && dim1 = dim2 then
            [
              Path.create Point.zero
                Point.
                  { left = array_max height; right = array_max width; top = 0. };
            ]
          else []
        in
        Group.create (children @ paths)
    | [ height; width; depth ] ->
        Cube.create ~height ~width ~depth ~draw_diag ~edge_attributes
          ~class_name:fill_class_name ()
    | [] -> Text.create ~opts:(Text.opts "(scalar)") ()
    | invalid ->
        failwith
          Fmt.(
            str "Invalid tensor: [@[%a@]]@."
              (list ~sep:semi
                 (parens
                    (pair ~sep:comma string (brackets (array ~sep:semi float)))))
              invalid)
end

module Scene : sig
  val render :
    ?scale:float ->
    ?height:int ->
    ?width:int ->
    edge_attributes:Colors.edge_attributes ->
    string list list ->
    string list ->
    Brr.El.t option
end = struct
  let render_unsafe ?(scale = 10.) ?(height = default_height)
      ?(width = default_width) ~edge_attributes lhs rhs =
    let get_length label = (Hashtbl.find edge_attributes label).Colors.length in
    let get_classes edge_name = get_class_name edge_attributes edge_name in
    let rows = Queue.create () in
    let div, span, txt' = Brr.El.(div, span, txt') in
    let Einops.Steps.{ diagonalized; broadcast } =
      Einops.Steps.make (lhs, rhs)
    in

    (* Fmt.pr "@[lhs: %a,@ rhs: %a@]@." *)
    (*   Fmt.(brackets (list (brackets (list string)))) *)
    (*   lhs *)
    (*   Fmt.(brackets (list string)) *)
    (*   rhs; *)
    (* Fmt.pr "@[broadcast: %a,@ diagonalized: %a@]@." *)
    (*   Fmt.(brackets (list string)) *)
    (*   broadcast *)
    (*   Fmt.(brackets (list (brackets (list string)))) *)
    (*   diagonalized; *)
    let mk_canvas () =
      let container = div [] in
      let canvas =
        Canvas.create
          ~opts:
            (Canvas.opts ~background_color:"rgba(0, 0, 0, 0)" ~container ~scale
               ~height ~width ())
          ()
      in
      (canvas, container)
    in

    Queue.add (div [ txt' "Start" ]) rows;
    let row =
      List.map
        (fun tensor ->
          let canvas, container = mk_canvas () in
          let tensor =
            Tensor.create ~draw_diag:false ~edge_attributes
              (List.map
                 (fun label ->
                   (label, [| get_length label; get_length label |]))
                 tensor)
          in
          Canvas.add_child canvas tensor;
          container)
        lhs
    in
    Queue.add (div ~at:(classes "flex flex-row") row) rows;

    let diagonals_exist =
      List.exists2
        (fun tensor diagonalized ->
          not (List.equal String.equal tensor diagonalized))
        lhs diagonalized
    in
    let elem =
      if diagonals_exist then (
        Queue.add (div [ txt' "Diagonalize" ]) rows;
        let row =
          List.map
            (fun tensor ->
              let canvas, container = mk_canvas () in
              let tensor =
                Tensor.create ~draw_diag:true ~edge_attributes
                  (List.map
                     (fun label ->
                       (label, [| get_length label; get_length label |]))
                     tensor)
              in
              Canvas.add_child canvas tensor;
              container)
            lhs
        in
        div ~at:(classes "flex flex-row") row)
      else div ~at:(classes "flex flex-row") [ (* txt' "(no diagonals)" *) ]
    in
    Queue.add elem rows;

    let reorder_necessary, reordered =
      List.fold_right
        (fun tensor (reorder_necessary, acc) ->
          let reordered_tensor =
            List.sort
              (fun a b ->
                match Einops.Index_helpers.(indexof a rhs, indexof b rhs) with
                | Some i, Some j -> compare i j
                | Some _, None -> -1
                | None, Some _ -> 1
                | None, None -> 0)
              tensor
          in
          if reordered_tensor = tensor then
            (reorder_necessary, reordered_tensor :: acc)
          else (true, reordered_tensor :: acc))
        diagonalized (false, [])
    in

    let tensors_match_shape =
      List.(for_all (equal String.equal broadcast) reordered)
    in
    let broadcast_necessary = not tensors_match_shape in
    let elem =
      if reorder_necessary || broadcast_necessary then (
        let msg =
          match (reorder_necessary, broadcast_necessary) with
          | true, true -> "Reorder and Broadcast"
          | true, false -> "Reorder"
          | false, true -> "Broadcast"
          | false, false -> assert false
        in
        Queue.add (div [ txt' msg ]) rows;
        let row =
          List.map
            (fun diag_tensor ->
              let canvas, container = mk_canvas () in
              let animate_edges =
                List.filter (fun x -> not (List.mem x diag_tensor)) broadcast
              in
              (* Fmt.( *)
              (*   pr *)
              (* "@[diag_tensor: [%a], broadcast: [%a], animate_edges: \ *)
                 (*      [%a]@]@." *)
              (*     (list string) diag_tensor (list string) broadcast *)
              (*     (list string) animate_edges); *)
              let axis_spec =
                List.map
                  (fun label ->
                    let base_length = get_length label in
                    let lengths =
                      if List.mem label animate_edges then [| 0.; base_length |]
                      else [| base_length; base_length |]
                    in
                    (label, lengths))
                  broadcast
              in
              (* Fmt.( *)
              (*   pr "broadcast axis_spec: %a@." *)
              (*     (list ~sep:semi *)
              (*        (parens *)
              (*           (pair ~sep:comma string *)
              (*              (brackets (array ~sep:semi float))))) *)
              (*     axis_spec); *)
              let tensor =
                Tensor.create ~draw_diag:false ~edge_attributes axis_spec
              in
              Canvas.add_child canvas tensor;
              container)
            reordered
        in
        div ~at:(classes "flex flex-row") row)
      else
        div ~at:(classes "flex flex-row")
          [ (* txt' "(no reordering / broadcasting)" *) ]
    in
    Queue.add elem rows;

    let elem =
      match reordered with
      | [ _ ] -> div [ (* txt' "(only one tensor, no pointwise multiply)" *) ]
      | _ ->
          Queue.add (div [ txt' "Pointwise Multiply" ]) rows;
          let canvas, container = mk_canvas () in
          let tensor =
            Tensor.create ~draw_diag:false ~edge_attributes
              (List.map
                 (fun label ->
                   (label, [| get_length label; get_length label |]))
                 broadcast)
          in
          Canvas.add_child canvas tensor;
          div [ container ]
    in
    Queue.add elem rows;

    let need_contraction = not (List.equal String.equal broadcast rhs) in
    let elem =
      if need_contraction then (
        let axes_to_contract =
          String_set.(diff (of_list broadcast) (of_list rhs) |> to_list)
        in
        Queue.add
          (div
             [
               txt' "Contract ";
               span (Frontend_util.list_variables get_classes axes_to_contract);
             ])
          rows;
        let canvas, container = mk_canvas () in
        let axis_spec =
          List.map
            (fun label ->
              let base_length = get_length label in
              let lengths =
                if List.mem label axes_to_contract then [| base_length; 0. |]
                else [| base_length; base_length |]
              in
              (label, lengths))
            broadcast
        in
        (* Fmt.( *)
        (*   pr "contraction axis_spec: %a@." *)
        (*     (list ~sep:semi *)
        (*        (parens *)
        (*           (pair ~sep:comma string (brackets (array ~sep:semi float))))) *)
        (*     axis_spec); *)
        let tensor =
          Tensor.create ~draw_diag:false ~edge_attributes axis_spec
        in

        Canvas.add_child canvas tensor;
        div [ container ])
      else div [ (* txt' "(no contraction)" *) ]
    in
    Queue.add elem rows;

    div ~at:(classes "flex flex-col") (rows |> Queue.to_seq |> List.of_seq)

  let render ?(scale = 10.) ?(height = default_height) ?(width = default_width)
      ~edge_attributes lhs rhs =
    try Some (render_unsafe ~scale ~height ~width ~edge_attributes lhs rhs)
    with e ->
      Fmt.(pf stderr "@[render: %a@]@." exn e);
      None
end
