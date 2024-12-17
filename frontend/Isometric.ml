open Tensor_playground
module String_set = Set.Make (String)

let default_height = 320
let default_width = 500
let fill_color = "#ccc"
let isometric () = Jv.get Jv.global "isometric"
let classes = Frontend_util.classes
let duration = Jv.of_float 2.

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

  val create : ?opts:opts -> ?animate_edges:EdgeAnimation.t list -> unit -> t
end = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  type opts = Jv.t

  let opts ?(height = 0.) ?(width = 0.) ?top ?left ?right ?plane_view
      ?fill_color () =
    let o = Jv.obj [||] in
    Jv.Float.set o "height" height;
    Jv.Float.set o "width" width;
    Jv.Float.set_if_some o "top" top;
    Jv.Float.set_if_some o "left" left;
    Jv.Float.set_if_some o "right" right;
    Jv.Jstr.set_if_some o "planeView" plane_view;
    Jv.set_if_some o "fillColor" (Option.map Jv.of_string fill_color);
    o

  let jobj () = Jv.get (isometric ()) "IsometricRectangle"

  let create ?(opts = Jv.undefined) ?(animate_edges = []) () =
    let rect = Jv.new' (jobj ()) [| opts |] in
    List.iter
      (fun (edge, direction) ->
        let height, width =
          Jv.(get rect "height" |> to_float, get rect "width" |> to_float)
        in
        let edge_name, values =
          match (edge, direction) with
          | EdgeAnimation.Height, EdgeAnimation.Expand ->
              ("height", [| 0.1; height |])
          | Height, Contract -> ("height", [| height; 0.1 |])
          | Width, Expand -> ("width", [| 0.1; width |])
          | Width, Contract -> ("width", [| width; 0.1 |])
        in
        Brr.Console.log
          [ "Rectangle.create"; edge_name; Jv.of_array Jv.of_float values ];
        let anim_opts =
          Jv.obj
            [|
              ("property", Jv.of_string edge_name);
              ("duration", duration);
              ("values", Jv.of_array Jv.of_float values);
            |]
        in
        let _ = Jv.call rect "addAnimation" [| anim_opts |] in
        ())
      animate_edges;
    rect
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

  let jobj () = Jv.get (isometric ()) "IsometricText"

  let create ?(opts = Jv.undefined) () =
    Brr.Console.log [ "Text.create opts"; opts ];
    Jv.new' (jobj ()) [| opts |]
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

let filter_map f lst =
  List.fold_left
    (fun acc x -> match f x with Some x -> x :: acc | None -> acc)
    [] lst

module Cube : sig
  type t = Jv.t
  type labels = string * string * string

  val create :
    ?animate_edges:(string * EdgeAnimation.direction) list ->
    draw_diag:bool ->
    edge_attributes:Colors.edge_attributes ->
    left:float ->
    fill_color:string ->
    labels:labels ->
    unit ->
    t
end = struct
  type t = Jv.t
  type labels = string * string * string

  let create ?(animate_edges = []) ~draw_diag ~edge_attributes ~left:left_pos
      ~fill_color ~labels:(a, b, c) () =
    let get_length i =
      let label = match i with 0 -> a | 1 -> b | 2 -> c | _ -> assert false in
      (Hashtbl.find edge_attributes label).Colors.length
    in
    let l_depth, r_depth, height = (get_length 0, get_length 1, get_length 2) in
    let faces =
      Rectangle.
        [
          create
            ~animate_edges:
              (filter_map
                 EdgeAnimation.(
                   fun (name, direction) ->
                     if name = a then Some (Height, direction)
                     else if name = b then Some (Width, direction)
                     else None)
                 animate_edges)
            ~opts:
              (opts ~height:l_depth ~width:r_depth ~top:height ~fill_color
                 ~plane_view:Plane_view.top ())
            ();
          create
            ~animate_edges:
              (filter_map
                 EdgeAnimation.(
                   fun (name, direction) ->
                     if name = a then Some (Width, direction)
                     else if name = c then Some (Height, direction)
                     else None)
                 animate_edges)
            ~opts:
              (opts ~height ~width:l_depth ~right:r_depth ~fill_color
                 ~plane_view:Plane_view.front ())
            ();
          create
            ~animate_edges:
              (filter_map
                 EdgeAnimation.(
                   fun (name, direction) ->
                     if name = b then Some (Width, direction)
                     else if name = c then Some (Height, direction)
                     else None)
                 animate_edges)
            ~opts:
              (opts ~height ~width:r_depth ~left:l_depth ~fill_color
                 ~plane_view:Plane_view.side ())
            ();
        ]
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
    let paths =
      if draw_diag then
        if a = b && b = c then
          [
            Path.create
              Point.{ left = 0.; right = l_depth; top = height }
              Point.{ left = r_depth; right = 0.; top = 0. };
          ] (* TODO: really these three should animate planes *)
        else if a = b then
          [
            Path.create
              Point.{ left = 0.; right = l_depth; top = height }
              Point.{ left = r_depth; right = 0.; top = height };
          ]
        else if a = c then
          [
            Path.create
              Point.{ left = r_depth; right = l_depth; top = 0. }
              Point.{ left = r_depth; right = 0.; top = height };
          ]
        else if b = c then
          [
            Path.create
              Point.{ left = 0.; right = l_depth; top = 0. }
              Point.{ left = r_depth; right = l_depth; top = height };
          ]
        else []
      else []
    in
    Group.create ~left:left_pos ~top:(left_pos /. 2.) (faces @ labels @ paths)
end

module Tensor : sig
  type t = Jv.t

  val create :
    ?animate_edges:(string * EdgeAnimation.direction) list ->
    draw_diag:bool ->
    edge_attributes:Colors.edge_attributes ->
    left:float ->
    string list ->
    t

  val is_valid : string list -> bool
end = struct
  type t = Jv.t

  let is_valid = function
    | [] | [ _ ] | [ _; _ ] | [ _; _; _ ] -> true
    | _ -> false

  let create ?(animate_edges = []) ~draw_diag ~edge_attributes ~left = function
    | [ dim1 ] ->
        let Colors.{ length = l_depth; color } =
          Hashtbl.(find edge_attributes dim1)
        in
        let children =
          [
            Rectangle.create
              ~animate_edges:
                (filter_map
                   (fun (name, direction) ->
                     if name = dim1 then Some (EdgeAnimation.Height, direction)
                     else None)
                   animate_edges)
              ~opts:
                (Rectangle.opts ~height:l_depth ~width:0.
                   ~plane_view:Plane_view.top ~fill_color ())
              ();
            (* Path *)
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
              ~animate_edges:
                (filter_map
                   (fun (name, direction) ->
                     if name = dim1 then Some (EdgeAnimation.Height, direction)
                     else if name = dim2 then Some (Width, direction)
                     else None)
                   animate_edges)
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
        let paths =
          if draw_diag && dim1 = dim2 then
            [
              Path.create Point.zero
                Point.{ left = r_depth; right = l_depth; top = 0. };
            ]
          else []
        in
        Group.create ~left ~top:(left /. 2.) (children @ paths)
    | [ a; b; c ] ->
        Cube.create ~animate_edges ~draw_diag ~edge_attributes ~left ~fill_color
          ~labels:(a, b, c) ()
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
    string list ->
    Brr.El.t
end = struct
  let render ?(scale = 10.) ?(height = default_height) ?(width = default_width)
      ~edge_attributes lhs rhs =
    let rows = Queue.create () in
    let div, txt' = Brr.El.(div, txt') in
    let Einops.Steps.{ diagonalized; broadcast } =
      Einops.Steps.make (lhs, rhs)
    in
    Fmt.pr "@[lhs: %a,@ rhs: %a@]@."
      Fmt.(brackets (list (brackets (list string))))
      lhs
      Fmt.(brackets (list string))
      rhs;
    Fmt.pr "@[broadcast: %a,@ diagonalized: %a@]@."
      Fmt.(brackets (list string))
      broadcast
      Fmt.(brackets (list (brackets (list string))))
      diagonalized;

    let mk_canvas () =
      let container = div [] in
      let canvas =
        Canvas.create ~opts:(Canvas.opts ~container ~scale ~height ~width ()) ()
      in
      (canvas, container)
    in

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
                Tensor.create ~draw_diag:true ~edge_attributes ~left:0. tensor
              in
              Canvas.add_child canvas tensor;
              container)
            lhs
        in
        div ~at:(classes "flex flex-row") row)
      else div ~at:(classes "flex flex-row") [ txt' "(no diagonals)" ]
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

    let elem =
      if reorder_necessary then (
        Queue.add (div [ txt' "Reorder" ]) rows;
        let row =
          List.map
            (fun reordered_tensor ->
              let canvas, container = mk_canvas () in
              let tensor =
                Tensor.create ~draw_diag:false ~edge_attributes ~left:0.
                  reordered_tensor
              in
              Canvas.add_child canvas tensor;
              container)
            reordered
        in
        div ~at:(classes "flex flex-row") row)
      else div ~at:(classes "flex flex-row") [ txt' "(no reorder)" ]
    in
    Queue.add elem rows;

    let tensors_match_shape =
      List.(for_all (equal String.equal broadcast) reordered)
    in
    let broadcast_necessary = not tensors_match_shape in
    let elem =
      if broadcast_necessary then (
        Queue.add (div [ txt' "Broadcast" ]) rows;
        let row =
          List.map
            (fun diag_tensor ->
              let canvas, container = mk_canvas () in
              let animate_edges =
                List.filter (fun x -> not (List.mem x diag_tensor)) broadcast
                |> List.map (fun x -> (x, EdgeAnimation.Expand))
              in
              Fmt.(
                pr "diag_tensor: [%a], broadcast: [%a], animate_edges: [%a]@."
                  (list string) diag_tensor (list string) broadcast
                  (list string)
                  (List.map (fun (x, _) -> x) animate_edges));
              let tensor =
                Tensor.create ~draw_diag:false ~animate_edges ~edge_attributes
                  ~left:0. diag_tensor
              in
              Canvas.add_child canvas tensor;
              container)
            reordered
        in
        div ~at:(classes "flex flex-row") row)
      else div ~at:(classes "flex flex-row") [ txt' "(no diagonals)" ]
    in
    Queue.add elem rows;

    let elem =
      match reordered with
      | [ _ ] -> div [ txt' "(only one tensor, no pointwise multiply)" ]
      | _ ->
          Queue.add (div [ txt' "Pointwise Multiply" ]) rows;
          let canvas, container = mk_canvas () in
          let tensor =
            Tensor.create ~draw_diag:false ~edge_attributes ~left:0. broadcast
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
               txt'
                 Fmt.(
                   str "Contract [@[%a@]]" (list ~sep:comma string)
                     axes_to_contract);
             ])
          rows;
        let canvas, container = mk_canvas () in
        let animate_edges =
          List.map (fun x -> (x, EdgeAnimation.Contract)) axes_to_contract
        in
        let tensor =
          Tensor.create ~draw_diag:false ~animate_edges ~edge_attributes
            ~left:0. broadcast
        in
        Canvas.add_child canvas tensor;
        div [ container ])
      else div [ txt' "(no contraction)" ]
    in
    Queue.add elem rows;

    div ~at:(classes "flex flex-col") (rows |> Queue.to_seq |> List.of_seq)
end
