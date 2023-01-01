open Brr
open Tensor_playground
open Isometric

let render_tensor_type = Frontend_util.fmt_txt "%a" Tensor_type.pp

let () =
  (* Broadcast_explanation.explain (Document.body G.document) "1, 2" "2, 3, 2" *)
  (* Vector_matrix_explanation.explain (Document.body G.document) "3" "3, 4" *)
  Contraction_explanation.explain (Document.body G.document) "3" "3, 4"
    "i, i j -> j"

let isometric () =
  let tensor_type = Tensor_type.[ Elem.Variable "a"; Elem.Concrete 1 ] in
  let container = El.div [] in
  let canvas =
    Isometric_canvas.(
      let opts =
        opts ~container ~background_color:"#ccc" ~scale:10. ~height:320
          ~width:500 ()
      in
      create ~opts ())
  in

  let fill_color i j =
    match (i, j) with
    | 0, 0 -> Some "#000"
    | 0, 9 -> Some "#a00"
    | 9, 0 -> Some "#0a0"
    | 9, 9 -> Some "#00a"
    | _ -> None
  in

  (* top *)
  for i = 0 to 9 do
    for j = 0 to 9 do
      let fill_color = fill_color i j in
      let opts =
        Isometric_rectangle.opts ~height:1 ~width:1 ~top:10 ~right:i ~left:j
          ?fill_color ~plane_view:Plane_view.top ()
      in
      Isometric_canvas.add_child canvas (Isometric_rectangle.create ~opts ())
    done
  done;

  (* front *)
  for i = 0 to 9 do
    for j = 0 to 9 do
      let fill_color = fill_color i j in
      let opts =
        Isometric_rectangle.opts ~height:1 ~width:1 ~top:i ~right:10 ~left:j
          ?fill_color ~plane_view:Plane_view.front ()
      in
      Isometric_canvas.add_child canvas (Isometric_rectangle.create ~opts ())
    done
  done;

  (* side *)
  for i = 0 to 9 do
    for j = 0 to 9 do
      let fill_color = fill_color i j in
      let opts =
        Isometric_rectangle.opts ~height:1 ~width:1 ~top:i ~right:j ?fill_color
          ~left:10 ~plane_view:Plane_view.side ()
      in
      Isometric_canvas.add_child canvas (Isometric_rectangle.create ~opts ())
    done
  done;

  let content = El.[ div [ render_tensor_type tensor_type ]; container ] in
  El.set_children (Document.body G.document) content
