open Brr
open Tensor_playground

let render_tensor_type = Frontend_util.fmt_txt "%a" Tensor_type.pp

let isometric () =
  let tensor_type = Tensor_type.[ Elem.Variable "a"; Elem.Concrete 1 ] in

  (* let iso_scene = Isometric_scene.render ~height:320 ~width:500 *)
  (*   [ [ "a"; "b"; "c" ]; [ "d"; "e" ]; [ "f" ] ] in *)
  let content = El.[ div [ render_tensor_type tensor_type ] ] in
  El.set_children (Document.body G.document) content

let () =
  (* Broadcast_explanation.explain (Document.body G.document) "1, 2" "2, 3, 2" *)
  (* Vector_matrix_explanation.explain (Document.body G.document) "3" "3, 4" *)
  (* Contraction_explanation.explain (Document.body G.document) "2, 3" "3, 4" *)
  (*   "i j, j k -> i k" *)
  Einsum_explanation.explain
    (Option.get (Document.find_el_by_id G.document (Jstr.of_string "render")))
    "a i j, a j k, a i k ->" ""
(* isometric () *)
