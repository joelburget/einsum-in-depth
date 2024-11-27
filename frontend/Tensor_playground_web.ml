open Brr

let () =
  (* Broadcast_explanation.explain (Document.body G.document) "1, 2" "2, 3, 2" *)
  (* Vector_matrix_explanation.explain (Document.body G.document) "3" "3, 4" *)
  (* Contraction_explanation.explain (Document.body G.document) "2, 3" "3, 4" *)
  (*   "i j, j k -> i k" *)
  Einsum_explanation.explain
    (Option.get (Document.find_el_by_id G.document (Jstr.of_string "render")))
    "a i j, a j k, a i k ->" ""
