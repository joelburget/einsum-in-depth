open Brr

let main () =
  Einsum_explanation.explain
    (Option.get (Document.find_el_by_id G.document (Jstr.of_string "render")))
    "a i j, a j k, a i k ->" ""

let () = Jv.set Jv.global "tensor_playground_main" (Jv.callback ~arity:1 main)
