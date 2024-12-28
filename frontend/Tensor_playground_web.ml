open Brr

let main () =
  let container =
    Option.get (Document.find_el_by_id G.document (Jstr.of_string "render"))
  in
  Einsum_explanation.tutorial container

let () = Jv.set Jv.global "tensor_playground_main" (Jv.callback ~arity:1 main)
