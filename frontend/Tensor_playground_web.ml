open Brr
open Tensor_playground

let render_elem = function
  | Tensor_type.Elem.Concrete i -> El.txt' (Int.to_string i)
  | Variable v -> El.txt' v

let render_tensor_type t = El.txt' (Tensor_type.to_string t)

let () =
  let tensor_type = Tensor_type.[ Elem.Variable "a"; Elem.Concrete 1 ] in
  let content = El.[ div [ render_tensor_type tensor_type ] ] in
  El.set_children (Document.body G.document) content
