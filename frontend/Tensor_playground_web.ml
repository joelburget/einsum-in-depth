open Brr
open Tensor_playground

let () = El.set_children (Document.body G.document) El.[ txt' "Hello World!" ]
