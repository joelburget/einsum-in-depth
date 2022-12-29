let rec join ?(sep = "") = function
  | [] -> ""
  | [ x ] -> x
  | x :: xs -> x ^ sep ^ join ~sep xs

let replicate n item = List.init n (fun _ -> item)
let pad n x xs = replicate (n - List.length xs) x @ xs
let sum = List.fold_left ( + ) 0

let rec transpose = function
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss ->
      (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)
