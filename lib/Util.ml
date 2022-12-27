let rec join ?(sep = "") = function
  | [] -> ""
  | [ x ] -> x
  | x :: xs -> x ^ sep ^ join ~sep xs
