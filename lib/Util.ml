let rec join ?(sep = "") = function
  | [] -> ""
  | [ x ] -> x
  | x :: xs -> x ^ ", " ^ join ~sep xs
