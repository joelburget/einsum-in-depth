type t =
  | Variable_variable of string * string
  | Variable_concrete of string * int

let to_string = function
  | Variable_variable (x, y) -> "(Variable_variable (" ^ x ^ ", " ^ y ^ "))"
  | Variable_concrete (x, y) ->
      "(Variable_concrete (" ^ x ^ ", " ^ Int.to_string y ^ "))"
