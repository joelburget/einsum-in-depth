type t =
  | Variable_variable of string * string
  | Variable_concrete of string * int

let pp ppf =
  let open Fmt in
  function
  | Variable_variable (x, y) ->
      pf ppf "(Variable_variable %a)"
        (parens (pair ~sep:comma string string))
        (x, y)
  | Variable_concrete (x, y) ->
      pf ppf "(Variable_concrete %a)"
        (parens (pair ~sep:comma string int))
        (x, y)
