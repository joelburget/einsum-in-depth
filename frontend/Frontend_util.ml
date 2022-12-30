open Tensor_playground

let txt_td str = Brr.El.(td [ txt' str ])
let class_ str = Brr.At.(class' (Jstr.of_string str))

let parse_type str =
  try
    let ty, bracketed =
      str |> Lexing.from_string |> Type_parser.lax Type_lexer.token
    in
    Ok (ty, bracketed)
  with Type_lexer.Error msg -> Error msg

let fmt_txt : type a. (a, Format.formatter, unit, Brr.El.t) format4 -> a =
 fun fmt -> Fmt.kstr (fun s -> Brr.El.txt' s) fmt
