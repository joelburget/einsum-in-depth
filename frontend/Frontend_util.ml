open Tensor_playground
open Brr
open Brr_note
open Note

type direction = Horizontal | Vertical

let ( code,
      set_children,
      input,
      prop,
      as_target,
      div,
      txt',
      table,
      tbody,
      td,
      tr,
      p ) =
  El.
    ( code,
      set_children,
      input,
      prop,
      as_target,
      div,
      txt',
      table,
      tbody,
      td,
      tr,
      p )

let txt_td str = td [ txt' str ]
let class_ str = At.(class' (Jstr.of_string str))

let parse_type str =
  match Type_parser_runner.parse Type_parser.Incremental.lax str with
  | Ok (ty, bracketed) -> Ok (ty, bracketed)
  | Error (_location, indication, message) -> Error (indication, message)

let parse_einsum str =
  match Einsum_parser_runner.parse Einsum_parser.Incremental.rewrite str with
  | Ok rewrite -> Ok rewrite
  | Error (_location, indication, message) -> Error (indication, message)

let collect_parse_errors = function
  | Result.Ok _ -> []
  | Error (msg1, msg2) -> [ msg1; msg2 ]

let fmt_txt : type a. (a, Format.formatter, unit, El.t) format4 -> a =
 fun fmt -> Fmt.kstr (fun s -> El.txt' s) fmt

let bracketed_input bracketed_s elem =
  let lbracket = El.span [] in
  let rbracket = El.span [] in
  let f str = function
    | Tensor_type.Unbracketed -> [ El.txt' str ]
    | Bracketed -> []
  in
  Elr.def_children lbracket (S.map (f "[") bracketed_s);
  Elr.def_children rbracket (S.map (f "]") bracketed_s);
  El.(span [ lbracket; elem; rbracket ])

let render_vec ~direction items =
  match direction with
  | Horizontal ->
      tbody [ items |> List.map Int.to_string |> List.map txt_td |> tr ]
  | Vertical ->
      items |> List.map (fun i -> tr [ txt_td (Int.to_string i) ]) |> tbody

let render_mat items =
  items
  |> List.map (fun items -> items |> List.map Int.to_string |> List.map txt_td)
  |> List.map tr |> tbody
