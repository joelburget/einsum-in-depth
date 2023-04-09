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
      span,
      txt',
      table,
      tbody,
      td,
      tr,
      p,
      ol,
      ul,
      li ) =
  El.
    ( code,
      set_children,
      input,
      prop,
      as_target,
      div,
      span,
      txt',
      table,
      tbody,
      td,
      tr,
      p,
      ol,
      ul,
      li )

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

let parsed_input :
    type a.
    (string -> (a, string * string) result) ->
    string ->
    a option signal * El.t * El.t =
 fun parse start_value ->
  let input_elem = input ~at:[ At.value (Jstr.of_string start_value) ] () in
  let parse_error_elem = div [] in

  let input_signal, set_input = S.create start_value in
  let parse_error_signal, set_parse_error = S.create [] in
  let parsed_signal, set_parsed = S.create None in

  Evr.endless_listen (as_target input_elem) Ev.change (fun _evt ->
      set_input (Jstr.to_string (prop El.Prop.value input_elem)));

  let output_logger =
    S.log input_signal (fun str ->
        match parse str with
        | Result.Ok t ->
            set_parsed (Some t);
            set_parse_error []
        | Error (msg1, msg2) ->
            set_parsed None;
            set_parse_error [ txt' msg1; txt' msg2 ])
  in
  Logr.hold output_logger;

  Elr.def_children parse_error_elem parse_error_signal;
  (parsed_signal, input_elem, parse_error_elem)

let bracketed_parsed_input :
    type a.
    (string -> (a * Tensor_type.bracketed, string * string) result) ->
    string ->
    a option signal * El.t * El.t =
 fun parse start_value ->
  let bracket_signal, set_bracket = S.create Tensor_type.Unbracketed in
  let out_signal, input_elem, err_elem = parsed_input parse start_value in
  let out_signal =
    S.map
      (Option.map (fun (t, bracketed) ->
           set_bracket bracketed;
           t))
      out_signal
  in
  let bracketed_input_elem = bracketed_input bracket_signal input_elem in
  (out_signal, bracketed_input_elem, err_elem)

let l4 :
    type a b c d e.
    (a -> b -> c -> d -> e) ->
    a signal ->
    b signal ->
    c signal ->
    d signal ->
    e signal =
 fun f s0 s1 s2 s3 ->
  S.l2 (fun (a, b) (c, d) -> f a b c d) (S.Pair.v s0 s1) (S.Pair.v s2 s3)
