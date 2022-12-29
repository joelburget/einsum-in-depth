open Brr
open Brr_note
open Tensor_playground
open Note

let set_children, input, prop, as_target, div, txt', table, tbody, td, tr =
  El.(set_children, input, prop, as_target, div, txt', table, tbody, td, tr)

let process_type_input str =
  str |> Lexing.from_string |> Type_parser.lax Type_lexer.token

let parse str =
  try
    Fmt.pr "attempting to parse %S\n" str;
    let ty, bracketed = process_type_input str in
    Ok (ty, bracketed)
  with
  | Type_lexer.Eof ->
      (* TODO: handle empty case with more explanation or fail *)
      if str = "" then Ok ([], Unbracketed)
      else Error "Unknown error (unclosed bracket?)"
  | Type_lexer.Error msg -> Error msg

let combine : Tensor_type.Elem.t -> Tensor_type.Elem.t -> Tensor_type.Elem.t =
 fun x y ->
  match (x, y) with
  | Concrete x, Concrete y -> Concrete (max x y)
  | Variable x, Concrete 1 | Concrete 1, Variable x -> Variable x
  | Variable _, Concrete c | Concrete c, Variable _ -> Concrete c
  | Variable x, Variable y ->
      if x = y then Variable x
      else Fmt.failwith "Two different variables (%s, %s)!" x y

let replicate n item = List.init n (fun _ -> item)
let pad n x xs = replicate (n - List.length xs) x @ xs
let txt_td str = td [ txt' str ]

let explain' :
    Tensor_type.t ->
    Tensor_type.bracketed ->
    Tensor_type.t ->
    Tensor_type.bracketed ->
    El.t list =
 fun xs _xs_bracketed ys _ys_bracketed ->
  let max_len = max (List.length xs) (List.length ys) in
  let padded_xs = pad max_len Tensor_type.Elem.one xs in
  let padded_ys = pad max_len Tensor_type.Elem.one ys in
  let unified =
    List.combine padded_xs padded_ys |> List.map (fun (x, y) -> max x y)
  in

  let mk_align_row len items =
    items
    |> List.map Tensor_type.Elem.to_string
    |> pad len "" |> List.map txt_td |> tr
  in

  let mk_row items =
    items |> List.map Tensor_type.Elem.to_string |> List.map txt_td |> tr
  in

  let unequal_info =
    if List.length xs = List.length ys then []
    else
      [
        div
          [
            txt'
              "Because the number of dimensions is not equal, prepend 1s to \
               the front of the tensor with fewer dimensions to make them the \
               same length.";
          ];
        table [ tbody [ mk_row padded_xs; mk_row padded_ys ] ];
      ]
  in

  List.concat
    [
      [
        div
          [
            txt' "First we align the two tensor types, starting from the right.";
          ];
        table [ tbody [ mk_align_row max_len xs; mk_align_row max_len ys ] ];
      ];
      unequal_info;
      [
        div
          [
            txt'
              "Check that in each position, the tensor sizes are equal, or one \
               of them is 1.";
          ];
        div
          [
            txt'
              "Finally, take the maximum of each pair of sizes. The other size \
               is either the same or 1, in which case it will be stretched.";
          ];
        table [ tbody [ mk_row unified ] ];
      ];
    ]

let update_output : (string * string) signal -> El.t list signal =
  S.map (fun (a, b) ->
      match (parse a, parse b) with
      | Ok (a, a_bracketed), Ok (b, b_bracketed) ->
          explain' a a_bracketed b b_bracketed
      | Error msg, Ok _ -> [ txt' "Error parsing A: "; txt' msg ]
      | Ok _, Error msg -> [ txt' "Error parsing B: "; txt' msg ]
      | Error msg1, Error msg2 ->
          [
            div [ txt' "Error parsing A: "; txt' msg1 ];
            div [ txt' "Error parsing B: "; txt' msg2 ];
          ])

let explain container a_type_str b_type_str =
  let a_input = input ~at:[ At.value (Jstr.of_string a_type_str) ] () in
  let b_input = input ~at:[ At.value (Jstr.of_string b_type_str) ] () in
  let result_output = div [] in

  let a_signal, set_a = S.create a_type_str in
  let b_signal, set_b = S.create b_type_str in

  Evr.endless_listen (as_target a_input) Ev.change (fun _evt ->
      set_a (Jstr.to_string (prop El.Prop.value a_input)));
  Evr.endless_listen (as_target b_input) Ev.change (fun _evt ->
      set_b (Jstr.to_string (prop El.Prop.value b_input)));

  let output_signal = S.Pair.v a_signal b_signal |> update_output in
  Elr.def_children result_output output_signal;

  set_children container
    [
      div [ txt' "A: "; a_input ];
      div [ txt' "B: "; b_input ];
      div [ result_output ];
    ]
