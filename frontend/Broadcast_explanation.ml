open Brr
open Brr_note
open Tensor_playground
open Note
open Frontend_util
open Util

type parse_result =
  | Error of string option * string option
  | Ok of El.t list * Tensor_type.bracketed * Tensor_type.bracketed

let explain_broadcast : int list -> int list -> El.t list =
 fun xs ys ->
  let max_len = max (List.length xs) (List.length ys) in
  let padded_xs = pad max_len 1 xs in
  let padded_ys = pad max_len 1 ys in
  let unified = List.map2 max padded_xs padded_ys in

  let mk_align_row len items =
    items |> List.map Int.to_string |> pad len "" |> List.map txt_td |> tr
  in

  let mk_row items = items |> List.map Int.to_string |> List.map txt_td |> tr in

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

let update_output : (string * string) signal -> parse_result signal =
  S.map (fun (a, b) ->
      match (parse_type a, parse_type b) with
      | Ok (a, a_bracketed), Ok (b, b_bracketed) ->
          Ok (explain_broadcast a b, a_bracketed, b_bracketed)
      | Error msg, Ok _ -> Error (Some msg, None)
      | Ok _, Error msg -> Error (None, Some msg)
      | Error msg1, Error msg2 -> Error (Some msg1, Some msg2))

let explain container a_type_str b_type_str =
  let a_input = input ~at:[ At.value (Jstr.of_string a_type_str) ] () in
  let b_input = input ~at:[ At.value (Jstr.of_string b_type_str) ] () in
  let result_output = div [] in

  let a_signal, set_a = S.create a_type_str in
  let b_signal, set_b = S.create b_type_str in
  let a_bracket_signal, set_a_bracket = S.create Tensor_type.Unbracketed in
  let b_bracket_signal, set_b_bracket = S.create Tensor_type.Unbracketed in
  let output_signal = S.Pair.v a_signal b_signal |> update_output in
  let result_signal, set_result = S.create [] in

  Evr.endless_listen (as_target a_input) Ev.change (fun _evt ->
      set_a (Jstr.to_string (prop El.Prop.value a_input)));
  Evr.endless_listen (as_target b_input) Ev.change (fun _evt ->
      set_b (Jstr.to_string (prop El.Prop.value b_input)));

  let output_logger =
    S.log output_signal (function
      | Ok (elems, a_bracketed, b_bracketed) ->
          set_result elems;
          set_a_bracket a_bracketed;
          set_b_bracket b_bracketed
      | Error _ -> ())
  in
  Logr.hold output_logger;

  Elr.def_children result_output result_signal;
  set_children container
    [
      div [ txt' "A: "; bracketed_input a_bracket_signal a_input ];
      div [ txt' "B: "; bracketed_input b_bracket_signal b_input ];
      div [ result_output ];
    ]
