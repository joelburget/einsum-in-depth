open Brr
open Brr_note
open Tensor_playground
open Note
open Frontend_util
open Util

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

let explain container a_type_str b_type_str =
  let result_output = div [] in

  let parsed_a_signal, a_input, a_err_elem =
    bracketed_parsed_input parse_type a_type_str
  in
  let parsed_b_signal, b_input, b_err_elem =
    bracketed_parsed_input parse_type b_type_str
  in
  let result_signal =
    S.Pair.v parsed_a_signal parsed_b_signal
    |> S.map (fun (a, b) ->
           match (a, b) with Some a, Some b -> explain_broadcast a b | _ -> [])
  in

  Elr.def_children result_output result_signal;
  set_children container
    [
      div [ txt' "A: "; a_input; a_err_elem ];
      div [ txt' "B: "; b_input; b_err_elem ];
      div [ result_output ];
    ]
