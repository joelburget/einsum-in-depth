open Brr
open Note_brr
open Frontend_util
open Note
open Tensor_playground
open Util

let combine_mat m1 m2 = List.map2 (List.map2 ( * )) m1 m2

let explain_vec_mat : int -> int -> El.t list =
 fun d0 d1 ->
  let v_elems = List.init d0 (fun i -> i) in
  let m_elems = List.init d0 (fun i -> List.init d1 (fun j -> (i * d1) + j)) in
  let v_expanded = List.map (fun i -> Util.replicate d1 i) v_elems in
  let expanded_result = combine_mat v_expanded m_elems in
  let summed_rows = List.map sum expanded_result in
  let summed_cols = expanded_result |> transpose |> List.map sum in
  let summed = sum summed_rows in
  [
    p [ txt' "We start with a vector and matrix." ];
    div
      ~at:[ class_ "row" ]
      [
        table [ render_vec ~direction:Horizontal v_elems ];
        table [ render_mat m_elems ];
      ];
    p
      [
        txt'
          "Rotate the vector and stretch to match the dimensions of the matrix.";
      ];
    div
      ~at:[ class_ "row" ]
      [
        table [ render_vec ~direction:Vertical v_elems ];
        table [ render_mat v_expanded ];
      ];
    p [ txt' "Now, elementwise multiply the stretched vector and the matrix." ];
    div
      ~at:[ class_ "row" ]
      [
        table [ render_mat v_expanded ];
        div [ txt' "@" ];
        table [ render_mat m_elems ];
        div [ txt' "=" ];
        table [ render_mat expanded_result ];
      ];
    p [ txt' "Finally, sum in either or both directions." ];
    table
      [
        tr
          [
            td
              [
                txt' "Don't sum (";
                code [ txt' "i,ij->ij" ];
                txt' ")";
                render_mat expanded_result;
              ];
            td
              [
                txt' "Rows (";
                code [ txt' "i,ij->i" ];
                txt' ")";
                render_vec ~direction:Vertical summed_rows;
              ];
          ];
        tr
          [
            td
              [
                txt' "Columns (";
                code [ txt' "i,ij->j" ];
                txt' ")";
                render_vec ~direction:Horizontal summed_cols;
              ];
            td
              [
                txt' "Both (";
                code [ txt' "i,ij->" ];
                txt' ")";
                txt' (Int.to_string summed);
              ];
          ];
      ];
  ]

let validate_inputs : int list -> int list -> (int * int, El.t) result =
 fun a_lst b_lst ->
  match (a_lst, b_lst) with
  | [ a ], [ b0; b1 ] ->
      if a > 15 || b1 > 15 then
        Error
          (p
             [
               fmt_txt "Sorry, I can't handle dimensions that large (%i)."
                 (max a b1);
             ])
      else if a = b0 then Ok (a, b1)
      else
        Error
          (fmt_txt "A and the first dimension of B must match (got %a and %a)"
             Fmt.int a Fmt.int b0)
  | _ ->
      Error
        (fmt_txt "A must have rank 1 and B must have rank 2 (got %a and %a)"
           Fmt.(list int)
           a_lst
           Fmt.(list int)
           b_lst)

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
    |> S.map (fun (as_opt, bs_opt) ->
           match (as_opt, bs_opt) with
           | Some a, Some b -> (
               match validate_inputs a b with
               | Ok (a, b) -> explain_vec_mat a b
               | Error elem -> [ elem ])
           | _ -> [])
  in

  Elr.def_children result_output result_signal;
  set_children container
    [
      div [ txt' "A: "; a_input; a_err_elem ];
      div [ txt' "B: "; b_input; b_err_elem ];
      result_output;
    ]
