open Brr
open Brr_note
open Frontend_util
open Note
open Tensor_playground
open Util

type parse_result =
  | Error of string list * string list
  | Ok of El.t list * Tensor_type.bracketed * Tensor_type.bracketed

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

let parse_types : (string * string) signal -> parse_result signal =
  S.map (fun (a, b) ->
      match (parse_type a, parse_type b) with
      | Ok (a, a_bracketed), Ok (b, b_bracketed) ->
          let elems =
            match (a, b) with
            | [ a ], [ b0; b1 ] ->
                if a > 15 || b1 > 15 then
                  [
                    p
                      [
                        fmt_txt
                          "Sorry, I can't handle dimensions that large (%i)."
                          (max a b1);
                      ];
                  ]
                else if a = b0 then explain_vec_mat a b1
                else
                  [
                    fmt_txt
                      "A and the first dimension of B must match (got %a and \
                       %a)"
                      Fmt.int a Fmt.int b0;
                  ]
            | _ ->
                [
                  fmt_txt
                    "A must have rank 1 and B must have rank 2 (got %a and %a)"
                    Fmt.(list int)
                    a
                    Fmt.(list int)
                    b;
                ]
          in
          Ok (elems, a_bracketed, b_bracketed)
      | a, b -> Error (collect_parse_errors a, collect_parse_errors b))

let explain container a_type_str b_type_str =
  let a_input = input ~at:[ At.value (Jstr.of_string a_type_str) ] () in
  let b_input = input ~at:[ At.value (Jstr.of_string b_type_str) ] () in
  let result_output = div [] in
  let a_parse_error = div [] in
  let b_parse_error = div [] in

  let a_signal, set_a = S.create a_type_str in
  let b_signal, set_b = S.create b_type_str in
  let a_bracket_signal, set_a_bracket = S.create Tensor_type.Unbracketed in
  let b_bracket_signal, set_b_bracket = S.create Tensor_type.Unbracketed in
  let output_signal = S.Pair.v a_signal b_signal |> parse_types in
  let result_signal, set_result = S.create [] in
  let a_parse_error_signal, set_a_parse_error = S.create [] in
  let b_parse_error_signal, set_b_parse_error = S.create [] in

  Evr.endless_listen (as_target a_input) Ev.change (fun _evt ->
      set_a (Jstr.to_string (prop El.Prop.value a_input)));
  Evr.endless_listen (as_target b_input) Ev.change (fun _evt ->
      set_b (Jstr.to_string (prop El.Prop.value b_input)));

  let output_logger =
    S.log output_signal (function
      | Ok (elems, a_bracketed, b_bracketed) ->
          set_result elems;
          set_a_bracket a_bracketed;
          set_b_bracket b_bracketed;
          set_a_parse_error [];
          set_b_parse_error []
      | Error (a_msgs, b_msgs) ->
          set_a_parse_error (List.map txt' a_msgs);
          set_b_parse_error (List.map txt' b_msgs))
  in
  Logr.hold output_logger;

  Elr.def_children result_output result_signal;
  Elr.def_children a_parse_error a_parse_error_signal;
  Elr.def_children b_parse_error b_parse_error_signal;
  set_children container
    [
      div
        [ txt' "A: "; bracketed_input a_bracket_signal a_input; a_parse_error ];
      div
        [ txt' "B: "; bracketed_input b_bracket_signal b_input; b_parse_error ];
      result_output;
    ]
