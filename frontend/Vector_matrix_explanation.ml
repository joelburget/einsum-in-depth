open Brr
open Brr_note
open Frontend_util
open Note
open Tensor_playground
open Util

let set_children, input, prop, as_target, div, txt', table, tbody, td, tr, p =
  El.(set_children, input, prop, as_target, div, txt', table, tbody, td, tr, p)

type direction = Horizontal | Vertical

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

let combine_mat m1 m2 = List.map2 (List.map2 ( * )) m1 m2

let explain' : int -> int -> El.t list =
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
            td [ txt' "Don't sum"; render_mat expanded_result ];
            td [ txt' "Rows"; render_vec ~direction:Vertical summed_rows ];
          ];
        tr
          [
            td [ txt' "Columns"; render_vec ~direction:Horizontal summed_cols ];
            td [ txt' "Both"; txt' (Int.to_string summed) ];
          ];
      ];
  ]

let update_output : (string * string) signal -> El.t list signal =
  S.map (fun (a, b) ->
      match (parse_type a, parse_type b) with
      | Ok (a, _a_bracketed), Ok (b, _b_bracketed) -> (
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
              else if a = b0 then explain' a b1
              else
                [
                  fmt_txt
                    "A and the first dimension of B must match (got %a and %a)"
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
              ])
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
