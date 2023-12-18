open Brr
open Brr_note
open Frontend_util
open Note
open Tensor_playground

type validated_inputs = unit

let validate_path n_tensors path =
  if List.length path = 0 then None
  else if List.length path <> n_tensors - 1 then
    Some [ fmt_txt "Path must have length (%d)" (n_tensors - 1) ]
  else
    let rec loop n_tensors path =
      match path with
      | [] -> None
      | (x, y) :: path ->
          if x < 0 || y < 0 then
            Some [ txt' "All path components must be non-negative" ]
          else if x >= n_tensors || y >= n_tensors then
            Some
              [
                fmt_txt
                  "Path components must be between 0 and %d (the number of \
                   un-contracted tensors remaining at this point) (got %a)"
                  (n_tensors - 1)
                  Fmt.(parens (pair ~sep:comma int int))
                  (x, y);
              ]
          else loop n_tensors path
    in
    loop n_tensors path

let validate_inputs :
    Einops.Rewrite.t -> (int * int) list -> (validated_inputs, El.t list) result
    =
 fun rewrite path ->
  let bindings, rhs = rewrite in
  match validate_path (List.length bindings) path with
  | Some errors -> Error errors
  | None ->
      let repeats = Util.find_repeats rhs in
      if repeats <> [] then
        Error
          [
            fmt_txt "Indices in the result must be unique. Invalid: %a."
              Fmt.(brackets (list ~sep:comma string))
              repeats;
          ]
      else
        let lhs_names' = bindings |> List.concat |> String_set.of_list in
        let rhs_names_set = String_set.of_list rhs in
        if not String_set.(subset rhs_names_set lhs_names') then
          Error
            [
              txt'
                "Indices in the result must be a subset of the indices in the \
                 inputs";
            ]
        else Ok ()

let embed_svg : Brr_svg.El.t -> Brr.El.t = Obj.magic

let mk_button text =
  let b = El.button [ txt' text ] in
  let evt = Evr.on_el Ev.click (fun _ -> text) b in
  (b, evt)

let explain container contraction_str path_str =
  let result_output = div [] in
  let parse_path s =
    s |> Path_parser.parse |> Result.map_error (fun msg -> (msg, ""))
  in
  let parsed_path_signal, path_input, path_err_elem =
    bracketed_parsed_input parse_path path_str
  in
  let c_input, input_rewrite_signal = input' contraction_str in

  let f path rewrite =
    let contractions = Einops.Explain.get_contractions ?path rewrite in
    let steps =
      List.map
        (fun contraction ->
          div
            [
              txt' (Einops.Explain.contraction contraction);
              embed_svg (Tensor_diagram.draw_contraction contraction);
            ])
        contractions
    in
    let python_code =
      Einops.Explain.show_loops rewrite |> Fmt.to_to_string Einops.Pyloops.pp
    in
    [ code [ El.pre [ txt' python_code ] ]; div steps ]
  in

  let b1, evt1 = mk_button "a i j, a j k, a i k ->" in
  (* inner product *)
  let b2, evt2 = mk_button "i i ->" in
  (* matmul *)
  let b3, evt3 = mk_button "i j, j k -> i k" in
  (* trace *)
  let b4, evt4 = mk_button "i i ->" in
  (* transpose *)
  let b5, evt5 = mk_button "i j -> j i" in

  let input_evts =
    E.select [ evt1; evt2; evt3; evt4; evt5; S.changes input_rewrite_signal ]
  in
  let current_input = S.hold contraction_str input_evts in
  let rewrite_signal = S.map parse_einsum current_input in

  let explanation_signal =
    S.l2
      (fun path rewrite ->
        match rewrite with
        | Ok rewrite -> f path rewrite
        | Error (msg1, msg2) -> [ txt' msg1; txt' msg2 ])
      parsed_path_signal rewrite_signal
  in

  Elr.def_children result_output explanation_signal;

  set_children container
    [
      div [ txt' "Contraction: "; c_input ];
      div [ txt' "Choose an example contraction"; b1; b2; b3; b4; b5 ];
      div [ txt' "Path (optional): "; path_input; path_err_elem ];
      result_output;
    ]
