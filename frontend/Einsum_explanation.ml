open Brr
open Note_brr
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

let parse_path s =
  s |> Path_parser.parse |> Result.map_error (fun msg -> (msg, ""))

let explain container contraction_str path_str =
  let result_output = div [] in
  let parsed_path_signal, path_input, path_err_elem =
    bracketed_parsed_input parse_path path_str
  in

  let f path rewrite =
    let contractions = Einops.Explain.get_contractions ?path rewrite in
    let steps =
      List.map
        (fun contraction ->
          div
            [
              code [txt' (Einops.Explain.contraction contraction)];
              embed_svg (Tensor_diagram.draw_contraction contraction);
            ])
        contractions
    in
    let python_code =
      Einops.Explain.show_loops rewrite |> Fmt.to_to_string Einops.Pyloops.pp
    in
    [
      p [ txt' "First, we give the equivalent Python code" ];
      code [ El.pre [ txt' python_code ] ];
      p [ txt' "Next, we show the steps of the contraction, one by one" ];
      div steps;
    ]
  in

  let selector, selected_signal =
    select
      [
        "batch pos head_index d_model, head_index d_model d_head -> batch pos head_index d_head";
        "a i j, a j k, a i k ->";
        (* inner product *)
        "i, i ->";
        "i j, i j ->";
        (* matmul *)
        "i j, j k -> i k";
        (* trace *)
        "i i ->";
        (* transpose *)
        "i j -> j i";
        (* sum *)
        "i j ->";
        (* column sum *)
        "i j -> j";
        (* row sum *)
        "i j -> i";
        (* hadamard product *)
        "i j, i j -> i j";
        (* outer product *)
        "i, j -> i j";
        (* batch matmul *)
        "b i j, b j k -> b i k";
        (* tensor contraction *)
        "p q r s, t u q v r -> p s t u v";
        (* bilinear transformation *)
        "i j, j k l -> i j";
      ]
  in

  let logger, c_input, current_input =
    input' ~at:input_classes contraction_str (S.changes selected_signal)
  in
  Logr.may_hold logger;

  let explanation_signal =
    current_input |> S.map Einsum_parser.parse
    |> S.l2
         (fun path rewrite ->
           match rewrite with
           | Ok rewrite -> f path rewrite
           | Error msg -> [ txt' msg ])
         parsed_path_signal
  in

  Elr.def_children result_output explanation_signal;

  set_children container
    [
      div
        ~at:[ class_ "m-2" ]
        [
          div ~at:(classes "flex flex-row")
            [
              div [ txt' "Enter a contraction: "; c_input ];
              div [ txt' "Or choose an example: "; selector ];
            ];
          div [ txt' "Path (optional): "; path_input; path_err_elem ];
          result_output;
        ];
    ]
