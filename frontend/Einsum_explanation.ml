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

let trim_before_colon s =
  try
    let colon_index = String.index s ':' in
    String.sub s (colon_index + 1) (String.length s - colon_index - 1)
  with Not_found -> s

let rec unsnoc = function
  | [] -> failwith "unsnoc empty list"
  | [ x ] -> ([], x)
  | x :: xs ->
      let init, last = unsnoc xs in
      (x :: init, last)

let rec intersperse sep = function
  | [] -> []
  | [ x ] -> [ x ]
  | x :: xs -> x :: x :: sep :: intersperse sep xs

let list_variables = function
  | [] -> []
  | [ x ] -> [ code [ txt' x ] ]
  | xs ->
      let init, last = unsnoc xs in
      (intersperse (txt' ", ") (List.map (fun str -> code [ txt' str ]) init)
       @ [ txt' " and "; code [ txt' last ] ]
        : El.t list)

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
              code [ txt' (Einops.Explain.contraction contraction) ];
              Tensor_diagram.draw_contraction contraction;
            ])
        contractions
    in
    let pyloops = Einops.Explain.show_loops rewrite in
    let python_code = Fmt.to_to_string Einops.Pyloops.pp pyloops in
    let free_indices = String_set.to_list pyloops.free_indices in
    let summation_indices = String_set.to_list pyloops.summation_indices in
    [
      p
        [
          txt'
            "In outer loops, we iterate over all free indices to generate each \
             output element (";
          span (list_variables free_indices);
          txt' "), while in inner loops we iterate over all summation indices (";
          span (list_variables summation_indices);
          txt' ") to sum over each product term.";
          txt'
            "First, here's equivalent, simplified (but slow, because it's not \
             vectorized) Python code. We initialize an empty ";
          code [ txt' "result" ];
          txt'
            " array and then iterate over every position in every axis, \
             building up the result.";
        ];
      code
        ~at:(classes "before:content-[''] after:content-['']")
        [ El.pre [ txt' python_code ] ];
      p
        [
          txt'
            (Fmt.str "Next, we show the steps of the contraction, one by one%s:"
               (if List.length steps = 1 then " (in this case there's just one)"
                else ""));
        ];
      div steps;
    ]
  in

  let selector, selected_signal =
    select
      [
        "batch pos head_index d_model, head_index d_model d_head -> batch pos \
         head_index d_head";
        "a i j, a j k, a i k ->";
        "inner product: i, i ->";
        "i j, i j ->";
        "matrix multiplication: i j, j k -> i k";
        "trace: i i ->";
        "transpose: i j -> j i";
        "sum: i j ->";
        "column sum: i j -> j";
        "row sum: i j -> i";
        "hadamard product: i j, i j -> i j";
        "outer product: i, j -> i j";
        "batch matrix multiplication: b i j, b j k -> b i k";
        (* tensor contraction *)
        "p q r s, t u q v r -> p s t u v";
        "bilinear transformation: i j, j k l -> i j";
      ]
  in

  let logger, c_input, current_input =
    input' ~at:input_classes contraction_str
      (selected_signal |> S.changes |> E.map trim_before_colon)
  in
  Logr.may_hold logger;

  let explanation_signal =
    current_input
    |> S.map (fun str -> str |> trim_before_colon |> Einsum_parser.parse)
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
        ~at:
          (classes
             "max-w-3xl mx-auto px-4 md:px-6 pt-6 md:pt-24 flex flex-col gap-4 \
              dark:prose-invert prose prose-p:my-2")
        [
          h1 ~at:(classes "font-normal") [ txt' "Einsum Explorer" ];
          p
            [
              txt'
                "Einsum notation is a compact and intuitive way to write many \
                 linear algebra operations: matrix multiplication, dot / \
                 Frobenius product, transpose, trace, as well as many more \
                 complex operations which don't have a name.";
            ];
          p
            [
              txt'
                "At the highest level, an einsum string describes the shapes \
                 of each of the input tensors and the output tensor, by \
                 labeling each axis of each tensor. Whenever a label is \
                 repeated, it's summed, whereas if a label only appears once, \
                 it's not summed. (Repeated labels on the same tensor take the \
                 diagonal.)";
            ];
          div [ txt' "Choose an example: "; selector ];
          div
            [
              txt' "Or enter your own contraction: ";
              c_input;
              info
                (div
                   [
                     txt' "We use ";
                     a "https://einops.rocks/" "einops notation";
                     txt' ", not the notation built in to Numpy and Pytorch.";
                   ]);
            ];
          div
            [
              txt' "Path (optional): ";
              path_input;
              info
                (div
                   [
                     p
                       [
                         txt'
                           "A group of tensors is reduced one pair at a time. \
                            (This only matters when there are three or more \
                            tensors). The path specifies the order in which we \
                            reduce the tensors.";
                       ];
                     p
                       [
                         txt'
                           "Why does this matter? The result will be the same \
                            no matter which order tensors were reduced in, but \
                            different orders can have vastly different \
                            performance characteristics. ";
                         a "https://github.com/dgasmith/opt_einsum" "opt_einsum";
                         txt'
                           " shows an example where the same reduction takes \
                            3000x longer without optimization.";
                       ];
                     p
                       [
                         txt'
                           "The general problem of finding the optimal path is \
                            NP-hard. See the ";
                         a
                           "https://dgasmith.github.io/opt_einsum/paths/introduction/"
                           "opt_einsum docs";
                         txt' " for more.";
                       ];
                     p
                       [
                         txt'
                           "In this tool, you can specify the path as a list \
                            of pairs of integers, separated by commas. For \
                            example, ";
                         code [ txt' "(0, 1), (1, 2)" ];
                       ];
                   ]);
              path_err_elem;
            ];
          result_output;
          div ~at:(classes "min-h-32") [];
        ];
    ]
