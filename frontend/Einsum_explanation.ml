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

let radio :
      'a. desc:string -> selected:bool signal -> 'a -> ('a -> unit) -> El.t =
 fun ~desc ~selected value f ->
  let base_classes =
    "flex items-center justify-center rounded-md bg-white px-3 py-3 text-sm \
     font-semibold uppercase text-gray-900 ring-1 ring-gray-300 \
     hover:bg-gray-50 data-[checked]:bg-indigo-600 data-[checked]:text-white \
     data-[checked]:ring-0 data-[focus]:data-[checked]:ring-2 \
     data-[focus]:ring-2 data-[focus]:ring-indigo-600 \
     data-[focus]:ring-offset-2 data-[checked]:hover:bg-indigo-500 sm:flex-1 \
     [&:not([data-focus],[data-checked])]:ring-inset cursor-pointer \
     focus:outline-none"
  in
  let attrs = At.[ v (Jstr.v "role") (Jstr.v "radio") ] in
  let input_elem = span ~at:(attrs @ classes base_classes) [ txt' desc ] in
  Elr.def_at (Jstr.v "data-checked")
    (selected
    |> S.map (fun selected -> if selected then Some (Jstr.v "true") else None))
    input_elem;
  Elr.def_at (Jstr.v "aria-checked")
    (selected
    |> S.map (fun selected ->
           Some (Jstr.v (if selected then "true" else "false"))))
    input_elem;
  Evr.endless_listen (as_target input_elem) Ev.click (fun _ -> f value);
  input_elem

let explain container contraction_str path_str =
  let result_output = div [] in
  let parsed_path_signal, path_input, path_err_elem =
    bracketed_parsed_input parse_path path_str
  in

  let code_preference_signal, code_preference_selector =
    let preference_signal, set_preference = S.create Einops.Numpy in
    let numpy_radio =
      radio ~desc:"Numpy"
        ~selected:
          (preference_signal
          |> S.map (function Einops.Numpy -> true | Pytorch -> false))
        Einops.Numpy set_preference
    in
    let torch_radio =
      radio ~desc:"Pytorch"
        ~selected:
          (preference_signal
          |> S.map (function Einops.Numpy -> false | Pytorch -> true))
        Einops.Pytorch set_preference
    in
    let selector =
      Brr.El.(
        fieldset
          ~at:
            (At.v (Jstr.v "role") (Jstr.v "radiogroup")
            :: classes "mt-2 grid grid-cols-2 gap-3 max-w-48")
          [ numpy_radio; torch_radio ])
    in
    (preference_signal, selector)
  in

  let render_steps path rewrite code_preference =
    let contractions = Einops.Explain.get_contractions ?path rewrite in
    let show_step_no =
      match contractions with
      | Unary_contraction _ -> false
      | Binary_contractions contractions -> List.length contractions > 1
    in
    let framework_name =
      match code_preference with
      | Einops.Numpy -> "Numpy"
      | Pytorch -> "Pytorch"
    in
    let tensor_diagram_info =
      info
        (div
           [
             h2 [ txt' "Interpreting a Tensor Diagram" ];
             p
               [
                 txt'
                   "Each node in a tensor diagram represents a tensor (e.g a \
                    matrix or vector). Each line emanating from a tensor \
                    represents one of that tensor's dimensions. When two \
                    tensors are connected by a single line that represents a \
                    contraction (summation over the connected indices). Look \
                    through the examples to see how common operations \
                    correspond to tensor diagrams.";
               ];
             p
               [
                 txt' "See ";
                 a "https://tensornetwork.org/diagrams/" "tensornetwork.org";
                 txt' " or ";
                 a "https://www.tensors.net/intro" "tensors.net";
                 txt' " for more.";
               ];
           ])
    in
    let steps =
      match contractions with
      | Einops.Explain.Unary_contraction (tensor, contraction) ->
          let Einops.Unary_contraction.
                { operations; contracted; preserved = _; result_type } =
            contraction
          in
          [
            div
              [
                span [ txt' "Contract " ];
                code [ txt' Fmt.(str "%a" (list string ~sep:sp) contracted) ];
                span [ txt' Fmt.(str "(in %s this would be " framework_name) ];
                code
                  [
                    txt'
                      Fmt.(
                        str "%a"
                          (Einops.Unary_contraction.pp_ops code_preference)
                          operations);
                  ];
                span [ txt' ")" ];
                Tensor_diagram.draw_unary_contraction contraction;
                tensor_diagram_info;
                Isometric.Scene.render [ tensor; result_type ];
              ];
          ]
      | Binary_contractions contractions ->
          List.mapi
            (fun i contraction ->
              let l_tensor, r_tensor, result_type =
                Einops.Binary_contraction.
                  (contraction.l, contraction.r, contraction.result_type)
              in
              div
                [
                  span
                    [
                      txt'
                        (if show_step_no then Fmt.str "Step %d: " (i + 1)
                         else "");
                    ];
                  span [ txt' "Contract " ];
                  code
                    [
                      txt'
                        Fmt.(
                          str "%a" (list string ~sep:sp) contraction.contracted);
                    ];
                  span [ txt' " (" ];
                  code
                    [
                      txt'
                        Fmt.(
                          str "@[%a, %a@] -> @[%a@]" (list string ~sep:sp)
                            l_tensor (list string ~sep:sp) r_tensor
                            (list string ~sep:sp) result_type);
                    ];
                  span [ txt' ")." ];
                  Tensor_diagram.draw_binary_contraction l_tensor r_tensor
                    contraction;
                  tensor_diagram_info;
                  Isometric.Scene.render [ l_tensor; r_tensor; result_type ];
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
      code_preference_selector;
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

  let clicked_op_in_text, send_clicked_op_in_text = E.create () in
  let selected_changes =
    selected_signal |> S.changes |> E.map trim_before_colon
  in

  let op_button text op_str =
    let elem = Brr.El.button ~at:(classes "underline") [ txt' text ] in
    let evt = Note_brr.Evr.on_el Ev.click (fun _ -> ()) elem in
    (* XXX does this have the correct semantics? Event leak etc? *)
    Logr.may_hold E.(log evt (fun () -> send_clicked_op_in_text op_str));
    elem
  in

  let logger, c_input, current_input =
    input' ~at:input_classes contraction_str
      (E.select [ clicked_op_in_text; selected_changes ])
  in
  Logr.may_hold logger;

  let parsed_input_signal =
    S.map
      (fun str -> str |> trim_before_colon |> Einsum_parser.parse)
      current_input
  in
  let explanation_signal =
    S.l3
      (fun path rewrite code_preference ->
        match rewrite with
        | Ok rewrite -> render_steps path rewrite code_preference
        | Error msg -> [ txt' msg ])
      parsed_path_signal parsed_input_signal code_preference_signal
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
                 linear algebra operations: ";
              op_button "matrix multiplication" "a b, b c -> a c";
              txt' ", ";
              op_button "dot" "a, a ->";
              txt' " / ";
              op_button "Frobenius product" "a b, a b ->";
              info
                (span
                   [
                     txt' "The ";
                     a "https://math.stackexchange.com/a/4858481/657087"
                       "Frobenius product";
                     txt'
                       " generalizes the dot product from vectors to matrices. \
                        A dot product takes two vectors of the same shape and \
                        returns a scalar. The Frobenius product takes two \
                        matrices of the same shape and returns a scalar. This \
                        can be generalized further by taking two tensors of \
                        the same shape (of arbitrary dimension), pointwise \
                        multiplying them, and summing over each element.";
                   ]);
              txt' ", ";
              op_button "transpose" "a b -> b a";
              txt' ", ";
              op_button "trace" "a a ->";
              txt' ", as well as many more ";
              op_button "complex operations" "a a b c, a d, d e f -> b c e f";
              txt' " which don't have a name.";
            ];
          p
            [
              txt'
                "At the highest level, an einsum string describes the shapes \
                 of each of the input tensors";
              info
                (span
                   [
                     txt'
                       "A tensor is the generalization of a vector or matrix \
                        to any number of dimensions. A vector is a \
                        one-dimensional tensor. A matrix is a two-dimensional \
                        tensor. A tensor can be three, four, or more \
                        dimensions.";
                   ]);
              txt'
                " and the output tensor, by labeling each axis of each tensor. \
                 Whenever a label is repeated, it's summed, whereas if a label \
                 only appears once, it's not summed. (Repeated labels on the \
                 same tensor take elements on the diagonal.)";
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
                         code [ txt' "(1, 2), (0, 1)" ];
                         txt'
                           " means that (if there are three tensors) the \
                            second two are reduced first, then (of the tensors \
                            remaining) the first two are reduced. (The last \
                            path element must be (0, 1) because those are the \
                            only tensors remaining after all the others have \
                            been reduced.) If no order is specified, we \
                            repeatedly reduce the first two elements. In other \
                            words, the default path is ";
                         code [ txt' "(0, 1) * (n_tensors - 1)" ];
                         txt' ".";
                       ];
                   ]);
              path_err_elem;
            ];
          result_output;
          div ~at:(classes "min-h-32") [];
        ];
    ]
