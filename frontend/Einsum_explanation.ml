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

let mk_color_style color = At.style (Jstr.v Fmt.(str "color: %a" string color))

let list_variables get_color vars =
  let mk_code x = code ~at:[ mk_color_style (get_color x) ] [ txt' x ] in
  match vars with
  | [] -> [ txt' "in his case there are none" ]
  | [ x ] -> [ mk_code x ]
  | [ x; y ] -> [ mk_code x; txt' " and "; mk_code y ]
  | xs ->
      let init, last = unsnoc xs in
      Einops.intersperse (fun () -> txt' ", ") (List.map mk_code init)
      @ [ txt' ", and "; mk_code last ]

module Make_formatter : sig
  type t = Brr.El.t * Format.formatter

  val make_formatter : unit -> t
end = struct
  type t = Brr.El.t * Format.formatter

  let mk_reactive cons ?d ?at s =
    let result = cons ?d ?at [] in
    let () = Elr.def_children result s in
    result

  let make_formatter () =
    let add_elem_e, (trigger_add_elem : El.t Note.E.send) = E.create () in

    let do_add_elem =
      add_elem_e |> E.map (fun elem elems -> elems @ [ elem ])
    in
    let top_level_elems = S.accum [] do_add_elem in
    let stack : (string * El.t Queue.t) Stack.t = Stack.create () in
    let add_at_current_level elem =
      match Stack.top stack with
      | _, q -> Queue.add elem q
      | exception Stack.Empty -> trigger_add_elem elem
    in
    let add_text str = add_at_current_level (span [ txt' str ]) in
    let add_spaces n =
      match () with
      | () when n > 0 -> add_text (String.make n ' ')
      | () when n < 0 -> Printf.printf "add_spaces negative value (!): %d\n" n
      | () -> ()
    in
    let out_fns : Format.formatter_out_functions =
      {
        out_string =
          (fun str _start _char_count -> add_text str)
          (* No need to do anything -- we update the element immediately on receiving
             characters. *);
        out_flush = (fun x -> x);
        out_newline = (fun () -> add_at_current_level (El.br ()));
        out_spaces = add_spaces;
        out_indent = add_spaces;
      }
    in
    let stag_fns : Format.formatter_stag_functions =
      (* We open a new span for every range tag we encounter. All children until we
         encounter the matching close tag will be nested under it (by enqueuing). *)
      Format.
        {
          mark_open_stag =
            (function
            | Einops.Colored color ->
                Stack.push (color, Queue.create ()) stack;
                ""
            | _ ->
                Stdio.printf "RangeFormatter unknown stag\n";
                "")
            (* Closing a range; create the span holding all of the enqueued children. *);
          mark_close_stag =
            (fun _ ->
              match Stack.pop stack with
              | color, q ->
                  q |> Queue.to_seq |> List.of_seq
                  |> span ~at:[ mk_color_style color ]
                  |> add_at_current_level;
                  ""
              | exception Stack.Empty -> "");
          print_open_stag = (fun _ -> ());
          print_close_stag = (fun _ -> ());
        }
    in
    let fmt = Format.formatter_of_out_functions out_fns in
    Format.pp_set_tags fmt true;
    Format.pp_set_formatter_stag_functions fmt stag_fns;
    (mk_reactive El.code top_level_elems, fmt)
end

open Make_formatter

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

module Tabs : sig
  type tab = { name : string }

  val make_tabs : ?default:int -> tab list -> El.t signal * int signal
end = struct
  let chevron_down_icon () =
    let open Brr_svg in
    let open El in
    svg
      ~at:
        (svg_classes
           "pointer-events-none col-start-1 row-start-1 mr-2 size-5 \
            self-center justify-self-end fill-gray-500 size-6"
        @ [
            svg_at "aria-hidden" "true";
            svg_at "stroke" "currentColor";
            svg_at "stroke-width" "1.5";
          ]
        @ [ viewbox "0 0 24 24"; fill "none" ])
      [
        path
          ~at:
            ([ d "m19.5 8.25-7.5 7.5-7.5-7.5" ]
            @ [
                svg_at "stroke-linecap" "round";
                svg_at "stroke-linejoin" "round";
              ])
          [];
      ]

  type tab = { name : string }

  let make_tabs ?(default = 0) tabs =
    let current_tab_s, set_current_tab = S.create default in
    let select =
      El.select
        ~at:
          (At.v (Jstr.v "default-value") (Jstr.v (List.hd tabs).name)
          :: At.v (Jstr.v "aria-label") (Jstr.v "Select a tab")
          :: classes
               "col-start-1 row-start-1 w-full appearance-none rounded-md \
                bg-white py-2 pl-3 pr-8 text-base text-gray-900 outline \
                outline-1 -outline-offset-1 outline-gray-300 focus:outline-2 \
                focus:-outline-offset-2 focus:outline-indigo-600")
        (List.map (fun tab -> El.option [ txt' tab.name ]) tabs)
    in
    let _listener =
      Ev.listen Ev.change
        (fun _evt ->
          let tab_name = El.prop El.Prop.value select |> Jstr.to_string in
          match List.find_index (fun tab -> tab.name = tab_name) tabs with
          | Some i -> set_current_tab i
          | None -> ())
        (El.as_target select)
    in
    let elem =
      current_tab_s
      |> S.l1 (fun current_tab_num ->
             div
               [
                 div
                   ~at:
                     (At.v (Jstr.v "aria-label") (Jstr.v "Tabs")
                     :: classes "grid grid-cols-1 sm:hidden")
                   [ select; embed_svg (chevron_down_icon ()) ];
                 div
                   ~at:(classes "hidden sm:block")
                   [
                     div
                       ~at:(classes "border-b border-gray-200")
                       [
                         nav
                           ~at:
                             (At.v (Jstr.v "aria-label") (Jstr.v "Tabs")
                             :: classes "-mb-px flex space-x-8")
                           (tabs
                           |> List.mapi (fun i tab ->
                                  let attrs =
                                    if i = current_tab_num then
                                      At.v (Jstr.v "aria-current")
                                        (Jstr.v "page")
                                      :: classes
                                           "border-indigo-500 text-indigo-600"
                                    else
                                      classes
                                        "border-transparent text-gray-500 \
                                         hover:border-gray-300 \
                                         hover:text-gray-700"
                                  in
                                  let attrs =
                                    attrs
                                    @ classes
                                        "whitespace-nowrap border-b-2 px-1 \
                                         py-4 text-sm font-medium"
                                  in
                                  let btn =
                                    El.button ~at:attrs [ txt' tab.name ]
                                  in
                                  let evt =
                                    Note_brr.Evr.on_el Ev.click
                                      (fun _ -> ())
                                      btn
                                  in
                                  Logr.may_hold
                                    E.(log evt (fun () -> set_current_tab i));
                                  btn));
                       ];
                   ];
               ])
    in
    (elem, current_tab_s)
end

let mk_tensor_diagram_info () =
  info
    (div
       [
         h2 ~at:(classes "text-lg") [ txt' "Interpreting a Tensor Diagram" ];
         p
           [
             txt'
               "Each node in a tensor diagram represents a tensor (e.g a \
                matrix or vector). Each line emanating from a tensor \
                represents one of that tensor's dimensions. When two tensors \
                are connected by a single line that represents a contraction \
                (summation over the connected indices). Look through the \
                examples to see how common operations correspond to tensor \
                diagrams.";
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

let render_steps path rewrite code_preference_selector code_preference =
  let edge_attributes = Colors.assign_edge_attributes (fst rewrite) in
  let get_color edge_name =
    match Hashtbl.find_opt edge_attributes edge_name with
    | None -> if Colors.prefers_dark () then "#fff" else "#000"
    | Some { Colors.color; _ } -> color
  in
  let pp_var = Einops.pp_var get_color in
  let contractions = Einops.Explain.get_contractions ?path rewrite in
  let show_step_no =
    match contractions with
    | Unary_contraction _ -> false
    | Binary_contractions contractions -> List.length contractions > 1
  in
  let framework_name, framework_code_name =
    match code_preference with
    | Einops.Numpy -> ("Numpy", "np")
    | Pytorch -> ("Pytorch", "torch")
  in
  let steps =
    match contractions with
    | Einops.Explain.Unary_contraction (tensor, contraction) ->
        let Einops.Unary_contraction.
              { operations; contracted; preserved = _; result_type } =
          contraction
        in
        let valid_isometric_tensors =
          List.for_all Isometric.Tensor.is_valid [ tensor; result_type ]
        in
        let tab_selector_s, current_tab_s =
          Tabs.make_tabs
            ~default:(if valid_isometric_tensors then 1 else 0)
            [ { name = "Tensor Diagram" }; { name = "Isometric Diagram" } ]
        in
        let diagram_s =
          current_tab_s
          |> S.map (fun i ->
                 match i with
                 | 0 ->
                     [
                       (* Tensor_diagram.draw_unary_contraction edge_attributes *)
                       (*   contraction; *)
                       mk_tensor_diagram_info ();
                     ]
                 | _ ->
                     if valid_isometric_tensors then
                       [
                         Isometric.Scene.render ~edge_attributes [ tensor ]
                           result_type;
                       ]
                     else
                       [
                         txt'
                           "Isometric diagrams are only available for tensors \
                            of dimension 3 or less.";
                       ])
        in
        let contraction_children =
          match contraction.contracted with
          | [] -> []
          | _ ->
              let contracted_list, code_ppf = make_formatter () in
              Fmt.(pf code_ppf "%a@?" (list ~sep:sp pp_var) contracted);
              [
                span [ txt' "Contract " ];
                contracted_list;
                span [ txt' Fmt.(str "(in %s this would be " framework_name) ];
                code
                  [
                    txt'
                      Fmt.(
                        str "%a"
                          (Einops.Unary_contraction.pp_ops code_preference)
                          operations);
                  ];
                span [ txt' ")." ];
              ]
        in
        let tab_selector_parent, diagram_parent = (div [], div []) in
        Elr.def_children tab_selector_parent
          (tab_selector_s |> S.l1 (fun x -> [ x ]));
        Elr.def_children diagram_parent diagram_s;
        [ div (contraction_children @ [ tab_selector_parent; diagram_parent ]) ]
    | Binary_contractions contractions ->
        List.mapi
          (fun i contraction ->
            let l_tensor, r_tensor, result_type =
              Einops.Binary_contraction.
                (contraction.l, contraction.r, contraction.result_type)
            in
            let aligned_list, align_ppf = make_formatter () in
            Fmt.(
              pf align_ppf "@[%a@]@?" (list pp_var ~sep:sp) contraction.aligned);
            let contracted_list, contract_ppf = make_formatter () in
            Fmt.(
              pf contract_ppf "@[%a@]@?" (list pp_var ~sep:sp)
                contraction.contracted);
            let binary_tensor_code, binary_tensor_ppf = make_formatter () in
            Fmt.(
              pf binary_tensor_ppf "@[@[%a, %a@]@ ->@ @[%a@]@]@?"
                (list pp_var ~sep:sp) l_tensor (list pp_var ~sep:sp) r_tensor
                (list pp_var ~sep:sp) result_type);
            let alignment_children =
              match contraction.aligned with
              | [] -> []
              | _ ->
                  [
                    span [ txt' "Align " ];
                    aligned_list;
                    span [ txt' " (these indices are not summed over yet)." ];
                  ]
            in
            let contraction_children =
              match contraction.contracted with
              | [] -> []
              | _ ->
                  [
                    span [ txt' "Contract " ];
                    contracted_list;
                    span [ txt' " (i.e. sum over these indices) (" ];
                    binary_tensor_code;
                    span [ txt' ")." ];
                  ]
            in
            let valid_isometric_tensors =
              List.for_all Isometric.Tensor.is_valid
                [ l_tensor; r_tensor; result_type ]
            in
            let tab_selector_s, current_tab_s =
              Tabs.make_tabs
                ~default:(if valid_isometric_tensors then 1 else 0)
                [ { name = "Tensor Diagram" }; { name = "Isometric Diagram" } ]
            in
            let diagram_s =
              current_tab_s
              |> S.map (function
                   | 0 ->
                       [
                         (* Tensor_diagram.draw_binary_contraction edge_attributes *)
                         (*   l_tensor r_tensor contraction; *)
                         mk_tensor_diagram_info ();
                       ]
                   | _ ->
                       if valid_isometric_tensors then
                         [
                           Isometric.Scene.render ~edge_attributes
                             [ l_tensor; r_tensor ] result_type;
                         ]
                       else
                         [
                           txt'
                             "Isometric diagrams are only available for \
                              tensors of dimension 3 or less.";
                         ])
            in
            let tab_selector_parent, diagram_parent = (div [], div []) in
            Elr.def_children tab_selector_parent
              (tab_selector_s |> S.l1 (fun x -> [ x ]));
            Elr.def_children diagram_parent diagram_s;
            let div_children =
              h3
                [
                  txt'
                    (if show_step_no then Fmt.str "Step %d: " (i + 1) else "");
                ]
              :: alignment_children
              @ [ txt' " " ]
              @ contraction_children
              @ [ tab_selector_parent; diagram_parent ]
            in
            div div_children)
          contractions
  in
  let pyloops = Einops.Explain.show_loops rewrite in
  let python_code, code_ppf = make_formatter () in
  Fmt.pf code_ppf "%a@?" (Einops.Pyloops.pp get_color) pyloops;
  let frob_python_code, frob_code_ppf = make_formatter () in
  Fmt.pf frob_code_ppf "%a@?"
    (Einops.Pyloops.pp ~use_frob:code_preference get_color)
    pyloops;
  let free_indices = String_set.to_list pyloops.free_indices in
  let summation_indices = String_set.to_list pyloops.summation_indices in
  [
    h2 [ txt' "Python Code" ];
    p
      [
        txt'
          "In outer loops, we iterate over all free indices to generate each \
           output element (";
        span (list_variables get_color free_indices);
        txt' "), while in inner loops we iterate over all summation indices (";
        span (list_variables get_color summation_indices);
        txt'
          ") to sum over each product term. First, here's equivalent, \
           simplified (but slow, because it's not vectorized) Python code. We \
           initialize an empty ";
        code [ txt' "result" ];
        txt'
          " array and then iterate over every position in every axis, building \
           up the result.";
      ];
    code_preference_selector;
    div ~at:(classes "flex flex-row")
      [
        El.pre
          [
            code
              ~at:(classes "before:content-[''] after:content-['']")
              [ python_code ];
          ];
      ];
    p
      [
        txt' "In this next version of the code, we use the Frobenius product (";
        code [ txt' (Fmt.str "%s.sum(inputs)" framework_code_name) ];
        txt' ") instead of iterating over each summation index individually.";
      ];
    div ~at:(classes "flex flex-row")
      [
        El.pre
          [
            code
              ~at:(classes "before:content-[''] after:content-['']")
              [ frob_python_code ];
          ];
      ];
    h2 [ txt' "Contraction Steps" ];
    p
      [
        txt' "Next, we show the steps of the contraction, one by one. ";
        (match contractions with
        | Unary_contraction _ ->
            txt'
              "In this case, since we're only contracting one tensor, there's \
               just a single step."
        | Binary_contractions steps ->
            txt'
              (Fmt.str
                 "In this case, since there are multiple tensors, we contract \
                  one pair at a time%s."
                 (match steps with
                 | [ _ ] -> " (in this case there's just one pair, so one step)"
                 | _ -> "")));
      ];
    div steps;
  ]

let mk_code_preference_selector () =
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

let explain container contraction_str path_str =
  let result_output = div [] in
  let parsed_path_signal, path_input, path_err_elem =
    bracketed_parsed_input parse_path path_str
  in

  let code_preference_signal, code_preference_selector =
    mk_code_preference_selector ()
  in

  let clicked_op_in_text_e, send_clicked_op_in_text = E.create () in

  let selector, selected_signal =
    select clicked_op_in_text_e
      [
        "---";
        "a i j, a j k, a i k ->";
        "batch pos head_index d_model, head_index d_model d_head -> batch pos \
         head_index d_head";
        "inner product: i, i ->";
        "matrix multiplication: i j, j k -> i k";
        "trace: i i ->";
        "transpose: i j -> j i";
        "sum: i j ->";
        "column sum: i j -> j";
        "row sum: i j -> i";
        "hadamard product: i j, i j -> i j";
        "frobenius prodduct: i j, i j ->";
        "outer product: i, j -> i j";
        "batch matrix multiplication: b i j, b j k -> b i k";
        "p q r s, t u q v r -> p s t u v";
        "bilinear transformation: i j, j k l -> i j";
      ]
  in

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
      (E.select [ clicked_op_in_text_e; selected_changes ])
  in
  Logr.may_hold logger;

  let parsed_input_signal =
    S.map
      (fun str -> str |> trim_before_colon |> Einsum_parser.Friendly.parse)
      current_input
  in
  let explanation_signal =
    S.l3
      (fun path rewrite code_preference ->
        match rewrite with
        | Ok rewrite ->
            render_steps path rewrite code_preference_selector code_preference
        | Error msg -> [ div ~at:(classes "text-red-600") [ txt' msg ] ])
      parsed_path_signal parsed_input_signal code_preference_signal
  in

  Elr.def_children result_output explanation_signal;

  set_children container
    [
      div
        ~at:
          (classes
             "max-w-3xl mx-auto px-4 md:px-6 pt-6 md:pt-24 flex flex-col gap-4 \
              prose dark:prose-invert prose-p:my-2 prose-pre:bg-gray-100 \
              prose-pre:dark:bg-[#00000080] prose-pre:text-gray-900 \
              prose-pre:dark:text-[#d1d5db]")
        [
          h1 ~at:(classes "font-normal") [ txt' "Einsum Explorer" ];
          p
            [
              txt' "Einsum";
              info
                (span
                   [
                     txt' "Short for ";
                     Brr.El.i [ txt' "Einstein summation" ];
                     txt' " notation";
                   ]);
              txt'
                " notation is a compact and intuitive way to write many linear \
                 algebra operations: ";
              op_button "matrix multiplication" "i j, j k -> i k";
              txt' ", ";
              op_button "dot" "i, i ->";
              txt' " / ";
              op_button "Frobenius" "i j, i j ->";
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
              txt' " product, ";
              op_button "transpose" "i j -> j i";
              txt' ", ";
              op_button "trace" "i i ->";
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
                (div
                   [
                     p
                       [
                         txt' "Here by ";
                         Brr.El.i [ txt' "tensor" ];
                         txt'
                           " we simply mean a multi-dimensional array. Numpy \
                            calls these ";
                         code [ txt' "ndarray" ];
                         txt'
                           "s (n-dimensional arrays), but deep learning \
                            frameworks typically call them tensors.";
                       ];
                     p
                       [
                         txt'
                           "A tensor is the generalization of a vector or \
                            matrix to any number of dimensions. A vector is a \
                            one-dimensional tensor. A matrix is a \
                            two-dimensional tensor. But a tensor can also be \
                            three, four, or more dimensions.";
                       ];
                   ]);
              txt'
                " and the output tensor, by labeling each axis of each tensor. \
                 Whenever a label is repeated, it's summed, whereas if a label \
                 only appears once, it's not summed (compare ";
              code [ txt' "a" ];
              txt' " and ";
              code [ txt' "b" ];
              txt' "in a ";
              op_button "matrix-vector product" "a b, b -> a";
              txt'
                "). (Repeated labels on the same tensor take elements on the \
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
                     txt'
                       ", not the notation built in to Numpy and Pytorch. Note \
                        that einops notation supports parenthesis and ellipsis \
                        notation, but we don't.";
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
          div
            [
              h2 [ txt' "Other resources" ];
              ul
                [
                  li
                    [
                      a
                        "https://obilaniu6266h16.wordpress.com/2016/02/04/einstein-summation-in-numpy/"
                        "Olexa Bilaniuk's Einstein Summation in Numpy";
                    ];
                  li
                    [
                      a "https://ajcr.net/Basic-guide-to-einsum/"
                        "ajcr's A basic introduction to NumPy's einsum";
                    ];
                  li
                    [
                      a
                        "https://stackoverflow.com/questions/72952005/how-is-numpy-einsum-implemented"
                        "Stack Overflow: How is numpy.einsum implemented?";
                    ];
                ];
            ];
          div ~at:(classes "min-h-32") [];
        ];
    ]

let tutorial container =
  let overlay_hidden_s, set_overlay_hidden = S.create true in

  let close_button =
    El.button
      ~at:(classes "md:hidden text-blue-500 font-semibold")
      [ txt' "Close" ]
  in

  let clicked_op_in_text_e, send_clicked_op_in_text = E.create () in
  let example str =
    let elem = Brr.El.button ~at:(classes "underline") [ code [ txt' str ] ] in
    let evt = Note_brr.Evr.on_el Ev.click (fun _ -> ()) elem in
    (* XXX does this have the correct semantics? Event leak etc? *)
    Logr.may_hold
      E.(
        log evt (fun () ->
            send_clicked_op_in_text str;
            (* TODO: this will always show the overlay if the screen becomes smaller *)
            set_overlay_hidden false));
    elem
  in

  let logger, c_input, current_input =
    input' ~at:input_classes "a b, b c -> a c" clicked_op_in_text_e
  in
  Logr.may_hold logger;

  let parsed_input_signal =
    S.map
      (fun str -> str |> trim_before_colon |> Einsum_parser.Friendly.parse)
      current_input
  in

  let code_preference_signal, code_preference_selector =
    mk_code_preference_selector ()
  in

  let render_explanation rewrite code_preference =
    let edge_attributes = Colors.assign_edge_attributes (fst rewrite) in
    let get_color edge_name =
      match Hashtbl.find_opt edge_attributes edge_name with
      | None -> if Colors.prefers_dark () then "#fff" else "#000"
      | Some { Colors.color; _ } -> color
    in
    let _framework_name, framework_code_name =
      match code_preference with
      | Einops.Numpy -> ("Numpy", "np")
      | Pytorch -> ("Pytorch", "torch")
    in

    let pyloops = Einops.Explain.show_loops rewrite in
    let python_code, code_ppf = make_formatter () in
    Fmt.pf code_ppf "%a@?" (Einops.Pyloops.pp get_color) pyloops;
    let frob_python_code, frob_code_ppf = make_formatter () in
    Fmt.pf frob_code_ppf "%a@?"
      (Einops.Pyloops.pp ~use_frob:code_preference get_color)
      pyloops;
    let free_indices = String_set.to_list pyloops.free_indices in
    let summation_indices = String_set.to_list pyloops.summation_indices in
    let lhs, rhs = rewrite in
    let valid_isometric_tensors =
      List.for_all Isometric.Tensor.is_valid (rhs :: lhs)
    in
    [
      h2 ~at:(classes "text-xl mt-3 mb-1") [ txt' "Low-level: Python Code" ];
      p
        [
          txt'
            "In outer loops, we iterate over all free indices to generate each \
             output element (";
          span (list_variables get_color free_indices);
          txt' "), while in inner loops we iterate over all summation indices (";
          span (list_variables get_color summation_indices);
          txt'
            ") to sum over each product term. First, here's equivalent, \
             simplified (but slow, because it's not vectorized) Python code. \
             We initialize an empty ";
          code [ txt' "result" ];
          txt'
            " array and then iterate over every position in every axis, \
             building up the result.";
        ];
      code_preference_selector;
      div ~at:(classes "flex flex-row")
        [
          El.pre
            [
              code
                ~at:(classes "before:content-[''] after:content-['']")
                [ python_code ];
            ];
        ];
      p
        [
          txt'
            "In this next version of the code, we use the Frobenius product (";
          code [ txt' (Fmt.str "%s.sum(inputs)" framework_code_name) ];
          txt' ") instead of iterating over each summation index individually.";
        ];
      div ~at:(classes "flex flex-row")
        [
          El.pre
            [
              code
                ~at:(classes "before:content-[''] after:content-['']")
                [ frob_python_code ];
            ];
        ];
      h2 ~at:(classes "text-xl mt-3 mb-1") [ txt' "Medium-level" ];
      div
        (if valid_isometric_tensors then
           [ Isometric.Scene.render ~edge_attributes lhs rhs ]
         else
           [
             txt'
               "Isometric diagrams are only available if all tensors are of \
                dimension 3 or less.";
           ]);
      h2 ~at:(classes "text-xl mt-3 mb-1") [ txt' "High-level: tensor diagram" ];
      div
        [
          Tensor_diagram.draw_einsum edge_attributes lhs rhs;
          mk_tensor_diagram_info ();
        ];
    ]
  in

  let explanation_signal =
    S.l2
      (fun rewrite code_preference ->
        match rewrite with
        | Ok rewrite -> render_explanation rewrite code_preference
        | Error msg -> [ div ~at:(classes "text-red-600") [ txt' msg ] ])
      parsed_input_signal code_preference_signal
  in

  let a href text =
    El.a
      ~at:
        [
          At.v (Jstr.v "href") (Jstr.v href);
          At.v (Jstr.v "target") (Jstr.v "_blank");
        ]
      [ txt' text ]
  in

  let code' text = code [ txt' text ] in
  let content =
    El.div
      [
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
              "Though Einsums are easy to read and to understand on the level \
               of the shapes of the input and output tensors, I've found it \
               hard to understand what an einsum expression is actually doing. \
               That's why I've built this page, where we start from the basics \
               and build up to understanding what complex expressions actually \
               mean.";
          ];
        h2 [ txt' "Basic Operations" ];
        p
          [
            txt'
              "We'll start with five simple examples which illustrate all of \
               the principles you need to understand einsums.";
          ];
        h3 [ txt' "1. identity" ];
        p
          [
            txt'
              "An einsum expression names the axes involved in an operation. ";
            example "i -> i";
            txt'
              " takes a vector (the fact that a vector is one-dimensional \
               corresponds to the single index, ";
            code [ txt' "i" ];
            txt' ") and returns the same vector. Likewise ";
            example "i j -> i j";
            txt' " is the identity operation on matrices, and ";
            example "i j k -> i j k";
            txt' " is the identity operation on 3-dimensional tensors.";
          ];
        h3 [ txt' "2. transpose" ];
        p
          [
            txt' "The order vectors are written in matters. ";
            example "i j -> j i";
            txt' " swaps the positions of the ";
            code [ txt' "i" ];
            txt' " and ";
            code [ txt' "j" ];
            txt' " dimensions. ";
            code [ txt' "i" ];
            txt'
              " changes from the row dimension to the column dimension, and \
               likewise with ";
            code [ txt' "j" ];
            txt' ". So this operation is the transpose.";
          ];
        p
          [
            txt'
              "As an aside, you may wonder whether there's a way to reverse \
               the direction of a dimension. For example, reverse ";
            code [ txt' "<1 2 3>" ];
            txt' " to ";
            code [ txt' "<3 2 1>" ];
            txt' " But nope, einsum can't do this.";
          ];
        h3 [ txt' "3. sum" ];
        p
          [
            txt'
              "So far we've just rearranged tensors. You can also sum across a \
               dimension by omitting it from the result. So ";
            example "i ->";
            txt' " sums a vector. ";
            example "i j ->";
            txt' " sums a matrix. Can you explain what ";
            example "i j -> j";
            txt' " and ";
            example "i j -> i";
            txt' " do?";
          ];
        h4 [ txt' "Terminology: free and summation indices" ];
        p
          [
            txt' "In an example like ";
            example "i j -> j";
            txt' ", you may notice that the ";
            code [ txt' "i" ];
            txt' " and ";
            code [ txt' "j" ];
            txt' " indices act differently. ";
            code [ txt' "j" ];
            txt' " is an example of a free index, while ";
            code [ txt' "i" ];
            txt'
              " is an example of a summed index. Free indices appear in the \
               output specification, while summation indices don't.";
          ];
        p
          [
            txt'
              "On a low level, free indices are associated with outer loops, \
               one loop for each dimension of the output. While summation \
               indices are associated with inner loops, which are run for each \
               value in the output.";
          ];
        p
          [
            txt'
              "On a higher level, you can think of the distinction being about \
               which dimensions are preserved vs contracted.";
          ];
        h3 [ txt' "4. diagonal" ];
        p
          [
            txt'
              "If a dimension is repeated in an input tensor, we take a \
               diagonal (";
            example "i i -> i";
            txt'
              "). You can think of this as using the same variable to iterate \
               over both dimensions (";
            code [ txt' "[arr[i, i] for i in d_i]" ];
            txt'
              "). It's also possible to repeat an index three or more times (";
            example "i i i -> i";
            txt' ").";
          ];
        p
          [
            txt' "Repeating a dimension in an output (";
            example "i -> i i";
            txt' ") is invalid.";
          ];
        h3 [ txt' "5. element-wise product" ];
        p
          [
            txt'
              "There's one more basic operation. So far, we've been operating \
               on a single tensor, but an einsum can act on any number of \
               tensors. TODO explain the meaning of this";
          ];
        p [ example "i, i -> i" ];
        h1 [ txt' "Three ways to think about contractions" ];
        p
          [
            txt'
              "There are at least three ways of thinking about what an einsum \
               is doing, increasingly abstractly.";
          ];
        p
          [
            txt'
              "First, the lowest level, which is useful for defining the \
               operational semantics of einsum but inefficient to compute (see \
               Python code) and not very insightful:";
          ];
        El.ul
          [
            El.li
              [
                txt'
                  "Classify each index as free (if it occurs in the output) or \
                   summation (if not).";
              ];
            El.li
              [
                txt'
                  "The free indices define the shape of the output (this gives \
                   rise to a loop over each free index in the code).";
              ];
            El.li
              [
                txt'
                  "For each cell in the output, loop over each summation \
                   index. This gives a unique location in each input tensor. \
                   You multiply the values in each tensor and add this product \
                   to the current cell.";
              ];
          ];
        p [ txt' "Second, on a higher level (see tensor diagram):" ];
        El.ul
          [
            El.li
              [
                txt'
                  "For each input tensor with repeated indices, extract the \
                   values on the diagonal, producing a new tensor of smaller \
                   dimension. Now each input tensor has uniquely named \
                   dimensions.";
              ];
            El.li
              [
                txt'
                  "Broadcast as necessary to make each input tensor the same \
                   shape.";
              ];
            El.li
              [
                txt'
                  "Pointwise multiply each tensor, giving a single tensor with \
                   maximal dimension (it has one axis for each unique input \
                   dimension).";
              ];
            El.li
              [
                txt'
                  "Contract all summation indices (those which don't appear in \
                   the output) by summing across them.";
              ];
          ];
        p
          [
            txt'
              "Finally, on the highest level, its best to think of dimensions \
               as fulfilling three different roles:";
          ];
        El.ul
          [
            El.li [ txt' "Input to a computation" ];
            El.li [ txt' "Output from a computation" ];
            El.li
              [ txt' "Different pieces of data being operated on in parallel" ];
          ];
        p
          [
            txt'
              "Because tensors are just n-dimensional grids of numbers, we \
               can't look at a tensor labeled ";
            code [ txt' "a b c" ];
            txt'
              " and say what each dimension represents. But given a tensor \
               labeled ";
            code [ txt' "batch d_in d_out" ];
            txt'
              " we can make a pretty good guess. In fact, those three \
               dimensions correspond to the three roles I just mentioned \
               (batch is a typical name for different pieces of data processed \
               in parallel).";
          ];
        p
          [
            txt'
              "When you see two tensors contracted along some dimension, the \
               typical interpretation is data flow, and the prototypical \
               examples are the matrix-vector product (applying some \
               transformation to data: ";
            example "a b, a -> b";
            txt' ") and matrix multiplication (composing two functions: ";
            example "a b, b c -> a c";
            txt' ").";
          ];
        p
          [
            txt' "Unfortunately, when you see a matrix with type ";
            code' "a b";
            txt' ", that's not enough information to say whether ";
            code' "a";
            txt' " is an input and ";
            code' "b";
            txt'
              " is an output or vice-versa. The convention in math is for that \
               shape of matrix to be interpreted as a function ";
            code' "b -> a";
            txt'
              ", and you multiply it on the left, but it sometimes happens \
               that it's more convenient for ";
            code' "a b";
            txt' " to be interpreted as a function ";
            code' "a -> b";
            txt' " and multiplied on the right.";
          ];
        p
          [
            txt'
              "In physics there's no ambiguity because physicists use special \
               notation to distinguish outputs and inputs (or what they call \
               co / contravariant). In physics, a contraction happens by \
               pairing a covariant and a contravariant index. This ensures \
               that you're connecting an output (or data) (covariant) to an \
               input (contravariant).";
          ];
        p
          [
            txt'
              "Above I classified dimensions in three ways: input, output, or \
               different pieces of data. Using the co / contravariant lens, we \
               can see that it's equally valid to think of just two \
               categories, where inputs are contravariant, and both outputs \
               and different pieces of data are covariant.";
          ];
        p
          [
            txt'
              "I suspect that in practice, people often approach tensor \
               contractions with an \"if it compiles, it works\" attitude. You \
               dump all of the tensors you need in, and tell einsum the shape \
               you want out, and it tends to do the right thing.";
          ];
        h1 [ txt' "Examples" ];
        p
          [
            txt'
              "Now that we've defined what einsums are doing and given basic \
               examples, here are some examples you might come across in \
               practice.";
          ];
        h2 [ txt' "Linear Algebra" ];
        El.ul
          [
            El.li [ example "a b, b c -> a c"; txt' " (matmul)" ];
            El.li [ example "a b, b -> a"; txt' " (matrix-vector product)" ];
            El.li [ example "i i ->"; txt' " (trace)" ];
            El.li [ example "i j -> j"; txt' " (column sum)" ];
            El.li [ example "i j -> i"; txt' " (row sum)" ];
            El.li [ example "i, i ->"; txt' " (inner product)" ];
            El.li [ example "i j, i j -> i j"; txt' " (Hadamard product)" ];
            El.li [ example "i, j -> i j"; txt' " (outer product)" ];
            El.li
              [
                example "i j, j k l -> i j";
                txt' " (bilinear transformation, see ";
                a "https://rockt.ai/2018/04/30/einsum" "Tim Rocktäschel's post";
                txt' ")";
              ];
          ];
        h2 [ txt' "Machine Learning" ];
        El.ul
          [
            El.li
              [
                example "batch i j, batch j k -> batch i k";
                txt' " (Batch matrix multiplication)";
              ];
            El.li
              [
                txt' "Attention:";
                El.ul
                  [
                    El.li
                      [
                        example
                          "batch head_index q_pos d_model, batch head_index \
                           k_pos d_model -> batch head_index q_pos k_pos";
                      ];
                    El.li
                      [
                        example
                          "batch pos head_index d_model, head_index d_model \
                           d_head -> batch pos head_index d_head";
                        txt' " (see ";
                        a
                          "https://github.com/TransformerLensOrg/TransformerLens/blob/3267a43ebcd7e42121fc1c568568165b073f4a9f/transformer_lens/utilities/attention.py"
                          "source code";
                        txt' ")";
                      ];
                  ];
              ];
          ];
        h2 [ txt' "Other Resources" ];
        El.ul
          [
            El.li
              [
                a
                  "https://obilaniu6266h16.wordpress.com/2016/02/04/einstein-summation-in-numpy/"
                  "Olexa Bilaniuk's Einstein Summation in Numpy";
              ];
            El.li
              [
                a "https://ajcr.net/Basic-guide-to-einsum/"
                  "ajcr's A basic introduction to NumPy's einsum";
              ];
            El.li
              [
                a
                  "https://stackoverflow.com/questions/72952005/how-is-numpy-einsum-implemented"
                  "Stack Overflow: How is numpy.einsum implemented?";
              ];
            El.li
              [
                a "https://rockt.ai/2018/04/30/einsum"
                  "Einsum is All you Need - Einstein Summation in Deep Learning";
              ];
          ];
      ]
  in

  (* Left pane: essay list *)
  let left_pane =
    El.div
      ~at:
        (classes
           "flex-1 md:w-1/2 w-full overflow-y-auto border-r p-4 prose \
            dark:prose-invert prose-p:my-2 prose-pre:bg-gray-100 \
            prose-pre:dark:bg-[#00000080] prose-pre:text-gray-900 \
            prose-pre:dark:text-[#d1d5db]")
      [ content ]
  in

  (* On mobile, the right pane is a full-screen collapsible overlay.
     On desktop, it's displayed side-by-side. *)
  let result_output = div [] in
  let right_pane =
    El.div
      ~at:
        ((* On desktop: static position, normal layout *)
         (* On mobile: fixed overlay hidden by default (translate-y-full),
            can be toggled via JS to show/hide *)
         classes
           "flex-1 md:w-1/2 w-full overflow-y-auto p-4 bg-gray-50 \
            dark:bg-gray-800 md:static md:translate-y-0 md:relative fixed \
            bottom-0 left-0 right-0 top-0 transform transition-transform \
            duration-300 ease-in-out md:border-none border-t border-gray-300 \
            z-50")
        (* translate-y-full *)
      [
        El.div
          ~at:(classes "flex items-center justify-between border-b pb-2 mb-4")
          [
            El.h2 ~at:(classes "text-2xl") [ txt' "Explanation" ]; close_button;
          ];
        El.p [ txt' "Click on an example to see its explanation here." ];
        div
          [
            txt' "Or enter your own contraction: ";
            c_input;
            info
              (div
                 [
                   txt' "We use ";
                   a "https://einops.rocks/" "einops notation";
                   txt'
                     ", not the notation built in to Numpy and Pytorch. Note \
                      that einops notation supports parenthesis and ellipsis \
                      notation, but we don't.";
                 ]);
          ];
        result_output;
      ]
  in

  Elr.def_children result_output explanation_signal;

  Elr.def_class (Jstr.v "translate-y-full") overlay_hidden_s right_pane;

  (* Hide overlay when close button is clicked *)
  let _hide_listener =
    Ev.listen Ev.click
      (fun _ ->
        set_overlay_hidden true (* El.add_class "translate-y-full" right_pane *))
      (El.as_target close_button)
  in

  let wrapper =
    El.div
      ~at:(classes "relative flex flex-col md:flex-row h-screen")
      [ left_pane; right_pane ]
  in

  set_children container [ wrapper ]
