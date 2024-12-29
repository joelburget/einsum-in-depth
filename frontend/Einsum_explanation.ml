open Brr
open Note_brr
open Frontend_util
open Note
open Tensor_playground

type validated_inputs = unit

let code' text = code [ txt' text ]

let text_button_classes =
  classes "not-prose text-indigo-500 dark:text-indigo-200 hover:text-gray-700"

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
              | class_name, q ->
                  q |> Queue.to_seq |> List.of_seq
                  |> span ~at:(classes class_name)
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
            self-center justify-self-end fill-gray-500"
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

module Icons = struct
  open Brr_svg
  open El

  let website =
    svg
      ~at:
        ([
           fill "none";
           viewbox "0 0 24 24";
           svg_at "stroke-width" "1.5";
           svg_at "stroke" "currentColor";
         ]
        @ svg_classes "size-5 inline")
      [
        path
          ~at:
            [
              svg_at "stroke-linecap" "round";
              svg_at "stroke-linejoin" "round";
              d
                "M12 21a9.004 9.004 0 0 0 8.716-6.747M12 21a9.004 9.004 0 0 \
                 1-8.716-6.747M12 21c2.485 0 4.5-4.03 4.5-9S14.485 3 12 3m0 \
                 18c-2.485 0-4.5-4.03-4.5-9S9.515 3 12 3m0 0a8.997 8.997 0 0 1 \
                 7.843 4.582M12 3a8.997 8.997 0 0 0-7.843 4.582m15.686 \
                 0A11.953 11.953 0 0 1 12 10.5c-2.998 \
                 0-5.74-1.1-7.843-2.918m15.686 0A8.959 8.959 0 0 1 21 12c0 \
                 .778-.099 1.533-.284 2.253m0 0A17.919 17.919 0 0 1 12 \
                 16.5c-3.162 0-6.133-.815-8.716-2.247m0 0A9.015 9.015 0 0 1 3 \
                 12c0-1.605.42-3.113 1.157-4.418";
            ]
          [];
      ]

  let x =
    svg
      ~at:(viewbox "0 0 300 271" :: svg_classes "size-5 inline")
      [
        path
          ~at:
            [
              d
                "m236 0h46l-101 115 118 156h-92.6l-72.5-94.8-83 \
                 94.8h-46l107-123-113-148h94.9l65.5 86.6zm-16.1 \
                 244h25.5l-165-218h-27.4z";
            ]
          [];
      ]

  let github =
    svg
      ~at:(viewbox "0 0 98 96" :: svg_classes "size-5 inline")
      [
        path
          ~at:
            [
              fill "#24292f";
              svg_at "fill-rule" "evenodd";
              svg_at "clip-rule" "evenodd";
              d
                "M48.854 0C21.839 0 0 22 0 49.217c0 21.756 13.993 40.172 \
                 33.405 46.69 2.427.49 3.316-1.059 3.316-2.362 \
                 0-1.141-.08-5.052-.08-9.127-13.59 \
                 2.934-16.42-5.867-16.42-5.867-2.184-5.704-5.42-7.17-5.42-7.17-4.448-3.015.324-3.015.324-3.015 \
                 4.934.326 7.523 5.052 7.523 5.052 4.367 7.496 11.404 5.378 \
                 14.235 4.074.404-3.178 1.699-5.378 \
                 3.074-6.6-10.839-1.141-22.243-5.378-22.243-24.283 0-5.378 \
                 1.94-9.778 5.014-13.2-.485-1.222-2.184-6.275.486-13.038 0 0 \
                 4.125-1.304 13.426 5.052a46.97 46.97 0 0 1 12.214-1.63c4.125 \
                 0 8.33.571 12.213 1.63 9.302-6.356 13.427-5.052 13.427-5.052 \
                 2.67 6.763.97 11.816.485 13.038 3.155 3.422 5.015 7.822 5.015 \
                 13.2 0 18.905-11.404 23.06-22.324 24.283 1.78 1.548 3.316 \
                 4.481 3.316 9.126 0 6.6-.08 11.897-.08 13.526 0 1.304.89 \
                 2.853 3.316 2.364 19.412-6.52 33.405-24.935 \
                 33.405-46.691C97.707 22 75.788 0 48.854 0z";
            ]
          [];
      ]
end

type syntax_preference = Friendly | Original

let tutorial container =
  let overlay_hidden_s, set_overlay_hidden = S.create true in
  let syntax_preference_s, set_syntax_preference = S.create None in

  let close_button =
    El.button
      ~at:(classes "md:hidden text-blue-500 font-semibold")
      [ txt' "Close" ]
  in

  let clicked_op_in_text_e, send_clicked_op_in_text = E.create () in
  let example friendly_spec =
    let f = function
      | Some Friendly | None -> friendly_spec
      | Some Original -> (
          match Einsum_parser.Original.parse friendly_spec with
          | Error _ -> friendly_spec (* Should never happen *)
          | Ok parsed ->
              parsed |> Einops.Rewrite.to_original
              |> Fmt.to_to_string Einops.Rewrite.pp_original)
    in
    let contents_s =
      syntax_preference_s |> S.map (fun preference -> [ code' (f preference) ])
    in
    let button = Brr.El.button ~at:text_button_classes [] in
    Elr.def_children button contents_s;
    let evt = Note_brr.Evr.on_el Ev.click (fun _ -> ()) button in
    (* XXX does this have the correct semantics? Event leak etc? *)
    Logr.may_hold
      E.(
        log evt (fun () ->
            send_clicked_op_in_text (f (S.value syntax_preference_s));
            (* TODO: this will always show the overlay if the screen becomes smaller *)
            set_overlay_hidden false));
    button
  in

  let logger, c_input, current_input =
    input' ~at:input_classes "a b, b c -> a c" clicked_op_in_text_e
  in
  Logr.may_hold logger;

  let parsed_input_signal =
    S.map
      (fun str -> str |> trim_before_colon |> Einsum_parser.parse)
      current_input
  in

  let code_preference_signal, code_preference_selector =
    mk_code_preference_selector ()
  in

  let interpretation_info ~valid_as_both =
    p
      [
        txt'
          (if valid_as_both then "This input is valid as either "
           else "We accept either ");
        a "https://einops.rocks/#why-use-einops-notation" "Einops notation";
        txt' " or as ";
        El.a
          ~at:
            [
              At.href
                (Jstr.v
                   "https://numpy.org/doc/stable/reference/generated/numpy.einsum.html");
            ]
          [ code' "numpy.einsum" ];
        txt' " notation (which is the same as ";
        El.a
          ~at:
            [
              At.href
                (Jstr.v
                   "https://pytorch.org/docs/stable/generated/torch.einsum.html");
            ]
          [ code' "torch.einsum" ];
        txt' ")";
      ]
  in

  let interpreted_as_friendly_message =
    let button =
      El.button ~at:text_button_classes
        [ txt' "Switch to "; code' "numpy.einsum" ]
    in
    let click_button_event =
      Evr.on_el Ev.click
        (fun evt ->
          Ev.stop_propagation evt;
          ())
        button
    in
    Logr.may_hold
      E.(
        log click_button_event (fun () -> set_syntax_preference (Some Original)));
    div
      [
        interpretation_info ~valid_as_both:true;
        p [ txt' "We've interpreted it as Einops notation." ];
        button;
      ]
  in
  let interpreted_as_original_message =
    let button =
      El.button ~at:text_button_classes [ txt' "Switch to Einops" ]
    in
    let click_button_event =
      Evr.on_el Ev.click
        (fun evt ->
          Ev.stop_propagation evt;
          ())
        button
    in
    Logr.may_hold
      E.(
        log click_button_event (fun () -> set_syntax_preference (Some Friendly)));
    div
      [
        interpretation_info ~valid_as_both:true;
        p
          [
            txt' "We've interpreted it as ";
            code' "numpy.einsum";
            txt' " notation.";
          ];
        button;
      ]
  in

  let render_explanation rewrite code_preference _syntax_preference =
    let edge_attributes = Colors.assign_edge_attributes (fst rewrite) in
    let text_color edge_name =
      match Hashtbl.find_opt edge_attributes edge_name with
      | None -> ""
      | Some Colors.{ text_classes; _ } -> text_classes
    in
    let _framework_name, framework_code_name =
      match code_preference with
      | Einops.Numpy -> ("Numpy", "np")
      | Pytorch -> ("Pytorch", "torch")
    in

    let pyloops = Einops.Explain.show_loops rewrite in
    let python_code, code_ppf = make_formatter () in
    Fmt.pf code_ppf "%a@?" (Einops.Pyloops.pp text_color) pyloops;
    let frob_python_code, frob_code_ppf = make_formatter () in
    Fmt.pf frob_code_ppf "%a@?"
      (Einops.Pyloops.pp ~use_frob:code_preference text_color)
      pyloops;
    let free_indices = String_set.to_list pyloops.free_indices in
    let summation_indices = String_set.to_list pyloops.summation_indices in
    let lhs, rhs = rewrite in
    [
      h2 ~at:(classes "text-xl mt-3 mb-1") [ txt' "Low-level: Python Code" ];
      p
        [
          txt'
            "In outer loops, we iterate over all free indices to generate each \
             output element (";
          span (list_variables text_color free_indices);
          txt' "), while in inner loops we iterate over all summation indices (";
          span (list_variables text_color summation_indices);
          txt'
            ") to sum over each product term. First, here's equivalent, \
             simplified (but slow, because it's not vectorized) Python code. \
             We initialize an empty ";
          code' "result";
          txt'
            " array and then iterate over every position in every axis, \
             building up the result.";
        ];
      code_preference_selector;
      div
        ~at:(classes "flex flex-row my-4")
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
          code' (Fmt.str "%s.sum(inputs)" framework_code_name);
          txt' ") instead of iterating over each summation index individually.";
        ];
      div
        ~at:(classes "flex flex-row my-4")
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
        (match Isometric.Scene.render ~edge_attributes lhs rhs with
        | Some render -> [ render ]
        | None ->
            [
              txt'
                "Isometric diagrams are only available if all tensors (and all \
                 intermediate tensors, which may be bigger than all inputs and \
                 the output!) are of dimension 3 or less.";
            ]);
      h2
        ~at:(classes "text-xl mt-3 mb-1")
        [ txt' "High-level: tensor diagram"; mk_tensor_diagram_info () ];
      div [ Tensor_diagram.draw_einsum edge_attributes lhs rhs ];
    ]
  in

  let explanation_message_s =
    S.l3
      (fun rewrites code_preference syntax_preference ->
        let explanation, opt_syntax_preference_message =
          match rewrites with
          | Ok friendly_rewrite, Ok original_rewrite -> (
              match syntax_preference with
              | Some Friendly | None ->
                  ( render_explanation friendly_rewrite code_preference
                      syntax_preference,
                    Some interpreted_as_friendly_message )
              | Some Original ->
                  ( render_explanation original_rewrite code_preference
                      syntax_preference,
                    Some interpreted_as_original_message ))
          | Ok rewrite, Error _ ->
              ( render_explanation rewrite code_preference syntax_preference,
                None )
          | Error _, Ok rewrite ->
              ( render_explanation rewrite code_preference syntax_preference,
                None )
          | Error msg, _ ->
              ([ div ~at:(classes "text-red-600") [ txt' msg ] ], None)
        in
        let syntax_preference_message =
          match opt_syntax_preference_message with
          | Some msg -> msg
          | None -> interpretation_info ~valid_as_both:false
        in
        (explanation, syntax_preference_message))
      parsed_input_signal code_preference_signal syntax_preference_s
  in

  let a href text =
    El.a
      ~at:
        [
          At.v (Jstr.v "href") (Jstr.v href);
          At.v (Jstr.v "target") (Jstr.v "_blank");
          At.class' (Jstr.v "underline");
        ]
      [ txt' text ]
  in

  let broadcast_info =
    div
      [
        txt'
          "Broadcasting is a technique used in numpy and other libraries to \
           allow operations on arrays of different shapes. For example, if you \
           have a 3x3 matrix and a 3x1 vector, numpy will automatically expand \
           the vector to a 3x3 matrix by repeating the vector along the second \
           axis. ";
      ]
  in

  let content =
    El.div
      [
        h1 [ txt' "Einsum Tutorial" ];
        p
          [
            a "https://en.wikipedia.org/wiki/Einstein_notation"
              "Einsum (Einstein summation) notation";
            txt'
              " is a compact and intuitive way to write many linear algebra \
               operations: matrix multiplication, dot / Frobenius product, \
               transpose, trace, as well as many more complex operations which \
               don't have a name.";
          ];
        p
          [
            txt'
              "Though Einsums are easy to read and to understand on the level \
               of the shapes of the input and output tensors, I initially \
               found it hard to understand what an einsum expression was \
               actually doing. That's why I built this page, where we start \
               from the basics and build up to understanding what complex \
               expressions actually mean.";
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
            code' "i";
            txt' ") and returns the same vector. Likewise ";
            example "i j -> i j";
            txt' " is the identity operation on matrices, and ";
            example "i j k -> i j k";
            txt' " is the identity operation on 3-dimensional tensors.";
          ];
        h3 [ txt' "2. transpose" ];
        p
          [
            txt' "The ordering of labels matters. ";
            example "i j -> j i";
            txt' " swaps the positions of the ";
            code' "i";
            txt' " and ";
            code' "j";
            txt' " dimensions. ";
            code' "i";
            txt'
              " changes from the row dimension to the column dimension, and \
               likewise with ";
            code' "j";
            txt' ". So this operation is the transpose.";
          ];
        p
          [
            txt'
              "As an aside, you may wonder whether there's a way to reverse \
               the direction of a dimension. For example, reverse ";
            code' "<1 2 3>";
            txt' " to ";
            code' "<3 2 1>";
            txt' ". But einsums can't do this.";
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
            code' "i";
            txt' " and ";
            code' "j";
            txt' " indices act differently. ";
            code' "j";
            txt' " is an example of a free index, while ";
            code' "i";
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
            code' "[arr[i, i] for i in d_i]";
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
               tensors. ";
            example "i, i -> i";
            txt' " is the element-wise product of two vectors. Likewise, ";
            example "i j, i j -> i j";
            txt'
              " is the element-wise product of two matrices. In general, we \
               can always combine two tensors of the same shape in this way. \
               This is called the ";
            a "https://en.wikipedia.org/wiki/Hadamard_product_(matrices)"
              "Hadamard product";
            txt' ".";
          ];
        h1 [ txt' "Three ways to think about contractions" ];
        p
          [
            txt'
              "There are at least three ways of thinking about what an einsum \
               is doing, at increasing levels of abstraction.";
          ];
        p
          [
            txt'
              "First, at the lowest level, which is useful for defining the \
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
        p
          [
            txt' "Second, on the middle level (see isometric diagrams):";
            info
              (p
                 [
                   txt'
                     "I hadn't previously seen this interpretation, so I wrote \
                      a ";
                   a
                     "https://github.com/joelburget/einsum-tutorial/blob/main/verify_consistency.py"
                     "script";
                   txt' " to confirm that it is consistent with the other two.";
                 ]);
          ];
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
                txt' "Broadcast";
                info broadcast_info;
                txt'
                  " and reorder as necessary to make each input tensor the \
                   same shape.";
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
              "Concisely: diagonalize, make the same shape (broadcast / \
               reorder), align, and contract.";
          ];
        p
          [
            txt'
              "Finally, on the highest level, I think of dimensions as \
               fulfilling three different roles:";
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
            code' "a b c";
            txt'
              " and say what each dimension represents. But given a tensor \
               labeled ";
            code' "batch d_in d_out";
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
        h3 [ txt' "Bonus: How Contractions Are Actually Computed" ];
        p
          [
            txt'
              "A group of tensors is reduced one pair at a time. The result \
               will be the same no matter which order tensors were reduced in, \
               but different orders can have vastly different performance \
               characteristics. ";
            a "https://github.com/dgasmith/opt_einsum" "opt_einsum";
            txt'
              " shows an example where the same reduction takes 3000x longer \
               without optimization.";
          ];
        p
          [
            txt'
              "The general problem of finding the optimal path is NP-hard (the \
               number of possible ways of reducing n tensors ";
            a "https://en.wikipedia.org/wiki/Catalan_number" "grows";
            txt' " ";
            a "https://en.wikipedia.org/wiki/Matrix_chain_multiplication"
              "quickly";
            txt' ". See the ";
            a "https://dgasmith.github.io/opt_einsum/paths/introduction/"
              "opt_einsum docs";
            txt' " for more.";
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
            El.li [ example "a b, b c -> a c"; txt' " (matrix multiplication)" ];
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
        p
          ~at:(classes "flex flex-row gap-4 mt-4")
          [
            span
              [
                txt' "Built by ";
                El.a
                  ~at:
                    [
                      At.href (Jstr.v "https://joelburget.com");
                      At.v (Jstr.v "target") (Jstr.v "_blank");
                    ]
                  [ txt' "Joel Burget"; embed_svg Icons.website ];
              ];
            El.a
              ~at:
                [
                  At.href (Jstr.v "https://x.com/joel_burget");
                  At.v (Jstr.v "target") (Jstr.v "_blank");
                ]
              [ embed_svg Icons.x ];
            El.a
              ~at:
                [
                  At.href
                    (Jstr.v "https://github.com/joelburget/einsum-tutorial");
                  At.v (Jstr.v "target") (Jstr.v "_blank");
                ]
              [ embed_svg Icons.github ];
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
  let result_container, syntax_message_container = (div [], div []) in
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
        syntax_message_container;
        result_container;
      ]
  in

  let explanation_signal = S.Pair.fst explanation_message_s in
  let syntax_preference_message_s = S.Pair.snd explanation_message_s in

  Elr.def_children result_container explanation_signal;
  Elr.def_children syntax_message_container
    (S.map
       (fun msg ->
         [ txt' "Or enter your own contraction: "; c_input; info msg ])
       syntax_preference_message_s);

  (* Overlay: click on the left pane to show the right pane *)
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
