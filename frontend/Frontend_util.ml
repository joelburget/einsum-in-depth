open Tensor_playground
open Brr
open Note_brr
open Note
module String_set = Set.Make (String)
module String_map = Map.Make (String)

type direction = Horizontal | Vertical

let ( code,
      set_children,
      input,
      prop,
      as_target,
      div,
      span,
      txt',
      table,
      tbody,
      td,
      tr,
      p,
      ol,
      ul,
      li,
      h1,
      h2,
      nav ) =
  El.
    ( code,
      set_children,
      input,
      prop,
      as_target,
      div,
      span,
      txt',
      table,
      tbody,
      td,
      tr,
      p,
      ol,
      ul,
      li,
      h1,
      h2,
      nav )

let embed_svg : Brr_svg.El.t -> Brr.El.t = Obj.magic
let svg_at name value = Brr_svg.At.v (Jstr.v name) (Jstr.v value)
let at name value = At.v (Jstr.v name) (Jstr.v value)
let txt_td str = td [ txt' str ]
let class_ str = At.class' (Jstr.of_string str)
let classes str = str |> String.split_on_char ' ' |> List.map class_
let svg_class str = Brr_svg.At.class' (Jstr.of_string str)
let svg_classes str = str |> String.split_on_char ' ' |> List.map svg_class
let type' str = At.type' (Jstr.of_string str)
let d str = Brr_svg.At.d (Jstr.of_string str)
let viewbox str = Brr_svg.At.viewbox (Jstr.of_string str)
let fill str = Brr_svg.At.fill (Jstr.of_string str)

let parse_type str =
  match Type_parser_runner.parse Type_parser.Incremental.lax str with
  | Ok (ty, bracketed) -> Ok (ty, bracketed)
  | Error (_location, indication, message) -> Error (indication, message)

let fmt_txt : type a. (a, Format.formatter, unit, El.t) format4 -> a =
 fun fmt -> Fmt.kstr (fun s -> El.txt' s) fmt

let bracketed_input bracketed_s elem =
  let lbracket = El.span [] in
  let rbracket = El.span [] in
  let f str = function
    | Tensor_type.Unbracketed -> [ El.txt' str ]
    | Bracketed -> []
  in
  Elr.def_children lbracket (S.map (f "[") bracketed_s);
  Elr.def_children rbracket (S.map (f "]") bracketed_s);
  El.(span [ lbracket; elem; rbracket ])

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

let input' :
    ?at:At.t list ->
    string ->
    string event ->
    Logr.t option * El.t * string signal =
 fun ?(at = []) start_value external_update_input ->
  let input_elem = input ~at () in
  let input_signal, set_input = S.create start_value in
  Evr.endless_listen (as_target input_elem) Ev.change (fun _evt ->
      set_input (Jstr.to_string (prop El.Prop.value input_elem)));
  Elr.set_prop El.Prop.value
    ~on:(E.map Jstr.of_string external_update_input)
    input_elem;
  (E.log external_update_input set_input, input_elem, input_signal)

let input_classes =
  classes
    "rounded-md border-0 px-3 py-1.5 text-gray-900 shadow-sm ring-1 ring-inset \
     ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset \
     focus:ring-indigo-600 sm:text-sm sm:leading-6"

let parsed_input :
    type a.
    (string -> (a, string * string) result) ->
    string ->
    a option signal * El.t * El.t =
 fun parse start_value ->
  let input_elem =
    input ~at:([ At.value (Jstr.of_string start_value) ] @ input_classes) ()
  in
  let parse_error_elem = div (* ~at:(classes "inline") *) [] in

  let input_signal, set_input = S.create start_value in
  let parse_error_signal, set_parse_error = S.create [] in
  let parsed_signal, set_parsed = S.create None in

  Evr.endless_listen (as_target input_elem) Ev.change (fun _evt ->
      set_input (Jstr.to_string (prop El.Prop.value input_elem)));

  let output_logger =
    S.log input_signal (fun str ->
        match parse str with
        | Result.Ok t ->
            set_parsed (Some t);
            set_parse_error []
        | Error (msg1, msg2) ->
            set_parsed None;
            set_parse_error [ txt' msg1; txt' msg2 ])
  in
  Logr.hold output_logger;

  Elr.def_children parse_error_elem parse_error_signal;
  (parsed_signal, input_elem, parse_error_elem)

let bracketed_parsed_input :
    type a.
    (string -> (a * Tensor_type.bracketed, string * string) result) ->
    string ->
    a option signal * El.t * El.t =
 fun parse start_value ->
  let bracket_signal, set_bracket = S.create Tensor_type.Unbracketed in
  let out_signal, input_elem, err_elem = parsed_input parse start_value in
  let out_signal =
    S.map
      (Option.map (fun (t, bracketed) ->
           set_bracket bracketed;
           t))
      out_signal
  in
  let bracketed_input_elem = bracketed_input bracket_signal input_elem in
  (out_signal, bracketed_input_elem, err_elem)

let result_list : ('a, 'e) result list -> ('a list, 'e) result =
 fun results ->
  let rec loop results =
    match results with
    | [] -> Ok []
    | Ok x :: results -> (
        match loop results with Ok xs -> Ok (x :: xs) | Error e -> Error e)
    | Error e :: _ -> Error e
  in
  loop results

module Select_and_info : sig
  val select : string list -> El.t * string signal
  val info : El.t -> El.t
end = struct
  let chevrons_svg =
    let open Brr_svg in
    let open El in
    svg
      ~at:
        (svg_classes "h-5 w-5 text-gray-400"
        @ [ svg_at "aria-hidden" "true" ]
        @ [ viewbox "0 0 20 20"; fill "currentColor" ])
      [
        path
          ~at:
            ([
               d
                 "M10 3a.75.75 0 01.55.24l3.25 3.5a.75.75 0 11-1.1 1.02L10 \
                  4.852 7.3 7.76a.75.75 0 01-1.1-1.02l3.25-3.5A.75.75 0 0110 \
                  3zm-3.76 9.2a.75.75 0 011.06.04l2.7 2.908 2.7-2.908a.75.75 0 \
                  111.1 1.02l-3.25 3.5a.75.75 0 01-1.1 0l-3.25-3.5a.75.75 0 \
                  01.04-1.06z";
             ]
            @ [ svg_at "fill-rule" "evenodd"; svg_at "clip-rule" "evenodd" ])
          [];
      ]

  let checkmark_svg () =
    let open Brr_svg in
    let open El in
    svg
      ~at:
        (svg_classes "h-5 w-5"
        @ [ svg_at "aria-hidden" "true" ]
        @ [ viewbox "0 0 20 20"; fill "currentColor" ])
      [
        path
          ~at:
            ([
               d
                 "M16.704 4.153a.75.75 0 01.143 1.052l-8 10.5a.75.75 0 \
                  01-1.127.075l-4.5-4.5a.75.75 0 011.06-1.06l3.894 3.893 \
                  7.48-9.817a.75.75 0 011.05-.143z";
             ]
            @ [ svg_at "fill-rule" "evenodd"; svg_at "clip-rule" "evenodd" ])
          [];
      ]

  let info_svg () =
    let open Brr_svg in
    let open El in
    svg
      ~at:
        (svg_classes "h-4 w-4"
        @ [ svg_at "aria-hidden" "true" ]
        @ [ viewbox "0 0 24 24"; fill "currentColor" ])
      [
        path
          ~at:
            [
              d
                "M 12 2 C 6.4889971 2 2 6.4889971 2 12 C 2 17.511003 6.4889971 \
                 22 12 22 C 17.511003 22 22 17.511003 22 12 C 22 6.4889971 \
                 17.511003 2 12 2 z M 12 4 C 16.430123 4 20 7.5698774 20 12 C \
                 20 16.430123 16.430123 20 12 20 C 7.5698774 20 4 16.430123 4 \
                 12 C 4 7.5698774 7.5698774 4 12 4 z M 11 7 L 11 9 L 13 9 L 13 \
                 7 L 11 7 z M 11 11 L 11 17 L 13 17 L 13 11 L 11 11 z";
            ]
          [];
      ]

  type button_click_event = ButtonClick
  type click_outside_event = ClickOutside

  let info child =
    let dropdown_open_signal, set_dropdown_open = S.create false in
    let click_outside_event =
      Evr.on_el Ev.click (fun _ -> ClickOutside) (Document.body G.document)
    in

    let click_child_event =
      Evr.on_el Ev.click
        (fun evt ->
          (* Prevent click_outside_event from triggering *)
          Ev.stop_propagation evt)
        child
    in

    let button =
      El.button
        ~at:[ type' "button" ]
        [ span ~at:(classes "inline-flex") [ embed_svg (info_svg ()) ] ]
    in
    let click_button_event =
      Evr.on_el Ev.click
        (fun evt ->
          Ev.stop_propagation evt;
          (* Prevent click_outside_event from triggering *)
          ButtonClick)
        button
    in

    Logr.may_hold
      E.(
        log click_button_event (fun ButtonClick ->
            set_dropdown_open (not (S.value dropdown_open_signal))));
    Logr.may_hold
      E.(log click_outside_event (fun ClickOutside -> set_dropdown_open false));
    Logr.may_hold E.(log click_child_event (fun _ -> ()));

    let result_elem =
      El.div ~at:(classes "inline align-middle relative not-prose") []
    in
    Elr.def_children result_elem
      (dropdown_open_signal
      |> S.map (fun open' ->
             if open' then
               [
                 button;
                 div
                   ~at:(classes "absolute z-10 flex w-screen max-w-max px-4")
                   [
                     div
                       ~at:
                         (classes
                            "w-screen max-w-md flex-auto overflow-hidden \
                             rounded-md bg-white dark:bg-slate-800 text-sm/6 \
                             shadow-lg ring-1 ring-gray-900/5 p-3")
                       [ child ];
                   ];
               ]
             else [ button ]));
    result_elem

  let select items =
    let selected_signal, set_selected = S.create 0 in
    let selected_item = selected_signal |> S.map (fun i -> List.nth items i) in
    let dropdown_open_signal, set_dropdown_open = S.create false in

    let button =
      let selected_span = span ~at:(classes "block truncate") [] in
      Elr.def_children selected_span
        (selected_item |> S.map (fun i -> [ txt' i ]));
      El.button
        ~at:
          ([
             type' "button";
             at "aria-haspopup" "listbox";
             at "aria-expanded" "true";
             at "aria-labelledby" "listbox-label";
           ]
          @ classes
              "relative w-full cursor-default rounded-md bg-white py-1.5 pl-3 \
               pr-10 text-left text-gray-900 shadow-sm ring-1 ring-inset \
               ring-gray-300 focus:outline-none focus:ring-2 \
               focus:ring-indigo-600 sm:text-sm sm:leading-6 inline-flex")
        El.
          [
            span
              ~at:
                (classes
                   "pointer-events-none absolute inset-y-0 right-0 flex \
                    items-center pr-2")
              [ embed_svg chevrons_svg ];
            selected_span;
          ]
    in
    let click_button_event =
      Evr.on_el Ev.click
        (fun evt ->
          Ev.stop_propagation evt;
          (* Prevent click_outside_event from triggering *)
          ButtonClick)
        button
    in

    let checkmark =
      span
        ~at:
          (classes
             "text-indigo-600 absolute inset-y-0 right-0 flex items-center pr-4")
        [ embed_svg (checkmark_svg ()) ]
    in

    let item i name =
      let elem =
        li
          ~at:
            (classes
               "text-gray-900 relative cursor-default select-none py-2 pl-3 \
                pr-9"
            @ [ at "role" "option" ])
          []
      in
      let text_child = span ~at:(classes "block truncate") [ txt' name ] in
      let is_selected_s = selected_signal |> S.map (fun i' -> i = i') in
      let evt = Evr.on_el Ev.click (fun _ -> i) elem in
      Elr.def_class (Jstr.v "font-normal") is_selected_s elem;
      Elr.def_class (Jstr.v "font-semibold") is_selected_s elem;
      Elr.def_children elem
        (is_selected_s
        |> S.map (fun is_selected ->
               if is_selected then [ text_child; checkmark ] else [ text_child ])
        );
      (evt, elem)
    in

    let list_evts, list_elems = List.split (List.mapi item items) in

    let click_outside_event =
      Evr.on_el Ev.click (fun _ -> ClickOutside) (Document.body G.document)
    in
    Logr.may_hold
      E.(
        log click_button_event (fun ButtonClick ->
            set_dropdown_open (not (S.value dropdown_open_signal))));
    Logr.may_hold
      E.(log click_outside_event (fun ClickOutside -> set_dropdown_open false));
    Logr.may_hold E.(log (select list_evts) set_selected);

    let list =
      ul
        ~at:
          (classes
             "absolute z-10 mt-1 max-h-60 w-full overflow-auto rounded-md \
              bg-white py-1 text-base shadow-lg ring-1 ring-black \
              ring-opacity-5 focus:outline-none sm:text-sm"
          @ [
              At.tabindex (-1);
              at "role" "listbox";
              at "aria-labelledby" "listbox-label";
            ])
        list_elems
    in

    let result_elem = El.div ~at:(classes "not-prose") [] in
    Elr.def_children result_elem
      (dropdown_open_signal
      |> S.map (fun open' -> if open' then [ button; list ] else [ button ]));
    (result_elem, selected_item)
end

let select items = Select_and_info.select items
let info child = Select_and_info.info child

let a link text =
  El.a ~at:[ At.href (Jstr.v link); class_ "underline" ] [ txt' text ]
