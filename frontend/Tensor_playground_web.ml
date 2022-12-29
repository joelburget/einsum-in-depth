open Brr
open Tensor_playground
open Isometric
open Note

let render_elem = function
  | Tensor_type.Elem.Concrete i -> El.txt' (Int.to_string i)
  | Variable v -> El.txt' v

let render_tensor_type t = El.txt' (Tensor_type.to_string t)

let process_type_input str =
  str |> Lexing.from_string |> Type_parser.ty Type_lexer.token

module Parse_result = struct
  type t = Ok of Tensor_type.t | No_input | Error of string
end

let parse str =
  try Parse_result.Ok (process_type_input str) with
  | Type_lexer.Eof ->
      if str = "" then No_input else Error "Unknown error (unclosed bracket?)"
  | Type_lexer.Error msg -> Error msg

let explain : Tensor_type.t -> Tensor_type.t -> El.t list =
 fun xs ys ->
  let len_xs = List.length xs in
  let len_ys = List.length ys in
  let max_len = max len_xs len_ys in
  let pad n = List.init n (fun _ -> Tensor_type.Elem.Concrete 1) in
  let padded_xs = pad (max_len - len_xs) @ xs in
  let padded_ys = pad (max_len - len_ys) @ ys in
  El.
    [
      div
        [ txt' "First we align the two tensor types, starting from the right." ];
      table
        [
          tbody
            [
              tr
                (padded_xs
                |> List.map Tensor_type.Elem.to_string
                |> List.map txt');
              tr
                (padded_ys
                |> List.map Tensor_type.Elem.to_string
                |> List.map txt');
            ];
        ];
      div
        [
          txt'
            "Because the number of dimensions is not equal, prepend 1s to the \
             front of the tensor with fewer dimensions to make them the same \
             length.";
        ];
      div
        [
          txt'
            "Check that in each position, the tensor sizes are equal, or one \
             of them is 1.";
        ];
      div [ txt' "Finally, take the maximum of each pair of sizes" ];
    ]

let update_output : (string * string) signal -> El.t list signal =
  S.map (fun (a, b) ->
      match (parse a, parse b) with
      | Ok a, Ok b -> explain a b
      | No_input, _ | _, No_input -> El.[ txt' "(empty input)" ]
      | Error msg, Ok _ -> El.[ txt' "Error parsing A: "; txt' msg ]
      | Ok _, Error msg -> El.[ txt' "Error parsing B: "; txt' msg ]
      | Error msg1, Error msg2 ->
          El.
            [
              div [ txt' "Error parsing A: "; txt' msg1 ];
              div [ txt' "Error parsing B: "; txt' msg2 ];
            ])

let () =
  let a_init, b_init = ("[1, 2]", "[2, 3, 2]") in
  let a_input = El.input ~at:[ At.value (Jstr.of_string a_init) ] () in
  let b_input = El.input ~at:[ At.value (Jstr.of_string b_init) ] () in
  let result_output = El.div [] in

  let a_signal, set_a = S.create a_init in
  let b_signal, set_b = S.create b_init in

  Brr_note.Evr.endless_listen (El.as_target a_input) Ev.change (fun _evt ->
      set_a (Jstr.to_string (El.prop El.Prop.value a_input)));
  Brr_note.Evr.endless_listen (El.as_target b_input) Ev.change (fun _evt ->
      set_b (Jstr.to_string (El.prop El.Prop.value b_input)));

  let output_signal = S.Pair.v a_signal b_signal |> update_output in
  Brr_note.Elr.def_children result_output output_signal;

  El.set_children (Document.body G.document)
    El.
      [
        div [ txt' "A: "; a_input ];
        div [ txt' "B: "; b_input ];
        div [ txt' "Result: "; result_output ];
      ]

let isometric () =
  let tensor_type = Tensor_type.[ Elem.Variable "a"; Elem.Concrete 1 ] in
  let container = El.div [] in
  let canvas =
    Isometric_canvas.(
      let opts =
        opts ~container ~background_color:"#ccc" ~scale:10. ~height:320
          ~width:500 ()
      in
      create ~opts ())
  in

  let fill_color i j =
    match (i, j) with
    | 0, 0 -> Some "#000"
    | 0, 9 -> Some "#a00"
    | 9, 0 -> Some "#0a0"
    | 9, 9 -> Some "#00a"
    | _ -> None
  in

  (* top *)
  for i = 0 to 9 do
    for j = 0 to 9 do
      let fill_color = fill_color i j in
      let opts =
        Isometric_rectangle.opts ~height:1 ~width:1 ~top:10 ~right:i ~left:j
          ?fill_color ~plane_view:Plane_view.top ()
      in
      Isometric_canvas.add_child canvas (Isometric_rectangle.create ~opts ())
    done
  done;

  (* front *)
  for i = 0 to 9 do
    for j = 0 to 9 do
      let fill_color = fill_color i j in
      let opts =
        Isometric_rectangle.opts ~height:1 ~width:1 ~top:i ~right:10 ~left:j
          ?fill_color ~plane_view:Plane_view.front ()
      in
      Isometric_canvas.add_child canvas (Isometric_rectangle.create ~opts ())
    done
  done;

  (* side *)
  for i = 0 to 9 do
    for j = 0 to 9 do
      let fill_color = fill_color i j in
      let opts =
        Isometric_rectangle.opts ~height:1 ~width:1 ~top:i ~right:j ?fill_color
          ~left:10 ~plane_view:Plane_view.side ()
      in
      Isometric_canvas.add_child canvas (Isometric_rectangle.create ~opts ())
    done
  done;

  let content = El.[ div [ render_tensor_type tensor_type ]; container ] in
  El.set_children (Document.body G.document) content
