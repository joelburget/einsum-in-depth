open Brr
open Tensor_playground
open Isometric
open Note

let render_elem = function
  | Tensor_type.Elem.Concrete i -> El.txt' (Int.to_string i)
  | Variable v -> El.txt' v

let render_tensor_type t = El.txt' (Tensor_type.to_string t)

let () =
  let a_input = El.input () in
  let a_signal, a_send = E.create () in
  let b_input = El.input () in
  (* let b_signal, b_send = E.create () in *)
  Brr_note.Evr.endless_listen (El.as_target a_input) Ev.change (fun _evt ->
      let v = El.prop El.Prop.value a_input in
      Console.log [ v ];
      a_send v);
  (* Brr_note.Evr.on_el Ev.change *)
  (*   (fun _evt -> b_send (Jstr.to_string (El.prop El.Prop.value b_input))) *)
  (*   b_input; *)
  let result_output = El.span [] in
  let a_signal =
    S.hold Jstr.empty a_signal |> S.map (fun jstr -> [ El.txt jstr ])
  in
  Brr_note.Elr.def_children result_output a_signal;
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
