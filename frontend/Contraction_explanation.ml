open Brr
open Brr_note
open Frontend_util
open Note
open Tensor_playground

type parse_result =
  | Error of string list * string list * string list
  | Ok of El.t list * Tensor_type.bracketed * Tensor_type.bracketed

(* let explain_contraction : *)

let parse_types : (string * string * string) signal -> parse_result signal =
  S.map (fun (a, b, c) ->
      match (parse_type a, parse_type b, parse_einsum c) with
      | Ok (_a, a_bracketed), Ok (_b, b_bracketed), Ok _einsum ->
          Ok ([], a_bracketed, b_bracketed)
      | a, b, c ->
          Error
            ( collect_parse_errors a,
              collect_parse_errors b,
              collect_parse_errors c ))

module S_triple = struct
  let fst ?eq s = S.map ?eq (fun (a, _, _) -> a) s
  let snd ?eq s = S.map ?eq (fun (_, b, _) -> b) s
  let trd ?eq s = S.map ?eq (fun (_, _, c) -> c) s
  let v s0 s1 s2 = S.l3 (fun a b c -> (a, b, c)) s0 s1 s2
end

let explain container a_type_str b_type_str contraction_str =
  let a_input = input ~at:[ At.value (Jstr.of_string a_type_str) ] () in
  let b_input = input ~at:[ At.value (Jstr.of_string b_type_str) ] () in
  let c_input = input ~at:[ At.value (Jstr.of_string contraction_str) ] () in
  let result_output = div [] in
  let a_parse_error = div [] in
  let b_parse_error = div [] in
  let c_parse_error = div [] in

  let a_signal, set_a = S.create a_type_str in
  let b_signal, set_b = S.create b_type_str in
  let c_signal, set_c = S.create contraction_str in
  let a_bracket_signal, set_a_bracket = S.create Tensor_type.Unbracketed in
  let b_bracket_signal, set_b_bracket = S.create Tensor_type.Unbracketed in
  let output_signal = S_triple.v a_signal b_signal c_signal |> parse_types in
  let result_signal, set_result = S.create [] in
  let a_parse_error_signal, set_a_parse_error = S.create [] in
  let b_parse_error_signal, set_b_parse_error = S.create [] in
  let c_parse_error_signal, set_c_parse_error = S.create [] in

  Evr.endless_listen (as_target a_input) Ev.change (fun _evt ->
      set_a (Jstr.to_string (prop El.Prop.value a_input)));
  Evr.endless_listen (as_target b_input) Ev.change (fun _evt ->
      set_b (Jstr.to_string (prop El.Prop.value b_input)));
  Evr.endless_listen (as_target c_input) Ev.change (fun _evt ->
      set_c (Jstr.to_string (prop El.Prop.value c_input)));

  let output_logger =
    S.log output_signal (function
      | Ok (elems, a_bracketed, b_bracketed) ->
          set_result elems;
          set_a_bracket a_bracketed;
          set_b_bracket b_bracketed;
          set_a_parse_error [];
          set_b_parse_error [];
          set_c_parse_error []
      | Error (a_msgs, b_msgs, c_msgs) ->
          set_a_parse_error (List.map txt' a_msgs);
          set_b_parse_error (List.map txt' b_msgs);
          set_c_parse_error (List.map txt' c_msgs))
  in
  Logr.hold output_logger;

  Elr.def_children result_output result_signal;
  Elr.def_children a_parse_error a_parse_error_signal;
  Elr.def_children b_parse_error b_parse_error_signal;
  Elr.def_children c_parse_error c_parse_error_signal;
  set_children container
    [
      div
        [ txt' "A: "; bracketed_input a_bracket_signal a_input; a_parse_error ];
      div
        [ txt' "B: "; bracketed_input b_bracket_signal b_input; b_parse_error ];
      div [ txt' "Contraction: "; c_input; c_parse_error ];
      result_output;
    ]
