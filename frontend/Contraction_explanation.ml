open Brr
open Brr_note
open Frontend_util
open Note
(* open Tensor_playground *)

(* let explain_contraction : *)

let parse_contraction_s :
    string signal -> (El.t list, El.t list) Result.t signal =
  S.map (fun str ->
      match parse_einsum str with
      | Ok _einsum -> Result.Ok []
      | Error (msg1, msg2) -> Error [ txt' msg1; txt' msg2 ])

let parse_contraction : string -> (El.t list, string * string) result =
 fun str ->
  match parse_einsum str with
  | Ok _einsum -> Ok []
  | Error (msg1, msg2) -> Error (msg1, msg2)

let explain container a_type_str b_type_str contraction_str =
  let result_output = div [] in
  let parsed_a_signal, a_input, a_err_elem =
    bracketed_parsed_input parse_type a_type_str
  in
  let parsed_b_signal, b_input, b_err_elem =
    bracketed_parsed_input parse_type b_type_str
  in
  let output_signal, c_input, c_err_elem =
    parsed_input parse_contraction contraction_str
  in
  Elr.def_children result_output
    (output_signal |> S.map (function None -> [] | Some elems -> elems));

  Logr.hold (S.log parsed_a_signal (fun _ -> ()));
  Logr.hold (S.log parsed_b_signal (fun _ -> ()));

  set_children container
    [
      div [ txt' "A: "; a_input; a_err_elem ];
      div [ txt' "B: "; b_input; b_err_elem ];
      div [ txt' "Contraction: "; c_input; c_err_elem ];
      result_output;
    ]
