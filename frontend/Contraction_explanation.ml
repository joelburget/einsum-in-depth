open Brr
open Brr_note
open Frontend_util
open Note
open Tensor_playground

type validated_inputs = (string * int) list * (string * int) list * string list

module String_set = Set.Make (String)

let parse_contraction_s :
    string signal -> (El.t list, El.t list) Result.t signal =
  S.map (fun str ->
      match parse_einsum str with
      | Ok _einsum -> Result.Ok []
      | Error (msg1, msg2) -> Error [ txt' msg1; txt' msg2 ])

let rec get_names = function
  | [] -> Ok []
  | Einops.Atom.Name s :: atoms ->
      get_names atoms |> Result.map (fun names -> s :: names)
  | atom :: _ ->
      Error (Fmt.str "get_names: expected name, got %a" Einops.Atom.pp atom)

let list_of_err = function Result.Ok _ -> [] | Error err -> [ err ]

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
    int list ->
    int list ->
    Einops.Rewrite.t ->
    (int * int) list ->
    (validated_inputs, El.t list) result =
 fun a_ty b_ty eqn _path ->
  let bindings, rhs = eqn in
  match bindings with
  | [ g1; g2 ] -> (
      match (get_names g1, get_names g2, get_names rhs) with
      | Ok g1_names, Ok g2_names, Ok rhs_names ->
          if List.length g1_names <> List.length a_ty then
            Error [ txt' "Wrong number of indices in first group" ]
          else if List.length g2_names <> List.length b_ty then
            Error [ txt' "Wrong number of indices in second group" ]
          else if List.length g1_names > 2 || List.length g2_names > 2 then
            (* TODO: generalize *)
            Error [ txt' "Only 1D / 2D tensors are currently supported" ]
          else
            let g1_names_set = String_set.of_list g1_names in
            let g2_names_set = String_set.of_list g2_names in
            let rhs_names_set = String_set.of_list rhs_names in
            if
              not
                String_set.(
                  subset rhs_names_set (union g1_names_set g2_names_set))
            then
              Error
                [
                  txt'
                    "Indices in the result must be a subset of the indices in \
                     the inputs";
                ]
            else
              Ok
                ( List.combine g1_names a_ty,
                  List.combine g2_names b_ty,
                  rhs_names )
      | x, y, z ->
          let errs =
            list_of_err x @ list_of_err y @ list_of_err z |> List.map txt'
          in
          Error errs)
  | _ ->
      (* TODO: generalize *)
      Error [ txt' "Currently only binary contractions are supported" ]

let explain_contraction : validated_inputs -> (int * int) list -> El.t list =
 fun _ _ -> [ txt' "TODO: explanation" ]

let parse_contraction : string -> (Einops.Rewrite.t, string * string) result =
 fun str ->
  match parse_einsum str with
  | Ok einsum -> Ok einsum
  | Error (msg1, msg2) -> Error (msg1, msg2)

let explain container a_type_str b_type_str contraction_str =
  let result_output = div [] in
  let parsed_a_signal, a_input, a_err_elem =
    bracketed_parsed_input parse_type a_type_str
  in
  let parsed_b_signal, b_input, b_err_elem =
    bracketed_parsed_input parse_type b_type_str
  in
  let contraction_signal, c_input, c_err_elem =
    parsed_input parse_contraction contraction_str
  in
  let parse_path s =
    s |> Path_parser.parse |> Result.map_error (fun msg -> (msg, ""))
  in
  let parsed_path_signal, path_input, path_err_elem =
    bracketed_parsed_input parse_path ""
  in

  let f as_opt bs_opt contraction_opt parsed_path_opt =
    match (as_opt, bs_opt, contraction_opt, parsed_path_opt) with
    | Some as_, Some bs, Some contraction, Some path -> (
        match validate_path 2 path with
        | Some err -> err
        | None -> (
            match validate_inputs as_ bs contraction path with
            | Ok validated_inputs -> explain_contraction validated_inputs path
            | Error err -> err))
    | _ -> [ txt' "TODO" ]
  in

  let explanation_signal =
    l4 f parsed_a_signal parsed_b_signal contraction_signal parsed_path_signal
  in

  Elr.def_children result_output explanation_signal;
  set_children container
    [
      div [ txt' "A: "; a_input; a_err_elem ];
      div [ txt' "B: "; b_input; b_err_elem ];
      div [ txt' "Contraction: "; c_input; c_err_elem ];
      div [ txt' "Path: "; path_input; path_err_elem ];
      result_output;
    ]
