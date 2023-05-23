open Brr
open Brr_note
open Frontend_util
open Note
open Tensor_playground

type validated_inputs = {
  a_indices : (string * int) list;
  b_indices : (string * int) list;
  rhs_names : string list;
}

module String_set = Set.Make (String)
module String_map = Map.Make (String)

let of_alist alist = String_map.of_seq (List.to_seq alist)

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
          let repeats = Util.find_repeats rhs_names in
          if List.length g1_names <> List.length a_ty then
            Error [ txt' "Wrong number of indices in first group" ]
          else if List.length g2_names <> List.length b_ty then
            Error [ txt' "Wrong number of indices in second group" ]
          else if List.length g1_names > 2 || List.length g2_names > 2 then
            (* TODO: generalize *)
            Error [ txt' "Only 1D / 2D tensors are currently supported" ]
          else if repeats <> [] then
            Error
              [
                fmt_txt "Indices in the result must be unique. Invalid: %a."
                  Fmt.(brackets (list ~sep:comma string))
                  repeats;
              ]
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
                {
                  a_indices = List.combine g1_names a_ty;
                  b_indices = List.combine g2_names b_ty;
                  rhs_names;
                }
      | x, y, z ->
          Error (list_of_err x @ list_of_err y @ list_of_err z |> List.map txt')
      )
  | _ ->
      (* TODO: generalize *)
      Error [ txt' "Currently only binary contractions are supported" ]

let explain_contraction : validated_inputs -> (int * int) list -> El.t list =
 (* We ignore the path for now since it has to be [(0, 1)] *)
 fun { a_indices; b_indices; rhs_names } _path ->
  let a_names_set = a_indices |> List.map fst |> String_set.of_list in
  let b_names_set = b_indices |> List.map fst |> String_set.of_list in
  let rhs_names_set = String_set.of_list rhs_names in
  let repeated_names = String_set.inter a_names_set b_names_set in
  let omitted_names =
    String_set.(
      diff (union a_names_set b_names_set) (union rhs_names_set repeated_names))
  in
  let index_size = of_alist (a_indices @ b_indices) in
  let result_dims =
    List.map (fun name -> String_map.find name index_size) rhs_names
  in
  let in_rhs name =
    match String_set.find_opt name rhs_names_set with
    | Some _ -> true
    | None -> false
  in
  let index_name (name, _) = if in_rhs name then name else ":" in
  let a_indices' = List.map index_name a_indices in
  let b_indices' = List.map index_name b_indices in
  let index_size (name, size) = if in_rhs name then None else Some size in
  let indexed_a_dims = List.filter_map index_size a_indices in
  let indexed_b_dims = List.filter_map index_size b_indices in
  [
    ul
      [
        li
          [
            fmt_txt "The resulting tensor will have dimensions %a"
              Fmt.(brackets (list ~sep:comma int))
              result_dims;
          ];
        li
          [
            fmt_txt
              "Each variable which occurs on the right-hand side (%a), will be \
               an axis of the resulting tensor."
              Fmt.(list ~sep:comma string)
              (String_set.elements rhs_names_set);
          ];
        li
          [
            fmt_txt
              "Names %a are omitted, so values along those axes will be summed"
              Fmt.(brackets (list ~sep:comma string))
              (String_set.elements omitted_names);
          ];
        li
          [
            fmt_txt
              "Names %a are repeated, so values along those axes will be \
               multiplied"
              Fmt.(brackets (list ~sep:comma string))
              (String_set.elements repeated_names);
          ];
      ];
    p
      [
        fmt_txt "For each index of the resulting tensor (%a):"
          Fmt.(list ~sep:comma string)
          (String_set.elements rhs_names_set);
      ];
    ol
      [
        li
          [
            ul
              [
                span [ txt' "Index into both of the input tensors:" ];
                li
                  [
                    code
                      [
                        fmt_txt "A%a: %a"
                          Fmt.(brackets (list ~sep:comma string))
                          a_indices'
                          Fmt.(brackets (list ~sep:comma int))
                          indexed_a_dims;
                      ];
                  ];
                li
                  [
                    code
                      [
                        fmt_txt "B%a: %a"
                          Fmt.(brackets (list ~sep:comma string))
                          b_indices'
                          Fmt.(brackets (list ~sep:comma int))
                          indexed_b_dims;
                      ];
                  ];
              ];
          ];
        li [ txt' "Broadcast if necessary" ];
        li [ txt' "Pointwise multiply" ];
        li [ txt' "Sum" ];
      ];
  ]

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
