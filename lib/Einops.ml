module Atom = struct
  type t = Name of string | Ellipsis | Parenthesized of string list

  let pp ppf = function
    | Name name -> Fmt.string ppf name
    | Ellipsis -> Fmt.string ppf "..."
    | Parenthesized names -> Fmt.(parens (list ~sep:sp string)) ppf names
end

module Group = struct
  type t = Atom.t list

  let pp = Fmt.(box (list ~sep:sp Atom.pp))

  let rec get_names = function
    | [] -> Ok []
    | Atom.Name s :: atoms ->
        get_names atoms |> Result.map (fun names -> s :: names)
    | atom :: _ ->
        Error (Fmt.str "get_names: expected name, got %a" Atom.pp atom)
end

module Bindings = struct
  type t = Group.t list

  let pp = Fmt.(box (list ~sep:comma Group.pp))
end

module Rewrite = struct
  type t = Bindings.t * Group.t

  let pp = Fmt.(box (pair ~sep:(any " -> ") Bindings.pp Group.pp))
end

module Single_contraction : sig
  type single_contraction = {
    contracted : string list;
    preserved : string list;
  }

  (* [get_result a b other_tensors eventual_result] returns the result of a
     single contraction, given the names of the contracted indices [a] and [b],
     the names of the indices of the other tensors [other_tensors], and the
     names of the indices of the eventual result [eventual_result]. *)
  val get_result :
    string list list -> string list list -> string list -> single_contraction
end = struct
  module String_set = Set.Make (String)

  type single_contraction = {
    contracted : string list;
    preserved : string list;
  }

  let pp_single_contraction ppf { contracted; preserved } =
    Fmt.pf ppf "contracted: @[[%a]@], preserved: @[[%a]@]"
      Fmt.(list string ~sep:semi)
      contracted
      Fmt.(list string ~sep:semi)
      preserved

  let get_result contracted_tensors other_tensors eventual_result =
    let open String_set in
    let contracted_tensors, other_tensors, eventual_result =
      ( contracted_tensors |> List.flatten |> of_list,
        of_list (List.flatten other_tensors),
        of_list eventual_result )
    in
    (* Contract dimensions which aren't needed later *)
    let contracted =
      elements
        (diff
           (union contracted_tensors other_tensors)
           (union eventual_result other_tensors))
    in
    (* Preserve dimensions which are needed later *)
    let preserved =
      elements (inter contracted_tensors (union eventual_result other_tensors))
    in
    { contracted; preserved }

  let%expect_test "get_result" =
    let go contracted_tensors other_tensors eventual_result =
      get_result contracted_tensors other_tensors eventual_result
      |> pp_single_contraction Fmt.stdout
    in
    go [ [ "a"; "b" ]; [ "c"; "d" ] ] [] [ "a"; "b"; "c"; "d" ];
    [%expect {| contracted: [], preserved: [a; b; c; d] |}];
    go [ [ "a"; "j"; "k" ]; [ "a"; "j"; "k" ] ] [] [];
    [%expect {| contracted: [a; j; k], preserved: [] |}];
    go [ [ "a"; "j"; "k" ]; [ "a"; "i"; "j" ] ] [ [ "a"; "i"; "k" ] ] [];
    [%expect {| contracted: [j], preserved: [a; i; k] |}];
    go
      [ [ "n"; "l"; "k" ]; [ "i"; "j"; "k" ] ]
      [ [ "i"; "l"; "m" ]; [ "n"; "j"; "m" ]; [ "a"; "b"; "c" ] ]
      [ "i"; "n"; "j"; "l" ];
    [%expect {| contracted: [k], preserved: [i; j; l; n] |}];
    go [ [ "i"; "i" ]; [ "i"; "i" ]; [ "i"; "i" ] ] [] [ "i" ];
    [%expect {| contracted: [], preserved: [i] |}]
end

module Explain : sig
  val contract_path : Rewrite.t -> int list list -> string list
end = struct
  let contract_path contraction path =
    let bindings, result_group = contraction in
    let result_group = result_group |> Group.get_names |> Result.get_ok in
    let _, _, steps =
      path
      |> List.fold_left
           (fun (i, tensors, steps) ixs ->
             let contracted_tensors = List.map (List.nth tensors) ixs in
             let new_tensors =
               List.fold_right Util.delete_from_list ixs tensors
             in
             let single_contraction =
               Single_contraction.get_result contracted_tensors new_tensors
                 result_group
             in
             let new_tensors =
               List.append new_tensors [ single_contraction.preserved ]
             in
             let step =
               Fmt.(
                 str "Step %i: contract @[%a@] (@[%a@] -> @[%a@])" i
                   (list string ~sep:sp) single_contraction.contracted
                   (list (list string ~sep:sp) ~sep:comma)
                   contracted_tensors (list string ~sep:sp)
                   single_contraction.preserved)
             in
             (i + 1, new_tensors, List.append steps [ step ]))
           ( 1,
             List.map
               (fun binding -> binding |> Group.get_names |> Result.get_ok)
               bindings,
             [] )
    in
    steps

  let%expect_test "contract_path" =
    let rewrite =
      Atom.
        ( [
            [ Name "a"; Name "i"; Name "j" ];
            [ Name "a"; Name "j"; Name "k" ];
            [ Name "a"; Name "i"; Name "k" ];
          ],
          [] )
    in
    let path = [ [ 1; 2 ]; [ 0; 1 ] ] in
    contract_path rewrite path |> Fmt.(list ~sep:sp string) Fmt.stdout;
    [%expect
      {|
      Step 1: contract k (a j k, a i k -> a i j)
      Step 2: contract a i j (a i j, a i j -> )
      |}]
end
