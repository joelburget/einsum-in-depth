module String_set = Set.Make (String)

module Atom = struct
  type t = Name of string | Ellipsis | Parenthesized of string list

  let pp ppf = function
    | Name name -> Fmt.string ppf name
    | Ellipsis -> Fmt.string ppf "..."
    | Parenthesized names -> Fmt.(parens (list ~sep:sp string)) ppf names
end

(** A Group is the set of indices of a tensor. *)
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

(** Bindings are the left-hand side of a rewrite. *)
module Bindings = struct
  type t = Group.t list

  let pp = Fmt.(box (list ~sep:comma Group.pp))
end

(** A Rewrite binds some groups of tensor indices and results in some tensor indices. *)
module Rewrite : sig
  type t = Bindings.t * Group.t
  type indices = { free : String_set.t; summation : String_set.t }

  val indices : t -> indices
  val free_indices : t -> String_set.t
  val summation_indices : t -> String_set.t
  val pp : t Fmt.t
end = struct
  type t = Bindings.t * Group.t
  type indices = { free : String_set.t; summation : String_set.t }

  let indices (lhs, rhs) =
    let lhs' =
      lhs
      |> List.map (fun group -> group |> Group.get_names |> Result.get_ok)
      |> List.flatten |> String_set.of_list
    in
    let rhs' = rhs |> Group.get_names |> Result.get_ok |> String_set.of_list in
    { free = rhs'; summation = String_set.diff lhs' rhs' }

  let free_indices t = (indices t).free
  let summation_indices t = (indices t).summation
  let pp = Fmt.(box (pair ~sep:(any " -> ") Bindings.pp Group.pp))
end

(** Given a list of strings, find a maximal set of matching indices, where each position in a list can only match once (in the case of duplicates either match is okay). The lists are not necessarily the same length. *)
let find_matches lst1 lst2 =
  let h1 = Hashtbl.create 10 in
  let h2 = Hashtbl.create 10 in

  let add_to_hashtbl h lst =
    List.iteri
      (fun i x ->
        if Hashtbl.mem h x then Hashtbl.replace h x (i :: Hashtbl.find h x)
        else Hashtbl.add h x [ i ])
      lst
  in

  add_to_hashtbl h1 lst1;
  add_to_hashtbl h2 lst2;

  let matches = ref [] in

  Hashtbl.iter
    (fun k v1 ->
      if Hashtbl.mem h2 k then
        let v2 = Hashtbl.find h2 k in
        let match_pairs = List.map2 (fun x y -> (k, x, y)) v1 v2 in
        matches := List.append !matches match_pairs)
    h1;

  !matches

let%expect_test "find_matches" =
  let pp_triple ppf (a, b, c) = Fmt.pf ppf "(%s, %d, %d)" a b c in
  let go lst1 lst2 =
    let result = find_matches lst1 lst2 in
    Fmt.pr "@[[%a]@], @[[%a]@] -> @[[%a]@]@."
      Fmt.(list ~sep:semi string)
      lst1
      Fmt.(list ~sep:semi string)
      lst2
      Fmt.(list ~sep:semi pp_triple)
      result
  in
  go [ "a"; "b"; "c" ] [ "b"; "a"; "c" ];
  [%expect {| [a; b; c], [b; a; c] -> [(a, 0, 1); (b, 1, 0); (c, 2, 2)]|}];
  go [ "a"; "b" ] [ "b"; "c"; "a" ];
  [%expect {| [a; b], [b; c; a] -> [(a, 0, 2); (b, 1, 0)]|}];
  go [ "a"; "a" ] [ "a"; "a" ];
  [%expect {| [a; a], [a; a] -> [(a, 1, 1); (a, 0, 0)]|}]

module Single_contraction : sig
  module Op : sig
    type t =
      | Tensordot of (int * int) list
      | Matmul
      | Transpose
      | Inner
      | Trace
      | Sum
      | Diag
      | Swapaxes
      | Id

    val pp : t Fmt.t
  end

  type t = {
    operations : Op.t list;
    contracted : string list;
    preserved : string list;
  }

  (* [get_result a b other_tensors eventual_result] returns the result of a
     single contraction, given the names of the contracted indices [a] and [b],
     the names of the indices of the other tensors [other_tensors], and the
     names of the indices of the eventual result [eventual_result]. *)
  val get_result : string list list -> string list list -> string list -> t
end = struct
  module String_set = Set.Make (String)

  module Op = struct
    type t =
      | Tensordot of (int * int) list
      | Matmul
      | Transpose
      | Inner
      | Trace
      | Sum
      | Diag
      | Swapaxes
      | Id

    let pp ppf = function
      | Tensordot ixs ->
          Fmt.pf ppf "tensordot@[<hov 1>(%a)@]"
            Fmt.(list ~sep:comma (pair int int))
            ixs
      | Matmul -> Fmt.string ppf "matmul"
      | Transpose -> Fmt.string ppf "transpose"
      | Inner -> Fmt.string ppf "inner"
      | Trace -> Fmt.string ppf "trace"
      | Sum -> Fmt.string ppf "sum"
      | Diag -> Fmt.string ppf "diag"
      | Swapaxes -> Fmt.string ppf "swapaxes"
      | Id -> Fmt.string ppf "id"
  end

  type t = {
    operations : Op.t list;
    contracted : string list;
    preserved : string list;
  }

  let pp ppf { operations; contracted; preserved } =
    Fmt.pf ppf "operations: @[[%a]@], contracted: @[[%a]@], preserved: @[[%a]@]"
      Fmt.(list ~sep:comma Op.pp)
      operations
      Fmt.(list string ~sep:semi)
      contracted
      Fmt.(list string ~sep:semi)
      preserved

  (* Try matching two inputs and an output to a tensordot operation *)
  (* let match_tensordot x y z = *)
  (*   let matches = find_matches x y in *)
  (*   failwith "TODO" *)

  let match_op contracted_tensors result =
    match (contracted_tensors, result) with
    | [ x ], x' when x = x' -> Some Op.Id
    | [ [ x ]; [ y ] ], [] when x = y -> Some Inner
    | [ [ x; y ] ], [ y'; x' ] when x = x' && y = y' -> Some Transpose
    | [ [ x; y ]; [ y'; z ] ], [ x'; z' ] when x = x' && y = y' && z = z' ->
        Some Matmul
    | [ [ x; x' ] ], [] when x = x' -> Some Trace
    | [ [ x; x' ] ], [ x'' ] when x = x' && x = x'' -> Some Diag
    | [ [ _ ] ], [] -> Some Sum
    | [ x ], x' when x <> x' && String_set.(of_list x = of_list x') ->
        Some Swapaxes
    (* | [ x; y ], z -> ( *)
    (*     match match_tensordot x y z with Some op -> Some op | _ -> None) *)
    | _ -> None

  let get_result contracted_tensors other_tensors eventual_result =
    let open String_set in
    let contracted_tensors', other_tensors', eventual_result' =
      ( contracted_tensors |> List.flatten |> of_list,
        of_list (List.flatten other_tensors),
        of_list eventual_result )
    in
    (* Contract dimensions which aren't needed later *)
    let contracted =
      elements
        (diff
           (union contracted_tensors' other_tensors')
           (union eventual_result' other_tensors'))
    in
    (* Preserve dimensions which are needed later *)
    let preserved =
      inter contracted_tensors' (union eventual_result' other_tensors')
    in
    (* Maintain the order of dimensions in the result if possible so op can be Id. *)
    let preserved =
      if preserved = eventual_result' then eventual_result
      else elements preserved
    in
    let operations =
      match match_op contracted_tensors preserved with
      | Some op -> [ op ]
      | None -> [ (* TODO *) ]
    in
    { operations; contracted; preserved }

  let%expect_test "operations" =
    let go contracted_tensors eventual_result =
      (* Fmt.pr "contracted_tensors: [%a], eventual_result: [%a]\n" *)
      (*   Fmt.(list ~sep:semi (list string)) *)
      (*   contracted_tensors *)
      (*   Fmt.(list string) *)
      (*   eventual_result; *)
      let { operations; _ } =
        get_result contracted_tensors [] eventual_result
      in
      Fmt.pr "%a\n" Fmt.(list ~sep:comma Op.pp) operations
    in
    go [ [ "i" ]; [ "i" ] ] [];
    [%expect {| inner |}];
    go [ [ "i" ] ] [ "i" ];
    [%expect {| id |}];
    go [ [ "i"; "j" ]; [ "j"; "k" ] ] [ "i"; "k" ];
    [%expect {| matmul |}];
    go [ [ "i"; "i" ] ] [];
    [%expect {| trace |}];
    go [ [ "i"; "j" ] ] [ "i"; "j" ];
    [%expect {| id |}];
    go [ [ "i"; "j" ] ] [ "j"; "i" ];
    [%expect {| transpose |}];
    go [ [ "i" ] ] [];
    [%expect {| sum |}];
    go [ [ "i"; "i" ] ] [ "i" ];
    [%expect {| diag |}]
  (* TODO *)
  (* go [ [ "i"; "j"; "k" ] ] [ "k"; "j"; "i" ]; *)
  (* [%expect {| swapaxes |}] *)

  let%expect_test "get_result" =
    let go contracted_tensors other_tensors eventual_result =
      get_result contracted_tensors other_tensors eventual_result
      |> pp Fmt.stdout
    in
    go [ [ "a"; "b" ]; [ "c"; "d" ] ] [] [ "a"; "b"; "c"; "d" ];
    [%expect {| operations: [], contracted: [], preserved: [a; b; c; d] |}];
    go [ [ "a"; "j"; "k" ]; [ "a"; "j"; "k" ] ] [] [];
    [%expect {| operations: [], contracted: [a; j; k], preserved: [] |}];
    go [ [ "a"; "j"; "k" ]; [ "a"; "i"; "j" ] ] [ [ "a"; "i"; "k" ] ] [];
    [%expect {| operations: [], contracted: [j], preserved: [a; i; k] |}];
    go
      [ [ "n"; "l"; "k" ]; [ "i"; "j"; "k" ] ]
      [ [ "i"; "l"; "m" ]; [ "n"; "j"; "m" ]; [ "a"; "b"; "c" ] ]
      [ "i"; "n"; "j"; "l" ];
    [%expect {| operations: [], contracted: [k], preserved: [i; j; l; n] |}];
    go [ [ "i"; "i" ]; [ "i"; "i" ]; [ "i"; "i" ] ] [] [ "i" ];
    [%expect {| operations: [], contracted: [], preserved: [i] |}]
end

module Pyloops = struct
  type t = {
    free_indices : String_set.t;
    summation_indices : String_set.t;
    lhs_tensors : string list list;
    rhs_tensor : string list;
  }

  let mk_indent indent = String.make (indent * 4) ' '

  let pp ppf { free_indices; summation_indices; lhs_tensors; rhs_tensor } =
    let free_indices = String_set.elements free_indices in
    let summation_indices = String_set.elements summation_indices in

    (* initialize result *)
    (match rhs_tensor with
    | [] -> ()
    | _ ->
        Fmt.pf ppf "result = np.empty((@[%a@]))@."
          Fmt.(list ~sep:comma string)
          (List.map (fun index -> "N" ^ index) rhs_tensor));

    (* loops *)
    let outer_indent =
      List.fold_left
        (fun indent index ->
          Fmt.pf ppf "%sfor %s in range(N%s):@." (mk_indent indent) index index;
          indent + 1)
        0 free_indices
    in
    Fmt.pf ppf "%stotal = 0@." (mk_indent outer_indent);
    let inner_indent =
      List.fold_left
        (fun indent index ->
          Fmt.pf ppf "%sfor %s in range(N%s):@." (mk_indent indent) index index;
          indent + 1)
        outer_indent summation_indices
    in

    (* summation inside loop *)
    (* let indices = free_indices @ summation_indices in *)
    let pp_access ppf (tensor, indices) =
      Fmt.pf ppf "%s[%a]" tensor Fmt.(list ~sep:comma string) indices
    in
    (* Name tensors starting with A, then B, etc *)
    let accesses =
      List.mapi
        (fun i tensor -> (String.make 1 (Char.chr (i + 65)), tensor))
        lhs_tensors
    in
    Fmt.pf ppf "%stotal += @[%a@]@." (mk_indent inner_indent)
      Fmt.(list ~sep:(any " * ") pp_access)
      accesses;

    (* assign total to result *)
    (match rhs_tensor with
    | [] -> ()
    | _ ->
        Fmt.pf ppf "%sresult[@[%a@]] = total@." (mk_indent outer_indent)
          Fmt.(list ~sep:comma string)
          free_indices);

    (* return result *)
    match rhs_tensor with
    | [] -> Fmt.pf ppf "return total@."
    | _ -> Fmt.pf ppf "return result@."
end

module Explain : sig
  val contract_path : Rewrite.t -> int list list -> string list
  (** Contract along the given path, returning an explanation in the format of
   one string per step. *)

  val show_loops : Rewrite.t -> Pyloops.t
  (** Put in [Pyloops.t] format. *)
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
    let go rewrite path =
      contract_path rewrite path |> Fmt.(list ~sep:sp string) Fmt.stdout
    in
    let rewrite =
      Atom.
        ( [
            [ Name "a"; Name "i"; Name "j" ];
            [ Name "a"; Name "j"; Name "k" ];
            [ Name "a"; Name "i"; Name "k" ];
          ],
          [] )
    in
    go rewrite [ [ 1; 2 ]; [ 0; 1 ] ];
    [%expect
      {|
      Step 1: contract k (a j k, a i k -> a i j)
      Step 2: contract a i j (a i j, a i j -> )
      |}];
    go rewrite [ [ 0; 1 ]; [ 0; 1 ] ];
    [%expect
      {|
      Step 1: contract j (a i j, a j k -> a i k)
      Step 2: contract a i k (a i k, a i k -> )
      |}];
    let rewrite =
      ( [ [ Atom.Name "i"; Name "k" ]; [ Name "k"; Name "j" ] ],
        [ Atom.Name "i"; Name "j" ] )
    in
    go rewrite [ [ 0; 1 ] ];
    [%expect {| Step 1: contract k (i k, k j -> i j) |}]

  let show_loops rewrite =
    let lhs, rhs = rewrite in
    let lhs_tensors =
      List.map (fun tensor -> tensor |> Group.get_names |> Result.get_ok) lhs
    in
    Pyloops.
      {
        free_indices = Rewrite.free_indices rewrite;
        summation_indices = Rewrite.summation_indices rewrite;
        lhs_tensors;
        rhs_tensor = rhs |> Group.get_names |> Result.get_ok;
      }

  let%expect_test "show_loops" =
    let go (rewrite : Rewrite.t) =
      show_loops rewrite |> Pyloops.pp Fmt.stdout
    in

    go
      ( [ [ Atom.Name "i"; Name "k" ]; [ Name "k"; Name "j" ] ],
        [ Atom.Name "i"; Name "j" ] );
    [%expect
      {|
      result = np.empty((Ni, Nj))
      for i in range(Ni):
          for j in range(Nj):
              total = 0
              for k in range(Nk):
                  total += A[i, k] * B[k, j]
              result[i, j] = total
      return result
    |}];

    go ([ [ Atom.Name "s" ]; [ Name "s"; Name "t" ]; [ Name "t" ] ], []);
    [%expect
      {|
      total = 0
      for s in range(Ns):
          for t in range(Nt):
              total += A[s] * B[s, t] * C[t]
      return total
    |}];

    go ([ [ Atom.Name "i"; Name "i" ] ], [ Name "i" ]);
    [%expect
      {|
      result = np.empty((Ni))
      for i in range(Ni):
          total = 0
          total += A[i, i]
          result[i] = total
      return result
    |}];

    go ([ [ Atom.Name "i"; Name "i" ] ], []);
    [%expect
      {|
      total = 0
      for i in range(Ni):
          total += A[i, i]
      return total
    |}];

    go ([ [ Atom.Name "s" ]; [ Name "s"; Name "t" ]; [ Name "t" ] ], []);
    [%expect
      {|
      total = 0
      for s in range(Ns):
          for t in range(Nt):
              total += A[s] * B[s, t] * C[t]
      return total
    |}];

    go
      ( [ [ Atom.Name "b"; Name "i" ]; [ Name "b"; Name "j" ] ],
        [ Name "b"; Name "i"; Name "j" ] );
    [%expect
      {|
      result = np.empty((Nb, Ni, Nj))
      for b in range(Nb):
          for i in range(Ni):
              for j in range(Nj):
                  total = 0
                  total += A[b, i] * B[b, j]
                  result[b, i, j] = total
      return result
    |}]
end
