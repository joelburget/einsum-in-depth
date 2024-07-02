module String_set = Set.Make (String)

(** A Group is the set of indices of a tensor. *)
module Group = struct
  type t = string list

  let pp = Fmt.(box (list ~sep:sp string))
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
    let lhs' = lhs |> List.flatten |> String_set.of_list in
    let rhs' = rhs |> String_set.of_list in
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
  [%expect {| [a; a], [a; a] -> [(a, 1, 1); (a, 0, 0)]|}];
  go [ "a"; "j"; "k" ] [ "a"; "j"; "k" ];
  [%expect {| [a; j; k], [a; j; k] -> [(a, 0, 0); (k, 2, 2); (j, 1, 1)]|}]

(* We can simplify a list of matches into a single index if the count from
   the end of a and the beginning of b simultaneously (and only contain up to
   two matches). *)
let matches_simplify matches len_x =
  List.length matches <= 2
  && List.for_all
       (fun (_, a, b) -> (b = 0 || b = 1) && len_x - a - 1 = b)
       matches

let%expect_test "matches_simplify" =
  let pp_triple ppf (a, b, c) = Fmt.pf ppf "(%s, %d, %d)" a b c in
  let go matches len_x =
    Fmt.pr "@[[%a] -> %b@]@."
      Fmt.(list ~sep:semi pp_triple)
      matches
      (matches_simplify matches len_x)
  in
  go [] 2;
  [%expect {| [] -> true|}];
  go [ ("a", 1, 0); ("b", 0, 1) ] 2;
  [%expect {| [(a, 1, 0); (b, 0, 1)] -> true|}];
  go [ ("a", 0, 0) ] 2;
  [%expect {| [(a, 0, 0)] -> false|}];
  go [ ("a", 1, 1) ] 2;
  [%expect {| [(a, 1, 1)] -> false|}];
  go [ ("a", 0, 1); ("b", 1, 0) ] 2;
  [%expect {| [(a, 0, 1); (b, 1, 0)] -> true|}];
  go [ ("a", 0, 1); ("b", 1, 0) ] 3;
  [%expect {| [(a, 0, 1); (b, 1, 0)] -> false|}];
  go [ ("a", 1, 1); ("b", 2, 0) ] 3;
  [%expect {| [(a, 1, 1); (b, 2, 0)] -> true|}];
  go [ ("a", 0, 2) ] 3;
  [%expect {| [(a, 0, 2)] -> false|}]

(* Remove list elements at the given indices *)
let remove_indices lst indices =
  let rec go lst indices acc =
    match (lst, indices) with
    | [], _ -> List.rev acc
    | _, [] -> List.rev_append acc lst
    | x :: xs, i :: is ->
        if i = 0 then go xs (List.map pred is) acc
        else go xs (List.map pred indices) (x :: acc)
  in
  go lst (List.sort compare indices) []

let%expect_test "remove_indices" =
  let go lst indices =
    Fmt.(
      pr "@[[%a] - [%a] -> [%a]@]@." (list ~sep:comma int) lst
        (list ~sep:comma int) indices (list ~sep:comma int)
        (remove_indices lst indices))
  in
  go [] [];
  [%expect {| [] - [] -> []|}];
  go [] [ 0 ];
  [%expect {| [] - [0] -> []|}];
  go [ 1 ] [ 0 ];
  [%expect {| [1] - [0] -> []|}];
  go [ 1; 2; 3 ] [ 0; 1 ];
  [%expect {| [1, 2, 3] - [0, 1] -> [3]|}];
  go [ 1; 2; 3 ] [ 1; 0 ];
  [%expect {| [1, 2, 3] - [1, 0] -> [3]|}];
  go [ 1; 2; 3; 4; 5; 6 ] [ 0; 2; 4 ];
  [%expect {| [1, 2, 3, 4, 5, 6] - [0, 2, 4] -> [2, 4, 6]|}]

module Binary_contraction : sig
  module Op : sig
    type t =
      | Tensordot1 of int
      | Tensordot2 of (int * int) list
      | Matmul
      | Inner

    val pp : t Fmt.t
  end

  type t = {
    operations : Op.t list;
    contracted : string list;
    zipped: string list;
    batch : string list;
  }

  val pp : t Fmt.t

  val get_result : string list * string list -> string list list -> string list -> t
end = struct
  module Op = struct
    type t =
      | Tensordot1 of int
      | Tensordot2 of (int * int) list
      | Matmul
      | Inner

    let pp ppf = function
      | Tensordot1 ix -> Fmt.pf ppf "tensordot %d" ix
      | Tensordot2 ixs ->
          Fmt.pf ppf "tensordot @[<hov 1>[%a]@]"
            Fmt.(list ~sep:comma (parens (pair int int ~sep:comma)))
            ixs
      | Matmul -> Fmt.string ppf "matmul"
      | Inner -> Fmt.string ppf "inner"
  end

  type t = {
    operations : Op.t list;
    contracted : string list;
    zipped: string list;
    batch : string list;
  }

  let pp ppf { operations; contracted; zipped; batch } =
    let l = Fmt.(list string ~sep:semi) in
    Fmt.pf ppf "operations: @[[%a]@], contracted: @[[%a]@], zipped: @[[%a]@], batch: @[[%a]@]"
      Fmt.(list ~sep:comma Op.pp)
      operations
      l
      contracted
      l
      zipped
      l
      batch

  (* Try matching two inputs and an output to a tensordot operation *)
  let match_tensordot x y z =
    let matches =
      find_matches x y
      (* Don't match indices which should be preserved. *)
      |> List.filter (fun (elem, _, _) -> not (List.mem elem z))
    in
    let remainder =
      remove_indices x (List.map (fun (_, i, _) -> i) matches)
      @ remove_indices y (List.map (fun (_, _, i) -> i) matches)
    in
    if List.(equal String.equal remainder z) then
      match matches with
      | [] -> Some (Op.Tensordot1 0)
      | _ ->
          if matches_simplify matches (List.length x) then
            Some (Op.Tensordot1 (List.length matches))
          else Some (Op.Tensordot2 (List.map (fun (_, a, b) -> (a, b)) matches))
    else None

  let%expect_test "match_tensordot" =
    let go x y z = Fmt.pr "%a@." (Fmt.option Op.pp) (match_tensordot x y z) in
    go [ "a"; "b"; "c" ] [ "d"; "e" ] [ "a"; "b"; "c"; "d"; "e" ];
    [%expect {| tensordot 0 |}];
    go [ "a"; "b"; "c" ] [ "c"; "d" ] [ "a"; "b"; "d" ];
    [%expect {| tensordot 1 |}];
    go [ "a"; "b"; "c" ] [ "c"; "b" ] [ "a" ];
    [%expect {| tensordot 2 |}];
    go [ "a"; "b"; "c" ] [ "b"; "a"; "d" ] [ "c"; "d" ];
    [%expect {| tensordot [(0, 1), (1, 0)] |}];
    go [ "a"; "j"; "k" ] [ "a"; "j"; "k" ] [];
    [%expect {| tensordot [(0, 0), (2, 2), (1, 1)] |}]

  let match_op contracted_tensors result =
    match (contracted_tensors, result) with
    | ( [ x ], [ y ] ), [] when x = y -> Some Op.Inner
    | ( [ x; y ], [ y'; z ] ), [ x'; z' ] when x = x' && y = y' && z = z' ->
        Some Matmul
    | ( x, y ), z -> match_tensordot x y z

  let get_result contracted_tensors other_tensors eventual_result =
    let open String_set in
    let cl, cr = contracted_tensors in
    let cl', cr' = of_list cl, of_list cr in
    let contracted_tensors', other_tensors', eventual_result' =
      ( [cl; cr] |> List.flatten |> of_list,
        other_tensors |> List.flatten |> of_list,
        of_list eventual_result )
    in
    (* Contract dimensions which aren't needed later *)
    let contracted_set =
        (diff
           (union contracted_tensors' other_tensors')
           (union eventual_result' other_tensors'))
    in
    let contracted = elements contracted_set in
    (* Preserve dimensions which are needed later *)
    let preserved =
      inter contracted_tensors' (union eventual_result' other_tensors')
    in
    (* Maintain the order of dimensions in the result if possible so op can be Id. *)
    let preserved =
      if preserved = eventual_result' then eventual_result
      else elements preserved
    in
    (* An axis is a batch axis if it's in one of the inputs, zipped if in both *)
    let zipped, batch = List.partition (fun x -> mem x cl' && mem x cr') preserved in
    let operations =
      match match_op contracted_tensors preserved with
      | Some op -> [ op ]
      | None -> [ (* TODO *) ]
    in
    { operations; contracted; zipped; batch }

  let%expect_test "operations" =
    let go contracted_tensors eventual_result =
      let { operations; _ } =
        get_result contracted_tensors [] eventual_result
      in
      Fmt.pr "%a\n" Fmt.(list ~sep:comma Op.pp) operations
    in
    go ( [ "i" ], [ "i" ] ) [];
    [%expect {| inner |}];
    go ( [ "i"; "j" ], [ "j"; "k" ] ) [ "i"; "k" ];
    [%expect {| matmul |}];
    go ( [ "a"; "b"; "c" ], [ "b"; "a"; "d" ] ) [ "c"; "d" ];
    [%expect {| tensordot [(0, 1), (1, 0)] |}];
    go ( [ "a"; "b"; "c" ], [ "c"; "b" ] ) [ "a" ];
    [%expect {| tensordot 2 |}];
    go ( [ "a"; "b"; "c" ], [ "c"; "d" ] ) [ "a"; "b"; "d" ];
    [%expect {| tensordot 1 |}];
    go ( [ "a"; "b"; "c" ], [ "d"; "e" ] ) [ "a"; "b"; "c"; "d"; "e" ];
    [%expect {| tensordot 0 |}]

  let%expect_test "get_result" =
    let go contracted_tensors other_tensors eventual_result =
      get_result contracted_tensors other_tensors eventual_result
      |> pp Fmt.stdout
    in
    go ( [ "a"; "b" ], [ "c"; "d" ] ) [] [ "a"; "b"; "c"; "d" ];
    [%expect
      {| operations: [tensordot 0], contracted: [], zipped: [], batch: [a; b; c; d] |}];
    go ( [ "a"; "j"; "k" ], [ "a"; "j"; "k" ] ) [] [];
    [%expect
      {|
        operations: [tensordot [(0, 0), (2, 2), (1, 1)]], contracted: [a; j; k], zipped:
          [], batch: [] |}];
    go ( [ "a"; "j"; "k" ], [ "a"; "i"; "j" ] ) [ [ "a"; "i"; "k" ] ] [];
    [%expect {| operations: [], contracted: [j], zipped: [a], batch: [i; k] |}];
    go
      ( [ "n"; "l"; "k" ], [ "i"; "j"; "k" ] )
      [ [ "i"; "l"; "m" ]; [ "n"; "j"; "m" ]; [ "a"; "b"; "c" ] ]
      [ "i"; "n"; "j"; "l" ];
    [%expect {| operations: [], contracted: [k], zipped: [], batch: [i; j; l; n] |}];
    go
      ( [ "n"; "l"; "k" ], [ "i"; "j"; "k" ] )
      [ [ "i"; "l"; "m" ]; [ "n"; "j"; "m" ]; [ "a"; "b"; "c" ] ]
      [ "n"; "l"; "i"; "j" ];
    [%expect
      {| operations: [tensordot [(2, 2)]], contracted: [k], zipped: [], batch: [n; l; i; j] |}];
end

module Unary_contraction : sig
  module Op : sig
    type t =
      | Transpose
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

  val pp : t Fmt.t
end = struct
  module Op = struct
    type t =
      | Transpose
      | Trace
      | Sum
      | Diag
      | Swapaxes
      | Id

    let pp ppf = function
      | Transpose -> Fmt.string ppf "transpose"
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
    let l = Fmt.(list string ~sep:semi) in
    Fmt.pf ppf "operations: @[[%a]@], contracted: @[[%a]@], preserved: @[[%a]@]"
      Fmt.(list ~sep:comma Op.pp)
      operations
      l
      contracted
      l
      preserved

  let match_op contracted_tensor result =
    match (contracted_tensor, result) with
    | x, x' when x = x' -> Some Op.Id
    | [ x; y ], [ y'; x' ] when x = x' && y = y' -> Some Transpose
    | [ x; x' ], [] when x = x' -> Some Trace
    | [ x; x' ], [ x'' ] when x = x' && x = x'' -> Some Diag
    | [ _ ], [] -> Some Sum
    | x, x' when x <> x' && String_set.(equal (of_list x) (of_list x')) ->
        Some Swapaxes
    | _ -> None

  let get_result contracted_tensor other_tensors eventual_result =
    let open String_set in
    let contracted_tensor_set, other_tensors', eventual_result' =
      ( of_list contracted_tensor,
        other_tensors |> List.flatten |> of_list,
        of_list eventual_result )
    in
    (* Contract dimensions which aren't needed later *)
    let contracted =
      elements
        (diff
           (union contracted_tensor_set other_tensors')
           (union eventual_result' other_tensors'))
    in
    (* Preserve dimensions which are needed later *)
    let preserved =
      inter contracted_tensor_set (union eventual_result' other_tensors')
    in
    (* Maintain the order of dimensions in the result if possible so op can be Id. *)
    let preserved =
      if preserved = eventual_result' then eventual_result
      else elements preserved
    in
    let operations =
      match match_op contracted_tensor preserved with
      | Some op -> [ op ]
      | None -> [ (* TODO *) ]
    in
    { operations; contracted; preserved }

  let%expect_test "operations" =
    let go contracted_tensors eventual_result =
      let { operations; _ } =
        get_result contracted_tensors [] eventual_result
      in
      Fmt.pr "%a\n" Fmt.(list ~sep:comma Op.pp) operations
    in
    go [ "i" ] [ "i" ];
    [%expect {| id |}];
    go [ "i"; "i" ] [];
    [%expect {| trace |}];
    go [ "i"; "j" ] [ "i"; "j" ];
    [%expect {| id |}];
    go [ "i"; "j" ] [ "j"; "i" ];
    [%expect {| transpose |}];
    go [ "i" ] [];
    [%expect {| sum |}];
    go [ "i"; "i" ] [ "i" ];
    [%expect {| diag |}];
    go [ "i"; "j"; "k" ] [ "k"; "j"; "i" ];
    [%expect {| swapaxes |}]
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
        Fmt.pf ppf "result = np.zeros((@[%a@]))@."
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

(** The contraction of two tensors as a generalized matmul.

 Example "a b c, a b d -> a c d":
   
   Contract (multiply along axis): b
   Zip (batch axis): a
   Leave (unique to one side): c d
   
   Steps:
   1. Pack tensors (a b c -> a c d)
   2. Perform matmul
   
        a c b
        a b d
     -> a c d
   
   3. Unpack (squeeze): a c b -> a c b

 Example a b, b a -> a
   Pack: 
     a 1 b
     a b 1
   Matmul -> a 1 1
   Unpack a 1 1 -> a

 Example a b, b a ->
   Pack: 
     1 (a b)
     (a b) 1
   Matmul -> 1 1
   Unpack 1 1 -> 1
 *)
module General_matmul : sig
  type packed = { batch_dims : string list; matrix : string list * string list }

  type t = {
    pack : packed * packed;
    view_l : string list list;
    view_r : string list list;
    matmul : string list * string list * string list;
    unpack_squeeze : int list;
  }

  val explain : string list -> string list -> string list -> t
  val pp_expr : t Fmt.t
end = struct
  type packed = { batch_dims : string list; matrix : string list * string list }

  type t = {
    pack : packed * packed;
    view_l : string list list;
    view_r : string list list;
    matmul : string list * string list * string list;
    unpack_squeeze : int list;
  }

  module SS = String_set

  let squeeze ppf i = Fmt.pf ppf ".squeeze(%d)" i

  let shape_slot ppf strs =
    match strs with
    | [] -> Fmt.pf ppf "1"
    | _ -> Fmt.(list ~sep:(any " * ") string) ppf strs

  let shape = Fmt.(parens (list ~sep:comma shape_slot))

  let pp_expr ppf { view_l; view_r; unpack_squeeze; _ } =
    Fmt.(
      pf ppf "torch.matmul(@[<hov 2>x.view%a,@ y.view%a@])%a" shape view_l shape
        view_r (list squeeze) unpack_squeeze)

  let explain input_l input_r output =
    let input_l_s, input_r_s, output_s =
      SS.(of_list input_l, of_list input_r, of_list output)
    in

    let common_inputs = SS.inter input_l_s input_r_s in

    (* Contract (matmul) axes which appear on both inputs but not the output *)
    let contracted = SS.diff common_inputs output_s |> SS.elements in

    (* Zip (batch) axes which are common to both inputs and the output *)
    let batch_dims = SS.inter common_inputs output_s |> SS.elements in

    (* Axes which appear only on the left so will appear in the left matrix *)
    let left_only_input = SS.diff input_l_s common_inputs |> SS.elements in

    (* Axes which appear only on the right so will appear in the right matrix *)
    let right_only_input = SS.diff input_r_s common_inputs |> SS.elements in

    let pack =
      ( { batch_dims; matrix = (left_only_input, contracted) },
        { batch_dims; matrix = (contracted, right_only_input) } )
    in

    (* Preserve batch dimensions, preserve unique axes on both sides *)
    let matmul = (batch_dims, left_only_input, right_only_input) in

    let view_l =
      List.map (fun x -> [ x ]) batch_dims @ [ left_only_input; contracted ]
    in
    let view_r =
      List.map (fun x -> [ x ]) batch_dims @ [ contracted; right_only_input ]
    in

    let n_batch_dims = List.length batch_dims in
    let unpack_squeeze =
      match (left_only_input, right_only_input) with
      | [], [] -> [ n_batch_dims + 1; n_batch_dims ]
      | l, [] -> [ n_batch_dims + List.length l ]
      | [], _ -> [ n_batch_dims ]
      | _, _ -> []
    in

    { pack; view_l; view_r; matmul; unpack_squeeze }

  let%expect_test "General_matmul" =
    let fmt_axis ppf strs =
      match strs with
      | [] -> Fmt.pf ppf "1"
      | [ a ] -> Fmt.string ppf a
      | _ -> Fmt.(parens (list ~sep:sp string)) ppf strs
    in
    let fmt_pack ppf { batch_dims; matrix = l, r } =
      match batch_dims with
      | [] -> Fmt.(pf ppf "%a %a" fmt_axis l fmt_axis r)
      | _ ->
          Fmt.(
            pf ppf "%a %a %a" (list ~sep:sp string) batch_dims fmt_axis l
              fmt_axis r)
    in
    let fmt_matmul ppf (batch_dims, y, z) =
      match batch_dims with
      | [] -> Fmt.(pf ppf "@[%a %a@]" fmt_axis y fmt_axis z)
      | _ ->
          Fmt.(
            pf ppf "@[%a %a %a@]" (list ~sep:sp string) batch_dims fmt_axis y
              fmt_axis z)
    in
    let squeeze ppf i = Fmt.pf ppf "Squeeze %d" i in
    let go input_l input_r output =
      let { pack; view_l; view_r; matmul; unpack_squeeze } =
        explain input_l input_r output
      in
      Fmt.pr "Pack:@.";
      Fmt.(
        pr "  @[View %a: %a -> %a@]@." shape view_l (list ~sep:sp string)
          input_l fmt_pack (fst pack));
      Fmt.(
        pr "  @[View %a: %a -> %a@]@." shape view_r (list ~sep:sp string)
          input_r fmt_pack (snd pack));
      Fmt.pr "Unpack:@.";
      Fmt.(
        pr "  @[%a: %a -> %a@]@."
          (brackets (list ~sep:semi squeeze))
          unpack_squeeze fmt_matmul matmul (list ~sep:sp string) output)
    in
    go [ "a"; "b" ] [ "b"; "a" ] [ "a" ];
    [%expect
      {|
        Pack:
          View (a, 1, b): a b -> a 1 b
          View (a, b, 1): b a -> a b 1
        Unpack:
          [Squeeze 2; Squeeze 1]: a 1 1 -> a
      |}];
    go [ "a"; "b"; "c" ] [ "a"; "b"; "d" ] [ "a"; "c"; "d" ];
    [%expect
      {|
       Pack: 
         View (a, c, b): a b c -> a c b
         View (a, b, d): a b d -> a b d
       Unpack:
         []: a c d -> a c d
    |}];
    go [ "a"; "b" ] [ "b"; "a" ] [];
    [%expect
      {|
       Pack: 
         View (1, a * b): a b -> 1 (a b)
         View (a * b, 1): b a -> (a b) 1
       Unpack:
         [Squeeze 1; Squeeze 0]: 1 1 ->
    |}]
end

module Explain : sig
  val contraction : (string list * string list) * Binary_contraction.t -> string
  (** Explain a single contraction step *)

  val get_contractions :
    ?path:(int * int) list ->
    Rewrite.t ->
    ((string list * string list) * Binary_contraction.t) list
  (** Get the contractions along the given path. *)

  val show_loops : Rewrite.t -> Pyloops.t
  (** Put in [Pyloops.t] format. *)
end = struct
  let get_contractions ?path (bindings, result_group) =
    let n_tensors = List.length bindings in
    let path =
      match path with
      | Some [] | None -> List.init (n_tensors - 1) (fun _ -> (0, 1))
      | Some path -> path
    in
    path
    |> List.fold_left
         (fun (tensors, steps) (ixl, ixr) ->
           let contracted_tensors =
             (List.nth tensors ixl, List.nth tensors ixr)
           in
           (* let contracted_tensors_ = *)
           (*   [ List.nth tensors ixl; List.nth tensors ixr ] *)
           (* in *)
           let new_tensors =
             List.fold_right Util.delete_from_list [ ixl; ixr ] tensors
           in
           let single_contraction =
             Binary_contraction.get_result contracted_tensors new_tensors
               result_group
           in
           let new_tensors =
             List.append new_tensors [ single_contraction.batch @ single_contraction.zipped ]
           in
           let step =
             Binary_contraction.get_result contracted_tensors new_tensors
               result_group
           in
           (new_tensors, List.append steps [ (contracted_tensors, step) ]))
         (bindings, [])
    |> snd

  let contraction
      (((l_tensor, r_tensor), single_contraction) :
        (string list * string list) * Binary_contraction.t) =
    let result_type = single_contraction.batch @ single_contraction.zipped in
    let general_matmul =
      General_matmul.explain l_tensor r_tensor result_type
    in
    Fmt.(
      str "@[<2>contract@ @[%a@] (@[%a, %a@] -> @[%a@])@ (%a)@]"
        (list string ~sep:sp) single_contraction.contracted
        (list string ~sep:sp) l_tensor (list string ~sep:sp) r_tensor
        (list string ~sep:sp) result_type
        General_matmul.pp_expr general_matmul)
  (* (list Single_contraction.Op.pp ~sep:comma)
     single_contraction.operations *)

  let%expect_test "contraction" =
    let go rewrite path =
      get_contractions ~path rewrite
      |> List.map contraction
      |> Fmt.(list ~sep:sp string) Fmt.stdout
    in
    let rewrite =
      ([ [ "a"; "i"; "j" ]; [ "a"; "j"; "k" ]; [ "a"; "i"; "k" ] ], [])
    in
    go rewrite [ (1, 2); (0, 1) ];
    [%expect
      {|
      contract k (a j k, a i k -> a i j) 
        (torch.matmul(x.view(a, j, k), y.view(a, k, i)))
      contract a i j (a i j, a i j -> ) 
        (torch.matmul(x.view(1, a * i * j), y.view(a * i * j, 1)).squeeze(1)
        .squeeze(0))
      |}];
    go rewrite [ (0, 1); (0, 1) ];
    [%expect
      {|
      contract j (a i j, a j k -> a i k) 
        (torch.matmul(x.view(a, i, j), y.view(a, j, k)))
      contract a i k (a i k, a i k -> ) 
        (torch.matmul(x.view(1, a * i * k), y.view(a * i * k, 1)).squeeze(1)
        .squeeze(0))
      |}];
    let rewrite = ([ [ "i"; "k" ]; [ "k"; "j" ] ], [ "i"; "j" ]) in
    go rewrite [ (0, 1) ];
    [%expect
      {| contract k (i k, k j -> i j) (torch.matmul(x.view(i, k), y.view(k, j))) |}]

  let show_loops rewrite =
    let lhs_tensors, rhs_tensor = rewrite in
    Pyloops.
      {
        free_indices = Rewrite.free_indices rewrite;
        summation_indices = Rewrite.summation_indices rewrite;
        lhs_tensors;
        rhs_tensor;
      }

  let%expect_test "show_loops" =
    let go (rewrite : Rewrite.t) =
      show_loops rewrite |> Pyloops.pp Fmt.stdout
    in

    go ([ [ "i"; "k" ]; [ "k"; "j" ] ], [ "i"; "j" ]);
    [%expect
      {|
      result = np.zeros((Ni, Nj))
      for i in range(Ni):
          for j in range(Nj):
              total = 0
              for k in range(Nk):
                  total += A[i, k] * B[k, j]
              result[i, j] = total
      return result
    |}];

    go ([ [ "s" ]; [ "s"; "t" ]; [ "t" ] ], []);
    [%expect
      {|
      total = 0
      for s in range(Ns):
          for t in range(Nt):
              total += A[s] * B[s, t] * C[t]
      return total
    |}];

    go ([ [ "i"; "i" ] ], [ "i" ]);
    [%expect
      {|
      result = np.zeros((Ni))
      for i in range(Ni):
          total = 0
          total += A[i, i]
          result[i] = total
      return result
    |}];

    go ([ [ "i"; "i" ] ], []);
    [%expect
      {|
      total = 0
      for i in range(Ni):
          total += A[i, i]
      return total
    |}];

    go ([ [ "s" ]; [ "s"; "t" ]; [ "t" ] ], []);
    [%expect
      {|
      total = 0
      for s in range(Ns):
          for t in range(Nt):
              total += A[s] * B[s, t] * C[t]
      return total
    |}];

    go ([ [ "b"; "i" ]; [ "b"; "j" ] ], [ "b"; "i"; "j" ]);
    [%expect
      {|
      result = np.zeros((Nb, Ni, Nj))
      for b in range(Nb):
          for i in range(Ni):
              for j in range(Nj):
                  total = 0
                  total += A[b, i] * B[b, j]
                  result[b, i, j] = total
      return result
    |}]
end
