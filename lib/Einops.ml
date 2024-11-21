module String_set = Set.Make (String)
module SS = String_set

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
  type indices = { free : SS.t; summation : SS.t }

  val indices : t -> indices
  val free_indices : t -> SS.t
  val summation_indices : t -> SS.t
  val pp : t Fmt.t
end = struct
  type t = Bindings.t * Group.t
  type indices = { free : SS.t; summation : SS.t }

  let indices (lhs, rhs) =
    let lhs' = lhs |> List.flatten |> SS.of_list in
    let rhs' = rhs |> SS.of_list in
    { free = rhs'; summation = SS.diff lhs' rhs' }

  let free_indices t = (indices t).free
  let summation_indices t = (indices t).summation
  let pp = Fmt.(box (pair ~sep:(any " -> ") Bindings.pp Group.pp))
end

module Find_matches_impl : sig
  val find_matches : string list -> string list -> (string * int * int) list
  (** Given a list of strings, find a maximal set of matching indices, where each position in a list can only match once (in the case of duplicates either match is okay). The lists are not necessarily the same length. *)
end = struct
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
end

open Find_matches_impl

module Matches_simplify_impl : sig
  val matches_simplify : (string * int * int) list -> int -> bool
  (** We can simplify a list of matches into a single index if the count from
     the end of a and the beginning of b simultaneously (and only contain up to
     two matches). *)
end = struct
  let matches_simplify matches len_x =
    List.length matches <= 2
    && List.for_all
         (fun (_, a, b) -> (b = 0 || b = 1) && b = len_x - a - 1)
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
end

open Matches_simplify_impl

module Remove_indices_impl : sig
  val remove_indices : 'a list -> int list -> 'a list
  (** Remove list elements at the given indices *)
end = struct
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
end

open Remove_indices_impl

module Minimum_swaps_impl : sig
  val minimum_swaps : 'a. 'a list -> 'a list -> (int * int) list
end = struct
  let minimum_swaps xs ys =
    let visited = Array.make (List.length xs) false in
    let result = Queue.create () in
    for i = 0 to List.length xs - 1 do
      if visited.(i) then ()
      else
        let j_ref = ref i in
        let cycle = Queue.create () in
        while not visited.(!j_ref) do
          Queue.add !j_ref cycle;
          visited.(!j_ref) <- true;
          let x = List.nth xs !j_ref in
          let new_j = List.find_index (fun y -> y = x) ys in
          match new_j with
          | None -> failwith "Element not found"
          | Some j -> j_ref := j
        done;
        let cycle = cycle |> Queue.to_seq |> List.of_seq |> ref in
        while List.length !cycle > 1 do
          match !cycle with
          | x :: y :: rest ->
              Queue.add (x, y) result;
              cycle := x :: rest
          | _ -> failwith "Cycle too short"
        done
    done;
    result |> Queue.to_seq |> List.of_seq

  let%expect_test "minimum_swaps" =
    let go xs ys =
      minimum_swaps xs ys
      |> Fmt.pr "@[%a@]@." Fmt.(list ~sep:sp (parens (pair ~sep:comma int int)))
    in
    go [ 0; 1; 2 ] [ 1; 0; 2 ];
    [%expect {| (0, 1) |}];
    go [ 0; 1; 2 ] [ 2; 1; 0 ];
    [%expect {| (0, 2) |}];
    go [ 0; 1; 2 ] [ 1; 2; 0 ];
    [%expect {| (0, 2) (0, 1) |}];
    go [ 0; 1; 2; 3 ] [ 1; 0; 3; 2 ];
    [%expect {| (0, 1) (2, 3) |}]
end

open Minimum_swaps_impl

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
    operations : Op.t list;  (** Operations to perform in order *)
    contracted : string list;
    zipped : string list;
    batch : string list;
    result_type : string list;
  }

  val pp : t Fmt.t

  val make :
    string list ->
    string list ->
    other_tensors:string list list ->
    result_type:string list ->
    t
  (** Get a binary contraction.

      @param contracted_tensors The two tensors which are contracted.
      @param other_tensors This is used to tell which dimensions won't be used later so we can contract them.
      @param result_type The type of tensor which should be left.
   *)
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
    zipped : string list;
    batch : string list;
    result_type : string list;
  }

  let pp ppf { operations; contracted; zipped; batch; result_type } =
    let l = Fmt.(list string ~sep:semi) in
    Fmt.pf ppf
      "operations: @[[%a]@], contracted: @[[%a]@], zipped: @[[%a]@], batch: \
       @[[%a]@], result_type: @[[%a]@]"
      Fmt.(list ~sep:comma Op.pp)
      operations l contracted l zipped l batch l result_type

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
    | ([ x ], [ y ]), [] when x = y -> Some Op.Inner
    | ([ x; y ], [ y'; z ]), [ x'; z' ] when x = x' && y = y' && z = z' ->
        Some Matmul
    | (x, y), z -> match_tensordot x y z

  let make cl cr ~other_tensors ~result_type =
    let inter, union, diff = SS.(inter, union, diff) in
    let cl', cr' = SS.(of_list cl, of_list cr) in
    let contracted_tensors_labels, other_tensors_labels, eventual_result_labels
        =
      ( [ cl; cr ] |> List.flatten |> SS.of_list,
        other_tensors |> List.flatten |> SS.of_list,
        SS.of_list result_type )
    in
    (* Contract dimensions which aren't needed later *)
    let contracted_labels =
      diff
        (union contracted_tensors_labels other_tensors_labels)
        (union eventual_result_labels other_tensors_labels)
    in
    let contracted = SS.elements contracted_labels in
    (* Preserve dimensions which are needed later *)
    let preserved =
      inter contracted_tensors_labels
        (union eventual_result_labels other_tensors_labels)
    in
    (* Maintain the order of dimensions in the result if possible so op can be Id. *)
    let preserved =
      if preserved = eventual_result_labels then result_type
      else SS.elements preserved
    in
    (* An axis is a batch axis if it's in one of the inputs, zipped if in both *)
    let zipped, batch =
      List.partition (fun x -> SS.mem x cl' && SS.mem x cr') preserved
    in
    let operations =
      match match_op (cl, cr) preserved with
      | Some op -> [ op ]
      | None -> [ (* TODO *) ]
    in
    { operations; contracted; zipped; batch; result_type }

  let%expect_test "operations" =
    let go l r result_type =
      let { operations; _ } = make l r ~other_tensors:[] ~result_type in
      Fmt.pr "%a\n" Fmt.(list ~sep:comma Op.pp) operations
    in
    go [ "i" ] [ "i" ] [];
    [%expect {| inner |}];
    go [ "i"; "j" ] [ "j"; "k" ] [ "i"; "k" ];
    [%expect {| matmul |}];
    go [ "a"; "b"; "c" ] [ "b"; "a"; "d" ] [ "c"; "d" ];
    [%expect {| tensordot [(0, 1), (1, 0)] |}];
    go [ "a"; "b"; "c" ] [ "c"; "b" ] [ "a" ];
    [%expect {| tensordot 2 |}];
    go [ "a"; "b"; "c" ] [ "c"; "d" ] [ "a"; "b"; "d" ];
    [%expect {| tensordot 1 |}];
    go [ "a"; "b"; "c" ] [ "d"; "e" ] [ "a"; "b"; "c"; "d"; "e" ];
    [%expect {| tensordot 0 |}]

  let%expect_test "make" =
    let go l r other_tensors result_type =
      make l r ~other_tensors ~result_type |> pp Fmt.stdout
    in
    go [ "a"; "b" ] [ "c"; "d" ] [] [ "a"; "b"; "c"; "d" ];
    [%expect
      {| 
      operations: [tensordot 0], contracted: [], zipped: [], batch: [a; b; c; d], result_type: 
      [a; b; c; d] 
      |}];
    go [ "a"; "j"; "k" ] [ "a"; "j"; "k" ] [] [];
    [%expect
      {|
      operations: [tensordot [(0, 0), (2, 2), (1, 1)]], contracted: [a; j; k], zipped:
      [], batch: [], result_type: [] |}];
    go [ "a"; "j"; "k" ] [ "a"; "i"; "j" ] [ [ "a"; "i"; "k" ] ] [];
    [%expect
      {| 
      operations: [], contracted: [j], zipped: [a], batch: [i; k], result_type: 
      [] 
      |}];
    go [ "n"; "l"; "k" ] [ "i"; "j"; "k" ]
      [ [ "i"; "l"; "m" ]; [ "n"; "j"; "m" ]; [ "a"; "b"; "c" ] ]
      [ "i"; "n"; "j"; "l" ];
    [%expect
      {| 
      operations: [], contracted: [k], zipped: [], batch: [i; j; l; n], result_type: 
      [i; n; j; l] 
      |}];
    go [ "n"; "l"; "k" ] [ "i"; "j"; "k" ]
      [ [ "i"; "l"; "m" ]; [ "n"; "j"; "m" ]; [ "a"; "b"; "c" ] ]
      [ "n"; "l"; "i"; "j" ];
    [%expect
      {| 
         operations: [tensordot [(2, 2)]], contracted: [k], zipped: [], batch: 
         [n; l; i; j], result_type: [n; l; i; j] |}]
end

module Unary_contraction : sig
  module Op : sig
    type t =
      | Transpose
      | Trace
      | Sum
      | Diag
      | Swapaxes of (int * int) list
      | Id

    val pp : t Fmt.t
  end

  type t = {
    operations : Op.t list;
    contracted : string list;
    preserved : string list;
  }

  val make : contracted:string list -> result_type:string list -> t
  val pp : t Fmt.t
end = struct
  module Op = struct
    type t =
      | Transpose
      | Trace
      | Sum
      | Diag
      | Swapaxes of (int * int) list
      | Id

    let pp_swap_axis ppf (a, b) = Fmt.pf ppf ".swapaxes(%d, %d)" a b

    let pp ppf = function
      | Transpose -> Fmt.string ppf "x.transpose()"
      | Trace -> Fmt.string ppf "x.trace()"
      | Sum -> Fmt.string ppf "x.sum()"
      | Diag -> Fmt.string ppf "x.diag()"
      | Swapaxes axes -> Fmt.(pf ppf "x%a" (list ~sep:cut pp_swap_axis) axes)
      | Id -> Fmt.string ppf "x"
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
      operations l contracted l preserved

  let match_op contracted_tensor result =
    match (contracted_tensor, result) with
    | x, x' when x = x' -> Some Op.Id
    | [ x; y ], [ y'; x' ] when x = x' && y = y' -> Some Transpose
    | [ x; x' ], [] when x = x' -> Some Trace
    | [ x; x' ], [ x'' ] when x = x' && x = x'' -> Some Diag
    | xs, [] ->
        let unique_labels = SS.of_list xs in
        if SS.cardinal unique_labels = List.length xs then Some Sum else None
    | x, x' when x <> x' && SS.(equal (of_list x) (of_list x')) ->
        (* Find minimal set of swaps *)
        let axes = minimum_swaps x x' in
        Some (Swapaxes axes)
    | _ -> None

  let make ~contracted ~result_type =
    let open SS in
    let contracted_labels, result_labels =
      (SS.of_list contracted, SS.of_list result_type)
    in
    let operations =
      match match_op contracted result_type with
      | Some op -> [ op ]
      | None -> [ (* TODO *) ]
    in
    {
      operations;
      contracted = elements (diff contracted_labels result_labels);
      preserved = elements (inter contracted_labels result_labels);
    }

  let%expect_test "make" =
    let go contracted result_type =
      make ~contracted ~result_type |> pp Fmt.stdout
    in
    go [ "a"; "b"; "c"; "d" ] [ "a"; "b"; "c"; "d" ];
    [%expect {| operations: [x], contracted: [], preserved: [a; b; c; d] |}];
    go [ "i"; "i" ] [];
    [%expect {| operations: [x.trace()], contracted: [i], preserved: [] |}];
    go [ "i"; "i" ] [ "i" ];
    (* XXX should i be included in contracted here? *)
    [%expect {| operations: [x.diag()], contracted: [], preserved: [i] |}];
    go [ "i" ] [];
    [%expect {| operations: [x.sum()], contracted: [i], preserved: [] |}];
    go [ "i"; "j" ] [ "j"; "i" ];
    [%expect
      {| operations: [x.transpose()], contracted: [], preserved: [i; j] |}];
    go [ "i"; "j"; "k" ] [ "k"; "j"; "i" ];
    [%expect
      {| operations: [x.swapaxes(0, 2)], contracted: [], preserved: [i; j; k] |}]

  let%expect_test "operations" =
    let go contracted result_type =
      let { operations; _ } = make ~contracted ~result_type in
      Fmt.pr "%a\n" Fmt.(list ~sep:comma Op.pp) operations
    in
    go [ "i" ] [ "i" ];
    [%expect {| x |}];
    go [ "i"; "i" ] [];
    [%expect {| x.trace() |}];
    go [ "i"; "j" ] [ "i"; "j" ];
    [%expect {| x |}];
    go [ "i"; "j" ] [ "j"; "i" ];
    [%expect {| x.transpose() |}];
    go [ "i" ] [];
    [%expect {| x.sum() |}];
    go [ "i"; "j" ] [];
    [%expect {| x.sum() |}];
    go [ "i"; "i" ] [ "i" ];
    [%expect {| x.diag() |}];
    go [ "i"; "j"; "k" ] [ "k"; "j"; "i" ];
    [%expect {| x.swapaxes(0, 2) |}]
end

module Pyloops = struct
  type t = {
    free_indices : SS.t;
    summation_indices : SS.t;
    lhs_tensors : string list list;
    rhs_tensor : string list;
  }

  let mk_indent indent = String.make (indent * 4) ' '

  let pp ppf { free_indices; summation_indices; lhs_tensors; rhs_tensor } =
    let free_indices = SS.elements free_indices in
    let summation_indices = SS.elements summation_indices in

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
  (** A packed argument to a matmul consists of batch dimensions followed by two matrix dimensions. *)

  type t = {
    pack : packed * packed;  (** First pack both arguments to the matmul. *)
    view_l : string list list;
        (** Instructions for packing the left tensor. Batch dimensions followed by matrix dimensions. *)
    view_r : string list list;  (** See [view_l] *)
    matmul : string list * string list * string list;
        (** Output of matmul, before unpacking. Batch dimensions, followed by matrix dimensions. *)
    unpack_squeeze : int list;  (** List of dimensions to squeeze. *)
  }

  val make : string list -> string list -> string list -> t
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

  let%expect_test "print squeeze" =
    Fmt.pr "@[%a@]@." (Fmt.list squeeze) [ 1; 2; 3 ];
    [%expect {| .squeeze(1).squeeze(2).squeeze(3) |}]

  let make input_l input_r output =
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
        make input_l input_r output
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
    go [ "a"; "b" ] [ "b"; "a" ] [ "a"; "a" ];
    [%expect
      {|
        Pack:
          View (a, b): a b -> a b
          View (b, a): b a -> b a
        Unpack:
          []: a a -> a a
      |}];
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
  type contraction =
    | Unary_contraction of string list * Unary_contraction.t
    | Binary_contraction of string list * string list * Binary_contraction.t

  val get_contractions : ?path:(int * int) list -> Rewrite.t -> contraction list
  (** Get the contractions along the given path. *)

  val show_loops : Rewrite.t -> Pyloops.t
  (** Put in [Pyloops.t] format. *)
end = struct
  type contraction =
    | Unary_contraction of string list * Unary_contraction.t
    | Binary_contraction of string list * string list * Binary_contraction.t

  let get_contractions ?path (bindings, result_group) =
    let n_tensors = List.length bindings in

    if n_tensors = 1 then
      let tensor = List.hd bindings in
      [
        Unary_contraction
          ( tensor,
            Unary_contraction.make ~contracted:tensor ~result_type:result_group
          );
      ]
    else
      let path =
        match path with
        | Some [] | None -> List.init (n_tensors - 1) (fun _ -> (0, 1))
        | Some path -> path
      in
      path
      |> List.fold_left
           (fun (tensors, steps) (ixl, ixr) ->
             let cl, cr = List.(nth bindings ixl, nth bindings ixr) in
             let new_tensors =
               List.fold_right Util.delete_from_list [ ixl; ixr ] tensors
             in
             (* XXX two calls to Binary_contraction.make *)
             let single_contraction =
               Binary_contraction.make cl cr ~other_tensors:new_tensors
                 ~result_type:result_group
             in
             let new_tensors =
               List.append new_tensors
                 [ single_contraction.batch @ single_contraction.zipped ]
             in
             let step =
               Binary_contraction.make cl cr ~other_tensors:new_tensors
                 ~result_type:result_group
             in
             ( new_tensors,
               List.append steps [ Binary_contraction (cl, cr, step) ] ))
           (bindings, [])
      |> snd

  let pp_explain_binary_contraction ppf
      ((l_tensor, r_tensor, single_contraction) :
        string list * string list * Binary_contraction.t) =
    let general_matmul =
      General_matmul.make l_tensor r_tensor single_contraction.result_type
    in
    let l = Fmt.(list string ~sep:sp) in
    Fmt.(
      pf ppf "@[<2>contract@ @[%a@] (@[%a, %a@] -> @[%a@])@ (%a)@]" l
        single_contraction.contracted l l_tensor l r_tensor l
        single_contraction.result_type General_matmul.pp_expr general_matmul)

  let pp_explain_unary_contraction ppf
      (tensor, Unary_contraction.{ operations; contracted; preserved }) =
    let l = Fmt.(list string ~sep:sp) in
    Fmt.(
      pf ppf "@[<2>contract@ %a (@[%a@] -> @[%a@])@ (%a)@]" l contracted l
        tensor l preserved
        (list ~sep:sp Unary_contraction.Op.pp)
        operations)

  let%expect_test "explain contractions" =
    let go rewrite path =
      get_contractions ~path rewrite
      |> List.iter (function
           | Binary_contraction (l, r, c) ->
               Fmt.pr "@[%a@]@." pp_explain_binary_contraction (l, r, c)
           | Unary_contraction (tensor, unary_contraction) ->
               Fmt.pr "@[%a@]@." pp_explain_unary_contraction
                 (tensor, unary_contraction))
    in
    let rewrite =
      ([ [ "a"; "i"; "j" ]; [ "a"; "j"; "k" ]; [ "a"; "i"; "k" ] ], [])
    in
    go rewrite [ (1, 2); (0, 1) ];
    [%expect
      {|
      contract k (a j k, a i k -> a i j) 
        (torch.matmul(x, y.view(0, 2, 1)))
      contract a i j (a i j, a i j -> ) 
        ((x * y).sum())
      |}];
    go rewrite [ (0, 1); (0, 1) ];
    [%expect
      {|
      contract j (a i j, a j k -> a i k) 
        (torch.matmul(x, y.view(0, 2, 1)))
      contract a i k (a i k, a i k -> ) 
        ((x * y).sum())
      |}];
    let rewrite = ([ [ "i"; "k" ]; [ "k"; "j" ] ], [ "i"; "j" ]) in
    go rewrite [ (0, 1) ];
    [%expect {| contract k (i k, k j -> i j) (torch.matmul(x, y.view(1, 0))) |}];
    let rewrite = ([ [ "i"; "i" ] ], []) in
    go rewrite [];
    [%expect {| contract i (i i -> ) (x.trace()) |}]

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
