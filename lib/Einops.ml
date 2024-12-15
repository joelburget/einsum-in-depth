module String_set = Set.Make (String)
module String_map = Map.Make (String)
module SS = String_set
module SM = String_map

type code_backend = Numpy | Pytorch
type Format.stag += Colored of string

module PP_var_impl : sig
  type get_color = string -> string

  val pp_var : get_color -> string Fmt.t
end = struct
  type get_color = string -> string

  let pp_var get_color ppf var =
    let color = get_color var in
    Format.pp_open_stag ppf (Colored color);
    Fmt.pf ppf "%s" var;
    Format.pp_close_stag ppf ()
end

include PP_var_impl

(** A Group is the set of indices of a tensor. *)
module Group = struct
  type t = string list

  let pp_friendly = Fmt.(box (list ~sep:sp string))
  let pp_original ppf group = Fmt.string ppf (String.concat "" group)
end

(** Bindings are the left-hand side of a rewrite. *)
module Bindings = struct
  type t = Group.t list

  let pp_friendly = Fmt.(box (list ~sep:comma Group.pp_friendly))
  let pp_original = Fmt.(box (list ~sep:comma Group.pp_original))
end

(** A Rewrite binds some groups of tensor indices and results in some tensor indices. *)
module Rewrite : sig
  type t = Bindings.t * Group.t
  type indices = { free : SS.t; summation : SS.t }

  val indices : t -> indices
  val free_indices : t -> SS.t
  val summation_indices : t -> SS.t
  val pp_friendly : t Fmt.t
  val pp_original : t Fmt.t

  val to_original : t -> t
  (** Convert a "friendly" Einsum into one compatible with the original format by making every identifier a single character. *)

  val validate : t -> string option
end = struct
  type t = Bindings.t * Group.t
  type indices = { free : SS.t; summation : SS.t }

  let indices (lhs, rhs) =
    let lhs' = lhs |> List.flatten |> SS.of_list in
    let rhs' = rhs |> SS.of_list in
    { free = rhs'; summation = SS.diff lhs' rhs' }

  let free_indices t = (indices t).free
  let summation_indices t = (indices t).summation

  let pp_friendly =
    Fmt.(box (pair ~sep:(any " -> ") Bindings.pp_friendly Group.pp_friendly))

  let pp_original =
    Fmt.(box (pair ~sep:(any " -> ") Bindings.pp_original Group.pp_original))

  let assign_chars strs =
    let used = ref [] in
    let pick_char s =
      let first = s.[0] in
      (* Try first char first *)
      if not (List.mem first !used) then (
        used := first :: !used;
        String.make 1 first)
      else
        (* If first is taken, pick the next available letter in the alphabet *)
        let rec try_alphabet c =
          if c > 'z' then failwith "Ran out of letters"
          else if List.mem c !used then
            try_alphabet (Char.chr (Char.code c + 1))
          else (
            used := c :: !used;
            String.make 1 c)
        in
        try_alphabet (Char.chr (Char.code 'a'))
    in
    List.map pick_char strs

  let to_original (lhs, rhs) =
    let { free; summation } = indices (lhs, rhs) in
    let all_indices = SS.union free summation |> SS.to_list in
    let assigned_chars = assign_chars all_indices in
    let assoc_list = List.combine all_indices assigned_chars in
    let find_char x = List.assoc x assoc_list in
    (List.map (List.map find_char) lhs, List.map find_char rhs)

  let validate (lhs, rhs) =
    let lhs_set = lhs |> List.flatten |> SS.of_list in
    let rhs_set = rhs |> SS.of_list in
    let rhs_extras = SS.diff rhs_set lhs_set in
    if not (SS.is_empty rhs_extras) then
      Some
        Fmt.(
          str
            "Result indices must be a subset of the input indices ([@[%a@]] \
             are not)"
            (list ~sep:comma string) (SS.to_list rhs_extras))
    else
      let repeated_in_rhs =
        rhs |> Counter.make |> Counter.to_list
        |> List.filter (fun (_, n) -> n > 1)
        |> List.map fst
      in
      if repeated_in_rhs <> [] then
        Some
          Fmt.(
            str "Result indices must not be repeated ([@[%a@]])"
              (list ~sep:comma string) repeated_in_rhs)
      else None
end

module Find_matches_impl : sig
  val find_matches : string list -> string list -> (string * int * int) list
  (** Given a list of strings, find a maximal set of matching indices, where each position in a list can only match once (in the case of duplicates either match is okay). The lists are not necessarily the same length. *)
end = struct
  let rec safe_map2 f xs ys =
    match (xs, ys) with
    | [], _ | _, [] -> []
    | x :: xs, y :: ys -> f x y :: safe_map2 f xs ys

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
          let match_pairs = safe_map2 (fun x y -> (k, x, y)) v1 v2 in
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
    [%expect {| [a; j; k], [a; j; k] -> [(a, 0, 0); (k, 2, 2); (j, 1, 1)]|}];
    go [ "a"; "a" ] [ "a" ];
    [%expect {| [a; a], [a] -> [(a, 1, 0)]|}]
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

module Index_helpers : sig
  val indexof : 'a -> 'a list -> int option
  val first_repeat_indices : 'a list -> ('a * int * int) option
  val has_repeat_indices : 'a list -> bool
end = struct
  let indexof x lst =
    let rec go i = function
      | [] -> None
      | y :: ys -> if x = y then Some i else go (i + 1) ys
    in
    go 0 lst

  let first_repeat_indices lst =
    let rec go i = function
      | [] -> None
      | x :: xs -> (
          match indexof x xs with
          | None -> go (i + 1) xs
          | Some j -> Some (x, i, i + j + 1))
    in
    go 0 lst

  let has_repeat_indices lst =
    match first_repeat_indices lst with Some _ -> true | None -> false

  let%expect_test "first_repeat_indices" =
    let go lst =
      match first_repeat_indices lst with
      | None -> Fmt.pr "None@."
      | Some (x, i, j) -> Fmt.pr "(%s, %d, %d)@." x i j
    in
    go [ "a"; "b"; "c" ];
    [%expect {| None |}];
    go [ "a"; "b"; "a" ];
    [%expect {| (a, 0, 2) |}];
    go [ "a"; "b"; "c"; "a" ];
    [%expect {| (a, 0, 3) |}];
    go [ "a"; "a" ];
    [%expect {| (a, 0, 1) |}]
end

open Index_helpers

module Binary_contraction : sig
  module Op : sig
    type t =
      | Tensordot1 of int
          (** Tensordot operation with the number of dimensions to contract *)
      | Tensordot2 of (int * int) list
          (** Tensordot operation with an explicit list of dimensions to contract *)
      | Matmul
      | Inner
      | Mul  (** Multiply two matrices (pointwise) *)

    val pp : code_backend -> 'a Fmt.t -> 'a -> 'b Fmt.t -> 'b -> t Fmt.t
  end

  module Diagonal : sig
    type t = { dim1 : int; dim2 : int }

    val pp : code_backend -> t Fmt.t
  end

  module Ops : sig
    type t = { diag_l : Diagonal.t list; diag_r : Diagonal.t list; op : Op.t }

    val pp : code_backend -> t Fmt.t
  end

  type t = {
    l : string list;
    r : string list;
    operations : Ops.t option;  (** Operations to perform in order *)
    aligned : string list;
    contracted : string list;
    result_type : string list;
  }

  val pp : code_backend -> t Fmt.t

  val make :
    string list ->
    string list ->
    other_tensors:string list list ->
    target:string list ->
    t
  (** Construct a binary contraction.

      @param contracted_tensors The two tensors to be contracted.
      @param other_tensors This is used to tell which dimensions will / won't be used later so we can preserve / contract them.
      @param target The eventual target tensor (after other contractions if other_tensors is not empty)
   *)
end = struct
  module Op = struct
    type t =
      | Tensordot1 of int
      | Tensordot2 of (int * int) list
      | Matmul
      | Inner
      | Mul

    let pp backend pp_arg1 arg1 pp_arg2 arg2 ppf op =
      let backend_name =
        match backend with Numpy -> "np" | Pytorch -> "torch"
      in
      match op with
      | Tensordot1 ix ->
          if ix = 2 then
            Fmt.pf ppf "%s.tensordot(@[%a,@ %a@])" backend_name pp_arg1 arg1
              pp_arg2 arg2
          else
            Fmt.pf ppf "%s.tensordot(@[%a,@ %a,@ %d@])" backend_name pp_arg1
              arg1 pp_arg2 arg2 ix
      | Tensordot2 ixs ->
          Fmt.pf ppf "%s.tensordot(@[<hov 1>%a,@ %a,@ [%a]@])" backend_name
            pp_arg1 arg1 pp_arg2 arg2
            Fmt.(list ~sep:comma (parens (pair int int ~sep:comma)))
            ixs
      | Matmul -> Fmt.pf ppf "(@[%a@ %@@ %a@])" pp_arg1 arg1 pp_arg2 arg2
      | Inner ->
          Fmt.pf ppf "%s.inner(@[%a,@ %a@])" backend_name pp_arg1 arg1 pp_arg2
            arg2
      | Mul -> Fmt.pf ppf "(@[%a * %a@])" pp_arg1 arg1 pp_arg2 arg2
  end

  module Diagonal = struct
    type t = { dim1 : int; dim2 : int }

    let pp backend ppf { dim1; dim2 } =
      match (backend, dim1, dim2) with
      | _, 0, 1 -> Fmt.pf ppf ".diagonal()"
      | Numpy, _, _ -> Fmt.pf ppf ".diagonal(@[axis1=%d,@ axis2=%d@])" dim1 dim2
      | Pytorch, _, _ -> Fmt.pf ppf ".diagonal(@[dim1=%d,@ dim2=%d@])" dim1 dim2
  end

  module Ops = struct
    type t = { diag_l : Diagonal.t list; diag_r : Diagonal.t list; op : Op.t }

    let pp_diag_list arg_name backend ppf =
      Fmt.(pf ppf "%s%a" arg_name (list ~sep:cut (Diagonal.pp backend)))

    let pp backend ppf { diag_l; diag_r; op } =
      Op.pp backend (pp_diag_list "x" backend) diag_l (pp_diag_list "y" backend)
        diag_r ppf op
  end

  type t = {
    l : string list;
    r : string list;
    operations : Ops.t option;
    aligned : string list;
    contracted : string list;
    result_type : string list;
  }

  let debug_op_pp = Op.pp Numpy Fmt.string "x" Fmt.string "y"

  let pp backend ppf { l; r; operations; aligned; contracted; result_type } =
    let lst = Fmt.(list string ~sep:semi) in
    Fmt.pf ppf
      "@[l: @[[%a]@], r: @[[%a]@], operations: @[[%a]@], aligned: @[[%a]@], \
       contracted: @[[%a]@], result_type: @[[%a]@]@]"
      lst l lst r
      Fmt.(option (Ops.pp backend))
      operations lst aligned lst contracted lst result_type

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
    if List.equal String.equal remainder z then
      match matches with
      | [] -> Some (Op.Tensordot1 0)
      | _ ->
          if matches_simplify matches (List.length x) then
            Some (Op.Tensordot1 (List.length matches))
          else Some (Op.Tensordot2 (List.map (fun (_, a, b) -> (a, b)) matches))
    else None

  let%expect_test "match_tensordot" =
    let go x y z =
      Fmt.pr "%a@." (Fmt.option debug_op_pp) (match_tensordot x y z)
    in
    go [ "a"; "b"; "c" ] [ "d"; "e" ] [ "a"; "b"; "c"; "d"; "e" ];
    [%expect {| np.tensordot(x, y, 0) |}];
    go [ "a"; "b"; "c" ] [ "c"; "d" ] [ "a"; "b"; "d" ];
    [%expect {| np.tensordot(x, y, 1) |}];
    go [ "a"; "b"; "c" ] [ "c"; "b" ] [ "a" ];
    [%expect {| np.tensordot(x, y) |}];
    go [ "a"; "b"; "c" ] [ "b"; "a"; "d" ] [ "c"; "d" ];
    [%expect {| np.tensordot(x, y, [(0, 1), (1, 0)]) |}];
    go [ "a"; "j"; "k" ] [ "a"; "j"; "k" ] [];
    [%expect {| np.tensordot(x, y, [(0, 0), (2, 2), (1, 1)]) |}]

  let rec match_ops l r result =
    match first_repeat_indices l with
    | Some (x, dim1, dim2) -> (
        let l = remove_indices l [ dim1; dim2 ] @ [ x ] in
        match match_ops l r result with
        | None -> None
        | Some Ops.{ diag_l; diag_r; op } ->
            Some Ops.{ diag_l = Diagonal.{ dim1; dim2 } :: diag_l; diag_r; op })
    | None -> (
        match first_repeat_indices r with
        | Some (x, dim1, dim2) -> (
            let r = remove_indices r [ dim1; dim2 ] @ [ x ] in
            match match_ops l r result with
            | None -> None
            | Some { diag_l; diag_r; op } ->
                Some
                  Ops.{ diag_l; diag_r = Diagonal.{ dim1; dim2 } :: diag_r; op }
            )
        | None -> (
            let op =
              if l = r && r = result then Some Op.Mul
              else
                match (l, r, result) with
                | [ x ], [ y ], [] when x = y -> Some Op.Inner
                | [ x; y ], [ y'; z ], [ x'; z' ]
                  when x = x' && y = y' && z = z' ->
                    Some Matmul
                | x, y, z -> match_tensordot x y z
            in
            match op with
            | Some op -> Some Ops.{ diag_l = []; diag_r = []; op }
            | None -> None))

  let make l r ~other_tensors ~target =
    let inter, union, diff = SS.(inter, union, diff) in
    let l_set, r_set = SS.(of_list l, of_list r) in
    let input_tensor_labels, other_tensors_labels, target_labels =
      ( SS.of_list (l @ r),
        other_tensors |> List.flatten |> SS.of_list,
        SS.of_list target )
    in
    let dimensions_seen_later = union target_labels other_tensors_labels in
    (* Contract dimensions which aren't needed later *)
    let contracted =
      diff input_tensor_labels dimensions_seen_later
      |> SS.elements |> List.sort compare
    in
    let aligned = inter l_set r_set |> SS.elements |> List.sort compare in
    let result_type =
      inter input_tensor_labels dimensions_seen_later
      |> SS.elements |> List.sort compare
    in
    let operations = match_ops l r result_type in
    { l; r; operations; contracted; aligned; result_type }

  let%expect_test "operations" =
    let go l r target =
      let { operations; _ } = make l r ~other_tensors:[] ~target in
      Fmt.pr "%a\n" Fmt.(option (Ops.pp Numpy)) operations
    in
    go [ "i" ] [ "i" ] [];
    [%expect {| np.inner(x, y) |}];
    go [ "i"; "j" ] [ "j"; "k" ] [ "i"; "k" ];
    [%expect {| (x @ y) |}];
    go [ "a"; "b"; "c" ] [ "b"; "a"; "d" ] [ "c"; "d" ];
    [%expect {| np.tensordot(x, y, [(0, 1), (1, 0)]) |}];
    go [ "a"; "b"; "c" ] [ "c"; "b" ] [ "a" ];
    [%expect {| np.tensordot(x, y) |}];
    go [ "a"; "b"; "c" ] [ "c"; "d" ] [ "a"; "b"; "d" ];
    [%expect {| np.tensordot(x, y, 1) |}];
    go [ "a"; "b"; "c" ] [ "d"; "e" ] [ "a"; "b"; "c"; "d"; "e" ];
    [%expect {| np.tensordot(x, y, 0) |}]

  let%expect_test "make" =
    let go l r other_tensors target =
      make l r ~other_tensors ~target |> pp Numpy Fmt.stdout
    in
    go [ "a"; "c" ] [ "c"; "d" ] [] [ "a"; "d" ];
    [%expect
      {|
      l: [a; c], r: [c; d], operations: [(x @ y)], aligned: [c], contracted:
      [c], result_type: [a; d]
    |}];
    go [ "a"; "b" ] [ "c"; "d" ] [] [ "a"; "b"; "c"; "d" ];
    [%expect
      {| 
      l: [a; b], r: [c; d], operations: [np.tensordot(x, y, 0)], aligned: [], contracted:
      [], result_type: [a; b; c; d] |}];
    go [ "i"; "i" ] [ "i" ] [] [ "i" ];
    [%expect
      {|
      l: [i; i], r: [i], operations: [(x.diagonal() * y)], aligned: [i], contracted:
      [], result_type: [i]
      |}];
    (* This could also be interpreted as `matmul; diag`, probably other ways *)
    go [ "a"; "a" ] [ "a"; "a" ] [] [ "a" ];
    [%expect
      {|
      l: [a; a], r: [a; a], operations: [(x.diagonal() * y.diagonal())], aligned:
      [a], contracted: [], result_type: [a]
      |}];
    (* m3.diagonal(0, 0, 1).diagonal(0, 0, 1) * v *)
    go [ "a"; "a"; "a" ] [ "a" ] [] [ "a" ];
    [%expect
      {|
      l: [a; a; a], r: [a], operations: [(x.diagonal().diagonal() * y)], aligned:
      [a], contracted: [], result_type: [a]
      |}];
    (* m3.diagonal(0, 0, 1).diagonal(0, 0, 1) * m.diagonal() *)
    go [ "a"; "a"; "a" ] [ "a"; "a" ] [] [];
    [%expect
      {|
      l: [a; a; a], r: [a; a], operations: [np.inner(x.diagonal().diagonal(),
                                                     y.diagonal())], aligned:
      [a], contracted: [a], result_type: []
      |}];
    go [ "a"; "j"; "k" ] [ "a"; "j"; "k" ] [] [];
    [%expect
      {|
      l: [a; j; k], r: [a; j; k], operations: [np.tensordot(x, y, [(0, 0), 
                                                             (2, 2), (1, 1)])], aligned:
      [a; j; k], contracted: [a; j; k], result_type: [] |}];
    go [ "a"; "j"; "k" ] [ "a"; "i"; "j" ] [ [ "a"; "i"; "k" ] ] [];
    [%expect
      {|
      l: [a; j; k], r: [a; i; j], operations: [], aligned: [a; j], contracted:
      [j], result_type: [a; i; k]
      |}];
    go [ "n"; "l"; "k" ] [ "i"; "j"; "k" ]
      [ [ "i"; "l"; "m" ]; [ "n"; "j"; "m" ]; [ "a"; "b"; "c" ] ]
      [ "i"; "n"; "j"; "l" ];
    [%expect
      {|
      l: [n; l; k], r: [i; j; k], operations: [], aligned: [k], contracted:
      [k], result_type: [i; j; l; n] 
      |}];
    go [ "n"; "l"; "k" ] [ "i"; "j"; "k" ]
      [ [ "i"; "l"; "m" ]; [ "n"; "j"; "m" ]; [ "a"; "b"; "c" ] ]
      [ "n"; "l"; "i"; "j" ];
    [%expect
      {|
      l: [n; l; k], r: [i; j; k], operations: [], aligned: [k], contracted:
      [k], result_type: [i; j; l; n]
      |}]
end

module Find_diag_impl : sig
  val find_diag : string list -> string list -> (int * int) option
end = struct
  type status = None_found | Found of string | Error of string

  let find_missing_label l r =
    let l_set = SS.of_list l in
    let r_set = SS.of_list r in
    (* Fail if the right tensor has *any* labels the left doesn't. *)
    if SS.(cardinal (diff r_set l_set)) > 0 then None
    else
      let count_l = Counter.make l in
      let count_r = Counter.make r in
      let diff = Counter.diff count_l count_r in
      let status =
        SM.fold
          (fun k diff acc ->
            match acc with
            | Error _ -> acc
            | _ ->
                if diff = 1 then
                  match acc with
                  | None_found -> Found k
                  | _ -> Error "found multiple missing labels"
                else Error "found a diff that wasn't 0 or 1")
          diff None_found
      in
      match status with Found x -> Some x | _ -> None

  let%expect_test "find_missing_label" =
    let go l r =
      match find_missing_label l r with
      | None -> Fmt.pr "None@."
      | Some x -> Fmt.pr "%s@." x
    in
    go [ "a"; "b"; "c" ] [ "a"; "b" ];
    [%expect {| c |}];
    go [ "a"; "b" ] [ "a"; "b" ];
    [%expect {| None |}];
    go [ "a"; "c" ] [ "a"; "b" ];
    [%expect {| None |}];
    go [ "a"; "a" ] [ "a" ];
    [%expect {| a |}];
    go [ "a"; "a"; "a" ] [ "a" ];
    [%expect {| None |}];
    go [ "a"; "a"; "b" ] [ "a"; "b" ];
    [%expect {| a |}]

  let find_indices lst target =
    let rec go i = function
      | [] -> []
      | x :: xs ->
          let rest = go (i + 1) xs in
          if x = target then i :: rest else rest
    in
    go 0 lst

  let%expect_test "find_indices" =
    let go lst target =
      find_indices lst target |> Fmt.pr "@[[%a]@]@." Fmt.(list ~sep:semi int)
    in
    go [ 1; 2; 3; 2; 1 ] 2;
    [%expect {| [1; 3] |}];
    go [ 1; 2; 3; 2; 1 ] 1;
    [%expect {| [0; 4] |}];
    go [ 1; 2; 3; 2; 1 ] 4;
    [%expect {| [] |}]

  let find_diag l r =
    match find_missing_label l r with
    | None -> None
    | Some label -> (
        match find_indices l label with i :: j :: _ -> Some (i, j) | _ -> None)
end

open Find_diag_impl

module Unary_contraction : sig
  module Op : sig
    type t =
      | Transpose
      | Trace
      | Sum
      | Diagonal of { dim1 : int; dim2 : int }
      | Swapaxes of (int * int) list
      | Id

    val pp : code_backend -> t Fmt.t
  end

  type t = {
    operations : Op.t list;
    contracted : string list;
    preserved : string list;
    result_type : string list;
  }

  val make : contracted:string list -> result_type:string list -> t
  val pp : t Fmt.t
  val pp_ops : code_backend -> Op.t list Fmt.t
end = struct
  module Op = struct
    type t =
      | Transpose
      | Trace
      | Sum
      | Diagonal of { dim1 : int; dim2 : int }
      | Swapaxes of (int * int) list
      | Id

    let pp_swap_axis ppf (a, b) = Fmt.pf ppf ".swapaxes(%d, %d)" a b

    let pp backend ppf = function
      | Transpose ->
          if backend = Numpy then Fmt.string ppf ".T" else Fmt.string ppf ".t()"
      | Trace -> Fmt.string ppf ".trace()"
      | Sum -> Fmt.string ppf ".sum()"
      | Diagonal { dim1; dim2 } ->
          if backend = Numpy then
            Fmt.pf ppf ".diagonal(axis1=%d, axis2=%d)" dim1 dim2
          else Fmt.pf ppf ".diagonal(dim1=%d, dim2=%d)" dim1 dim2
      | Swapaxes axes -> Fmt.(pf ppf "%a" (list ~sep:cut pp_swap_axis) axes)
      | Id -> Fmt.string ppf ""
  end

  type t = {
    operations : Op.t list;
    contracted : string list;
    preserved : string list;
    result_type : string list;
  }

  let op_pp_numpy = Op.pp Numpy

  let pp ppf { operations; contracted; preserved; result_type = _ } =
    let l = Fmt.(list string ~sep:semi) in
    Fmt.pf ppf "operations: @[[%a]@], contracted: @[[%a]@], preserved: @[[%a]@]"
      Fmt.(list ~sep:comma op_pp_numpy)
      operations l contracted l preserved

  let pp_ops backend ppf ops =
    Fmt.(pf ppf "x%a" (list ~sep:cut (Op.pp backend)) ops)

  let match_op contracted_tensor result =
    match find_diag contracted_tensor result with
    | Some (dim1, dim2) -> Some (Op.Diagonal { dim1; dim2 })
    | None -> (
        match (contracted_tensor, result) with
        | x, x' when x = x' -> Some Id
        | [ x; y ], [ y'; x' ] when x = x' && y = y' -> Some Transpose
        | [ x; x' ], [] when x = x' -> Some Trace
        | xs, [] -> if has_repeat_indices xs then None else Some Sum
        | x, x' when x <> x' && SS.(equal (of_list x) (of_list x')) ->
            (* Find minimal set of swaps *)
            let axes = minimum_swaps x x' in
            Some (Swapaxes axes)
        | _ -> None)

  let make ~contracted ~result_type =
    let operations =
      match match_op contracted result_type with
      | Some op -> [ op ]
      | None -> [ (* TODO *) ]
    in
    let diffs = Counter.(diff (make contracted) (make result_type)) in
    let contracted_labels = Counter.key_set diffs in
    let preserved =
      SS.(diff (of_list contracted) contracted_labels |> to_list)
    in
    let contracted = SS.elements contracted_labels in
    { operations; contracted; preserved; result_type }

  let%expect_test "make" =
    let go contracted result_type =
      make ~contracted ~result_type |> pp Fmt.stdout
    in
    go [ "a"; "b"; "c"; "d" ] [ "a"; "b"; "c"; "d" ];
    [%expect {| operations: [], contracted: [], preserved: [a; b; c; d] |}];
    go [ "i"; "i" ] [];
    [%expect {| operations: [.trace()], contracted: [i], preserved: [] |}];
    go [ "i"; "i" ] [ "i" ];
    [%expect
      {| 
      operations: [.diagonal(axis1=0, axis2=1)], contracted: [i], preserved: 
      [] 
      |}];
    go [ "i" ] [];
    [%expect {| operations: [.sum()], contracted: [i], preserved: [] |}];
    go [ "i"; "j" ] [ "j"; "i" ];
    [%expect {| operations: [.T], contracted: [], preserved: [i; j] |}];
    go [ "i"; "j"; "k" ] [ "k"; "j"; "i" ];
    [%expect
      {| operations: [.swapaxes(0, 2)], contracted: [], preserved: [i; j; k] |}]

  let%expect_test "operations" =
    let go contracted result_type =
      let { operations; _ } = make ~contracted ~result_type in
      Fmt.pr "%a\n" Fmt.(list ~sep:comma op_pp_numpy) operations
    in
    go [ "i" ] [ "i" ];
    [%expect {| |}];
    go [ "i"; "i" ] [];
    [%expect {| .trace() |}];
    go [ "i"; "j" ] [ "i"; "j" ];
    [%expect {| |}];
    go [ "i"; "j" ] [ "j"; "i" ];
    [%expect {| .T |}];
    go [ "i" ] [];
    [%expect {| .sum() |}];
    go [ "i"; "j" ] [];
    [%expect {| .sum() |}];
    go [ "i"; "i" ] [ "i" ];
    [%expect {| .diagonal(axis1=0, axis2=1) |}];
    go [ "i"; "j"; "k" ] [ "k"; "j"; "i" ];
    [%expect {| .swapaxes(0, 2) |}]
end

module Pyloops : sig
  type t = {
    free_indices : SS.t;
    summation_indices : SS.t;
    lhs_tensors : string list list;
    rhs_tensor : string list;
  }

  val pp : ?use_frob:code_backend -> get_color -> t Fmt.t
end = struct
  type t = {
    free_indices : SS.t;
    summation_indices : SS.t;
    lhs_tensors : string list list;
    rhs_tensor : string list;
  }

  let mk_indent indent = String.make (indent * 4) ' '

  let pp ?use_frob get_color ppf
      {
        free_indices;
        summation_indices = summation_indices_set;
        lhs_tensors;
        rhs_tensor;
      } =
    let free_indices = SS.elements free_indices in
    let summation_indices = SS.elements summation_indices_set in
    let pp_var = pp_var get_color in

    let pp_d_var ppf var = Fmt.pf ppf "d_%a" pp_var var in

    (* initialize result *)
    (match rhs_tensor with
    | [] -> ()
    | _ ->
        Fmt.pf ppf "result = np.zeros((@[%a@]))@."
          Fmt.(list ~sep:comma pp_d_var)
          rhs_tensor);

    (* loops *)
    (match free_indices with
    | [] -> ()
    | _ -> Fmt.pf ppf "# Loop over all free indices@.");
    let outer_indent =
      List.fold_left
        (fun indent index ->
          Fmt.pf ppf "%sfor %a in range(%a):@." (mk_indent indent) pp_var index
            pp_d_var index;
          indent + 1)
        0 free_indices
    in
    let pp_access ppf (tensor, indices) =
      Fmt.pf ppf "%s[%a]" tensor Fmt.(list ~sep:comma pp_var) indices
    in
    (match use_frob with
    | None -> (
        (match summation_indices with
        | [] -> ()
        | _ ->
            Fmt.pf ppf "%s# Loop over all summation indices@."
              (mk_indent outer_indent));
        Fmt.pf ppf "%stotal = 0@." (mk_indent outer_indent);
        let inner_indent =
          List.fold_left
            (fun indent index ->
              Fmt.pf ppf "%sfor %a in range(%a):@." (mk_indent indent) pp_var
                index pp_d_var index;
              indent + 1)
            outer_indent summation_indices
        in

        (* summation inside loop *)
        (* Name tensors starting with A, then B, etc *)
        let accesses =
          List.mapi
            (fun i tensor -> (String.make 1 (Char.chr (i + 65)), tensor))
            lhs_tensors
        in
        Fmt.pf ppf "%s@[<hov 4>total@ +=@ @[%a@]@]@." (mk_indent inner_indent)
          Fmt.(list ~sep:(any " * ") pp_access)
          accesses;

        (* assign total to result *)
        match rhs_tensor with
        | [] -> ()
        | _ ->
            Fmt.pf ppf "%sresult[@[%a@]] = total@." (mk_indent outer_indent)
              Fmt.(list ~sep:comma pp_var)
              free_indices)
    | Some backend ->
        (match rhs_tensor with
        | [] -> Fmt.pf ppf "%s@[<hov 4>total@ =@ " (mk_indent outer_indent)
        | _ ->
            Fmt.pf ppf "%s@[<hov 4>result[@[%a@]]@ +=@ "
              (mk_indent outer_indent)
              Fmt.(list ~sep:comma pp_var)
              free_indices);
        let accesses =
          List.mapi
            (fun i tensor ->
              ( String.make 1 (Char.chr (i + 65)),
                List.map
                  (fun label ->
                    if SS.mem label summation_indices_set then ":" else label)
                  tensor ))
            lhs_tensors
        in
        Fmt.pf ppf "@[%s.sum(@[%a@])@]@]@."
          (match backend with Numpy -> "np" | Pytorch -> "torch")
          Fmt.(list ~sep:(any " * ") pp_access)
          accesses);

    (* return result *)
    match rhs_tensor with
    | [] -> Fmt.pf ppf "return total@."
    | _ -> Fmt.pf ppf "return result@."
end

module Explain : sig
  type contractions =
    | Unary_contraction of string list * Unary_contraction.t
    | Binary_contractions of Binary_contraction.t list

  val get_contractions : ?path:(int * int) list -> Rewrite.t -> contractions
  (** Get the contractions along the given path. *)

  val show_loops : Rewrite.t -> Pyloops.t
  (** Put in [Pyloops.t] format. *)
end = struct
  type contractions =
    | Unary_contraction of string list * Unary_contraction.t
    | Binary_contractions of Binary_contraction.t list

  let get_contractions ?path (bindings, result_group) =
    let n_tensors = List.length bindings in

    if n_tensors = 1 then
      let tensor = List.hd bindings in
      Unary_contraction
        ( tensor,
          Unary_contraction.make ~contracted:tensor ~result_type:result_group )
    else
      let path =
        match path with
        | Some [] | None -> List.init (n_tensors - 1) (fun _ -> (0, 1))
        | Some path -> path
      in
      let _, steps =
        path
        |> List.fold_left
             (fun (tensors, steps) (ixl, ixr) ->
               let l, r = List.(nth tensors ixl, nth tensors ixr) in
               let other_tensors =
                 List.fold_right Util.delete_from_list [ ixl; ixr ] tensors
               in
               let single_contraction =
                 Binary_contraction.make l r ~other_tensors ~target:result_group
               in
               let new_tensors =
                 single_contraction.result_type :: other_tensors
               in
               (new_tensors, List.append steps [ single_contraction ]))
             (bindings, [])
      in
      Binary_contractions steps

  let%expect_test "steps" =
    let go in_tensors out_tensor =
      match get_contractions (in_tensors, out_tensor) with
      | Unary_contraction _ -> Fmt.pr "fail"
      | Binary_contractions steps ->
          List.iter
            (fun Binary_contraction.{ l; r; result_type; _ } ->
              Fmt.(
                pr "@[l: [%a], r: [%a], result_type: [%a]@]@."
                  (list ~sep:semi string) l (list ~sep:semi string) r
                  (list ~sep:semi string) result_type))
            steps
    in
    go [ [ "a"; "b" ]; [ "b"; "c" ]; [ "c"; "d" ] ] [ "a"; "d" ];
    [%expect
      {|
      l: [a; b], r: [b; c], result_type: [a; c]
      l: [a; c], r: [c; d], result_type: [a; d]
    |}]

  let pp_explain_binary_contraction backend ppf
      (single_contraction : Binary_contraction.t) =
    let l_tensor, r_tensor = (single_contraction.l, single_contraction.r) in
    let l = Fmt.(list string ~sep:sp) in
    Fmt.(
      pf ppf "@[<2>contract@ @[%a@] (@[%a, %a@] -> @[%a@])@ (@[%a@])@]" l
        single_contraction.contracted l l_tensor l r_tensor l
        single_contraction.result_type
        (option (Binary_contraction.Ops.pp backend))
        single_contraction.operations)

  let pp_explain_unary_contraction backend ppf
      ( tensor,
        Unary_contraction.{ operations; contracted; preserved; result_type = _ }
      ) =
    let l = Fmt.(list string ~sep:sp) in
    Fmt.(
      pf ppf "@[<2>contract@ %a (@[%a@] -> @[%a@])@ (%a)@]" l contracted l
        tensor l preserved
        (list ~sep:sp (Unary_contraction.Op.pp backend))
        operations)

  let%expect_test "explain contractions" =
    let go rewrite path =
      match get_contractions ~path rewrite with
      | Binary_contractions cs ->
          Fmt.(pr "@[%a@]@." (list (pp_explain_binary_contraction Pytorch)) cs)
      | Unary_contraction (tensor, unary_contraction) ->
          Fmt.pr "@[%a@]@."
            (pp_explain_unary_contraction Pytorch)
            (tensor, unary_contraction)
    in
    let rewrite =
      ([ [ "a"; "i"; "j" ]; [ "a"; "j"; "k" ]; [ "a"; "i"; "k" ] ], [])
    in
    go rewrite [ (1, 2); (0, 1) ];
    [%expect
      {|
      contract k (a j k, a i k -> a i j) ()
      contract a i j (a i j, a i j -> ) 
        (torch.tensordot(x, y, [(0, 0), (2, 2), (1, 1)]))
      |}];
    go rewrite [ (0, 1); (0, 1) ];
    [%expect
      {|
      contract j (a i j, a j k -> a i k) ()
      contract a i k (a i k, a i k -> )
        (torch.tensordot(x, y, [(0, 0), (2, 2), (1, 1)]))
      |}];
    let rewrite = ([ [ "i"; "k" ]; [ "k"; "j" ] ], [ "i"; "j" ]) in
    go rewrite [ (0, 1) ];
    [%expect {| contract k (i k, k j -> i j) ((x @ y)) |}];
    let rewrite = ([ [ "i"; "i" ] ], []) in
    go rewrite [];
    [%expect {| contract i (i i -> ) (.trace()) |}]

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
      show_loops rewrite |> Pyloops.pp (fun _ -> "#000") Fmt.stdout
    in

    go ([ [ "i"; "k" ]; [ "k"; "j" ] ], [ "i"; "j" ]);
    [%expect
      {|
      result = np.zeros((d_i, d_j))
      # Loop over all free indices
      for i in range(d_i):
          for j in range(d_j):
              # Loop over all summation indices
              total = 0
              for k in range(d_k):
                  total += A[i, k] * B[k, j]
              result[i, j] = total
      return result
      |}];

    go ([ [ "s" ]; [ "s"; "t" ]; [ "t" ] ], []);
    [%expect
      {|
      # Loop over all summation indices
      total = 0
      for s in range(d_s):
          for t in range(d_t):
              total += A[s] * B[s, t] * C[t]
      return total
      |}];

    go ([ [ "i"; "i" ] ], [ "i" ]);
    [%expect
      {|
      result = np.zeros((d_i))
      # Loop over all free indices
      for i in range(d_i):
          total = 0
          total += A[i, i]
          result[i] = total
      return result
      |}];

    go ([ [ "i"; "i" ] ], []);
    [%expect
      {|
      # Loop over all summation indices
      total = 0
      for i in range(d_i):
          total += A[i, i]
      return total
      |}];

    go ([ [ "s" ]; [ "s"; "t" ]; [ "t" ] ], []);
    [%expect
      {|
      # Loop over all summation indices
      total = 0
      for s in range(d_s):
          for t in range(d_t):
              total += A[s] * B[s, t] * C[t]
      return total
      |}];

    go ([ [ "b"; "i" ]; [ "b"; "j" ] ], [ "b"; "i"; "j" ]);
    [%expect
      {|
      result = np.zeros((d_b, d_i, d_j))
      # Loop over all free indices
      for b in range(d_b):
          for i in range(d_i):
              for j in range(d_j):
                  total = 0
                  total += A[b, i] * B[b, j]
                  result[b, i, j] = total
      return result
      |}]
end

let rec intersperse mk_sep = function
  | [] -> []
  | [ x ] -> [ x ]
  | x :: xs -> x :: mk_sep () :: intersperse mk_sep xs

let%expect_test "intersperse" =
  let go strs =
    Fmt.(
      pr "%a"
        (brackets (list ~sep:comma string))
        (intersperse (fun () -> "x") strs))
  in
  go [];
  [%expect {| [] |}];
  go [ "a" ];
  [%expect {| [a] |}];
  go [ "a"; "b" ];
  [%expect {| [a, x, b] |}];
  go [ "a"; "b"; "c" ];
  [%expect {| [a, x, b, x, c] |}]
