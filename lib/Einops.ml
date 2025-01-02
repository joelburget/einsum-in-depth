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

module Indexof_impl : sig
  val indexof : 'a -> 'a list -> int option
end = struct
  let indexof x lst =
    let rec go i = function
      | [] -> None
      | y :: ys -> if x = y then Some i else go (i + 1) ys
    in
    go 0 lst
end

include Indexof_impl

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
  val show_loops : Rewrite.t -> Pyloops.t
  (** Put in [Pyloops.t] format. *)
end = struct
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
      show_loops rewrite |> Pyloops.pp (fun _ -> "") Fmt.stdout
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

module Steps : sig
  type t = { diagonalized : string list list; broadcast : string list }

  val make : Rewrite.t -> t
end = struct
  type t = { diagonalized : string list list; broadcast : string list }

  let extract_diagonals tensor =
    let rec go seen = function
      | [] -> seen
      | x :: xs -> if List.mem x seen then go seen xs else go (x :: seen) xs
    in
    go [] tensor |> List.rev

  let%expect_test "extract_diagonals" =
    let go tensor =
      extract_diagonals tensor |> Fmt.(pr "[@[%a@]]@." (list ~sep:semi string))
    in
    go [ "a"; "b"; "c" ];
    [%expect {| [a; b; c] |}];
    go [ "a"; "b"; "a" ];
    [%expect {| [a; b] |}];
    go [ "a"; "b"; "c"; "a" ];
    [%expect {| [a; b; c] |}]

  let make_common_shape tensors rhs =
    let index_set = String_set.of_list (List.flatten tensors) in
    let compare x y =
      match (indexof x rhs, indexof y rhs) with
      | None, None -> String.compare x y
      | Some a, Some b -> compare a b
      | Some _, None -> -1
      | None, Some _ -> 1
    in
    List.sort compare (String_set.elements index_set)

  let%expect_test "make_common_shape" =
    let go tensors rhs =
      make_common_shape tensors rhs
      |> Fmt.(pr "[@[%a@]]@." (list ~sep:semi string))
    in
    go [ [ "a"; "b"; "c" ]; [ "a"; "b"; "a" ] ] [ "a"; "b"; "c" ];
    [%expect {| [a; b; c] |}];
    go [ [ "a"; "b"; "c" ]; [ "a"; "d"; "b" ] ] [ "a"; "b"; "c" ];
    [%expect {| [a; b; c; d] |}];
    go [ [ "a"; "b"; "c" ]; [ "a"; "b"; "a" ] ] [ "a"; "b" ];
    [%expect {| [a; b; c] |}];
    go [ [ "a"; "b" ] ] [ "a" ];
    [%expect {| [a; b] |}]

  let make (lhs, rhs) =
    let diagonalized = List.map extract_diagonals lhs in
    let broadcast = make_common_shape diagonalized rhs in
    { diagonalized; broadcast }

  let%expect_test "make" =
    let go lhs rhs =
      let { diagonalized; broadcast } = make (lhs, rhs) in
      Fmt.pr "@[diagonalized: [@[%a@]],@ broadcast: [@[%a@]]@]@."
        Fmt.(list ~sep:semi (brackets (list ~sep:semi string)))
        diagonalized
        Fmt.(list ~sep:semi string)
        broadcast
    in
    go [ [ "a"; "b" ] ] [ "a" ];
    [%expect {| diagonalized: [[a; b]], broadcast: [a; b] |}];
    go [ [ "a"; "b" ] ] [ "b" ];
    [%expect {| diagonalized: [[a; b]], broadcast: [b; a] |}]
end
