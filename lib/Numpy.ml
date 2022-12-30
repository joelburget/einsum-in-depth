module Fusion_step = struct
  type t =
    | Promote_left
    | Promote_right
    | Broadcast
    | Remove_dim
    | Matmul of int * int * int
    (* Note: dot product has different behavior in numpy and pytorch:

       x = torch.empty((9,5,7,4))
       y = torch.empty((9,5,4,3))
       torch.dot(x, y).shape -> RuntimeError: 1D tensors expected, but got 4D and 4D tensors

       x = np.empty((9,5,7,4))
       y = np.empty((9,5,4,3))
       np.dot(x, y).shape -> (9, 5, 7, 9, 5, 3)

       (I don't think I agree with the numpy behavior)
    *)
    | Dot_product

  let pp ppf = function
    | Promote_left -> Fmt.pf ppf "Promote_left"
    | Promote_right -> Fmt.pf ppf "Promote_right"
    | Broadcast -> Fmt.pf ppf "Broadcast"
    | Remove_dim -> Fmt.pf ppf "Remove_dim"
    | Matmul (t1, t2, t3) ->
        Fmt.(pf ppf "Matmul %a" (parens (list ~sep:comma int)) [ t1; t2; t3 ])
    | Dot_product -> Fmt.pf ppf "Dot_product"

  let to_string = Fmt.to_to_string pp
end

module Fusion_result = struct
  type t =
    | Failure of string
    | Success of { steps : Fusion_step.t list; result_shape : int list }

  let pp ppf =
    let open Fmt in
    function
    | Failure str -> string ppf str
    | Success { steps; result_shape } ->
        pf ppf "Success @[<1>{steps = %a;@ result_shape = %a}@]"
          (brackets (list ~sep:semi Fusion_step.pp))
          steps
          (brackets (list ~sep:semi int))
          result_shape
end

module type Op = sig
  type t

  val fuse : t (* -> Tensor_type.t *) -> Fusion_result.t
end

module Elem_unification_result = struct
  type t = Failure of string | Matching of int | Broadcasting of int

  let pp ppf = function
    | Failure msg -> Fmt.pf ppf "(Failure %s)" msg
    | Matching n -> Fmt.pf ppf "(Matching %d)" n
    | Broadcasting n -> Fmt.pf ppf "(Broadcasting %d)" n
end

let unify n m =
  if n = m then Elem_unification_result.Matching n
  else if n = 1 then Broadcasting m
  else if m = 1 then Broadcasting n
  else Failure Fmt.(str "Cannot unify %d and %d" n m)

module Matmul : Op = struct
  type t = { l : int list; r : int list }

  let rec last_two = function
    | [] | [ _ ] ->
        failwith "Invariant violation -- last_two called with short list"
    | [ x; y ] -> (x, y)
    | _ :: list -> last_two list

  let last_two' = function
    | [] | [ _ ] ->
        failwith "Invariant violation -- last_two' called with short list"
    | x :: y :: zs ->
        let rec go prev x y zs =
          match zs with [] -> (prev, x, y) | z :: zs -> go (x :: prev) y z zs
        in
        go [] x y zs

  let%expect_test "last_two'" =
    let go xs =
      let xs, x1, x2 = last_two' xs in
      let xs = xs |> List.map Int.to_string |> Util.join ~sep:"; " in
      print_endline
        ("([" ^ xs ^ "], " ^ Int.to_string x1 ^ ", " ^ Int.to_string x2 ^ ")")
    in
    go [ 1; 2; 3; 4; 5 ];
    [%expect {| ([3; 2; 1], 4, 5) |}]

  let rec broadcast_batches reverse_ls reverse_rs =
    match (reverse_ls, reverse_rs) with
    | [], [] -> Ok (Fusion_step.Broadcast (* TODO *), [])
    | [], xs | xs, [] -> Ok (Fusion_step.Broadcast, xs)
    | x :: xs, y :: ys -> (
        match unify x y with
        | Failure msg -> Error msg
        | Matching n | Broadcasting n ->
            broadcast_batches xs ys
            |> Result.map (fun (_step, batch_unified) ->
                   (Fusion_step.Broadcast, n :: batch_unified)))

  let%expect_test "broadcast_batches" =
    let go ls rs =
      Fmt.pr "@[[%a], [%a] ->@]@."
        Fmt.(list ~sep:semi int)
        ls
        Fmt.(list ~sep:semi int)
        rs;
      match broadcast_batches (List.rev ls) (List.rev rs) with
      | Error msg -> print_endline msg
      | Ok (_step, batch_unified) ->
          Fmt.pr "@[[%a]@]@." Fmt.(list ~sep:semi int) (List.rev batch_unified)
    in

    go [] [];
    go [ 1 ] [ 2 ];
    go [ 3; 2 ] [ 2 ];
    go [ 256; 256; 3 ] [ 3 ];
    go [ 8; 1; 6; 1 ] [ 7; 1; 5 ];
    [%expect
      {|
      [], [] ->
      []
      [1], [2] ->
      [2]
      [3; 2], [2] ->
      [3; 2]
      [256; 256; 3], [3] ->
      [256; 256; 3]
      [8; 1; 6; 1], [7; 1; 5] ->
      [8; 7; 6; 5] |}]

  let mk_result steps result_shape = function
    | Elem_unification_result.Failure msg -> Fusion_result.Failure msg
    | Matching _ | Broadcasting _ -> Success { steps; result_shape }

  let one_to_many promotion_side (n, m, o) =
    let steps =
      Fusion_step.[ promotion_side; Broadcast; Matmul (n, m, o); Remove_dim ]
    in
    let result_shape = [ n; o ] in
    function
    | Elem_unification_result.Failure msg -> Fusion_result.Failure msg
    | Matching _ | Broadcasting _ -> Success { steps; result_shape }

  (* First promote, then broadcast, then matmul, then remove dim. *)
  let fuse { l; r } =
    match (l, r) with
    | [], _ | _, [] ->
        Fusion_result.Failure "both arguments to matmul need to be at least 1D"
    | [ n ], [ m ] -> mk_result [ Fusion_step.Dot_product ] [] (unify n m)
    | [ n ], [ n'; m ] ->
        mk_result
          Fusion_step.[ Promote_left; Matmul (1, n, m); Remove_dim ]
          [ m ] (unify n n')
    | [ n; m ], [ m' ] ->
        mk_result
          Fusion_step.[ Promote_right; Matmul (n, m, 1); Remove_dim ]
          [ n ] (unify m m')
    | [ n; m ], [ m'; o ] ->
        mk_result [ Matmul (n, m, o) ] [ n; o ] (unify m m')
    | [ n ], batched ->
        let n', m = last_two batched in
        one_to_many Promote_left (1, n, m) (unify n n')
    | batched, [ m ] ->
        let n, m' = last_two batched in
        one_to_many Promote_right (n, m, 1) (unify m m')
    | _ -> (
        let reverse_batch_left, n, m = last_two' l in
        let reverse_batch_right, m', o = last_two' r in
        match broadcast_batches reverse_batch_left reverse_batch_right with
        | Error _msg -> failwith "TODO"
        | Ok (broadcast, batch_unified) -> (
            let result_shape = batch_unified @ [ n; o ] in
            match unify m m' with
            | Elem_unification_result.Failure msg -> Fusion_result.Failure msg
            | Matching _ | Broadcasting _ ->
                Success
                  { steps = [ broadcast; Matmul (n, m, o) ]; result_shape }))

  let%expect_test "fuse" =
    let testcase l r = Fmt.pr "  %a@." Fusion_result.pp (fuse { l; r }) in
    print_endline "no element:";
    testcase [] [];
    testcase [ 2 ] [];
    testcase [] [ 2 ];
    print_endline "single_element:";
    testcase [ 2 ] [ 2 ];
    testcase [ 2 ] [ 1 ];
    print_endline "regular matmul:";
    testcase [ 2; 3 ] [ 3; 4 ];
    print_endline "batched:";
    testcase [ 5; 1; 2; 2 ] [ 4; 2; 2 ];
    [%expect
      {|
      no element:
        both arguments to matmul need to be at least 1D
        both arguments to matmul need to be at least 1D
        both arguments to matmul need to be at least 1D
      single_element:
        Success {steps = [Dot_product]; result_shape = []}
        Success {steps = [Dot_product]; result_shape = []}
      regular matmul:
        Success {steps = [Matmul (2, 3, 4)]; result_shape = [2; 4]}
      batched:
        Success {steps = [Broadcast; Matmul (2, 2, 2)];
                 result_shape = [4; 5; 2; 2]} |}]
end

module Stack = struct
  type t = { arrays : int list; axis : int option }

  let fuse _ = [ (* TODO *) ]
end

module Reshape = struct
  type t = { tensor : int list; newshape : int list }

  let fuse _ = [ (* TODO *) ]
end

module Transpose = struct
  type t = int list (* TODO: axes? *)

  let fuse _ = [ (* TODO *) ]
end

module Squeeze = struct
  type t = { tensor : int list; axis : int list }

  let fuse _ = [ (* TODO *) ]
end

module Expand_dims = struct
  type t = { tensor : int list; axis : int list }

  let fuse _ = [ (* TODO *) ]
end

module Repeat = struct
  type t = { tensor : int list; repeats : int list; axis : int option }

  let fuse _ = [ (* TODO *) ]
end

module Tile = struct
  type t = { tensor : int list; reps : int list }

  let fuse _ = [ (* TODO *) ]
end

module Concatenate = struct
  type t = { tensors : int list list; axis : int option }

  let fuse _ = [ (* TODO *) ]
end
