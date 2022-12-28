module Unify_mode = Tensor_type.Unify_mode

module Fusion_step = struct
  type t =
    | Promote_left
    | Promote_right
    | Broadcast
    | Remove_dim
    | Matmul of Tensor_type.Elem.t * Tensor_type.Elem.t * Tensor_type.Elem.t
    | Unify
    (* Note: dot product has different behavior in numpy and pytorch:

       x = torch.empty((9,5,7,4))
       y = torch.empty((9,5,4,3))
       torch.dot(x, y).shape

       x = np.empty((9,5,7,4))
       y = np.empty((9,5,4,3))
       np.dot(x, y).shape

       (I don't think I agree with the numpy behavior)
    *)
    | Dot_product

  let to_string = function
    | Promote_left -> "Promote_left"
    | Promote_right -> "Promote_right"
    | Broadcast -> "Broadcast"
    | Remove_dim -> "Remove_dim"
    | Matmul (t1, t2, t3) ->
        let to_string = Tensor_type.Elem.to_string in
        let t1 = to_string t1 in
        let t2 = to_string t2 in
        let t3 = to_string t3 in
        "Matmul (" ^ t1 ^ ", " ^ t2 ^ ", " ^ t3 ^ ")"
        (* of Tensor_type.Elem.t * Tensor_type.Elem.t * Tensor_type.Elem.t *)
    | Unify -> "Unify"
    | Dot_product -> "Dot_product"
end

module Fusion_result = struct
  type t =
    | Failure of string
    | Success of {
        steps : Fusion_step.t list;
        equalities : Asserted_equality.t list;
        result_shape : Tensor_type.t;
      }

  let to_string = function
    | Failure str -> str
    | Success { steps; equalities; result_shape } ->
        let steps =
          steps |> List.map Fusion_step.to_string |> Util.join ~sep:"; "
        in
        let equalities =
          equalities
          |> List.map Asserted_equality.to_string
          |> Util.join ~sep:"; "
        in
        let result_shape = Tensor_type.to_string result_shape in
        "Success { steps = " ^ steps ^ "; equalities = [" ^ equalities
        ^ "]; result_shape = " ^ result_shape ^ " }"
end

module type Op = sig
  type t

  val fuse : Unify_mode.t -> t (* -> Tensor_type.t *) -> Fusion_result.t
end

module Matmul : Op = struct
  type t = { l : Tensor_type.t; r : Tensor_type.t }

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

  (* TODO: is this possible in general with variable sizes? yes -- just return constraints. *)
  let rec broadcast_batches unify_mode reverse_ls reverse_rs =
    match (reverse_ls, reverse_rs) with
    | [], [] -> Ok (Fusion_step.Broadcast (* TODO *), [], [])
    | [], xs | xs, [] -> Ok (Fusion_step.Broadcast, xs, [])
    | x :: xs, y :: ys -> (
        match Tensor_type.Elem.unify unify_mode x y with
        | Failure msg -> Error msg
        | Matching_concrete n | Broadcasting n ->
            broadcast_batches unify_mode xs ys
            |> Result.map (fun (_step, batch_unified, constraints) ->
                   ( Fusion_step.Broadcast,
                     Tensor_type.Elem.Concrete n :: batch_unified,
                     constraints ))
        | Matching_variables _ ->
            broadcast_batches unify_mode xs ys
            |> Result.map (fun (_step, batch_unified, constraints) ->
                   (Fusion_step.Broadcast, x :: batch_unified, constraints))
        | Asserted_equality equality ->
            broadcast_batches unify_mode xs ys
            |> Result.map (fun (_step, batch_unified, constraints) ->
                   ( Fusion_step.Broadcast,
                     batch_unified,
                     equality :: constraints )))

  let%expect_test "broadcast_batches" =
    let open Tensor_type.Elem in
    let go ?(mode = Unify_mode.Unify_concrete_variables) ls rs =
      let reverse_ls = List.rev ls in
      let reverse_rs = List.rev rs in
      let str_ls = reverse_ls |> List.map to_string |> Util.join ~sep:"; " in
      let str_rs = reverse_rs |> List.map to_string |> Util.join ~sep:"; " in
      print_endline ("[" ^ str_ls ^ "], [" ^ str_rs ^ "] -> ");
      match broadcast_batches mode reverse_ls reverse_rs with
      | Error msg -> print_endline msg
      | Ok (_step, batch_unified, batch_assertions) ->
          let items_str =
            batch_unified |> List.map to_string |> Util.join ~sep:"; "
          in
          print_endline ("[" ^ items_str ^ "]");
          let items_str =
            batch_assertions
            |> List.map Asserted_equality.to_string
            |> Util.join ~sep:"; "
          in
          print_endline ("[" ^ items_str ^ "]")
    in

    go [] [];
    print_endline "concrete only:";
    go [ Concrete 1 ] [ Concrete 2 ];
    go [ Concrete 3; Concrete 2 ] [ Concrete 2 ];
    go [ Concrete 256; Concrete 256; Concrete 3 ] [ Concrete 3 ];
    go
      [ Concrete 8; Concrete 1; Concrete 6; Concrete 1 ]
      [ Concrete 7; Concrete 1; Concrete 5 ];
    print_endline "variable:";
    let go = go ~mode:Dont_unify_concrete_variables in
    go [ Concrete 1 ] [ Variable "x" ];
    go [ Concrete 2 ] [ Variable "x" ];
    go [ Variable "x" ] [ Variable "x" ];
    go [ Variable "x" ] [ Variable "y" ];
    [%expect
      {|
      [], [] ->
      []
      []
      concrete only:
      [1], [2] ->
      [2]
      []
      [2; 3], [2] ->
      [2; 3]
      []
      [3; 256; 256], [3] ->
      [3; 256; 256]
      []
      [1; 6; 1; 8], [5; 1; 7] ->
      [5; 6; 7; 8]
      []
      variable:
      [1], [x] ->
      Can't unify x and 1 in Dont_unify_concrete_variables mode
      [2], [x] ->
      Can't unify x and 2 in Dont_unify_concrete_variables mode
      [x], [x] ->
      [x]
      []
      [x], [y] ->
      []
      [(Variable_variable (x, y))] |}]

  let mk_result steps result_shape = function
    | Tensor_type.Elem_unification_result.Failure msg ->
        Fusion_result.Failure msg
    | Matching_concrete _ | Matching_variables _ | Broadcasting _ ->
        Success { steps; equalities = []; result_shape }
    | Asserted_equality equality ->
        Success { steps; equalities = [ equality ]; result_shape }

  let one_to_many promotion_side (n, m, o) =
    let steps =
      Fusion_step.[ promotion_side; Broadcast; Matmul (n, m, o); Remove_dim ]
    in
    let result_shape = [ n; o ] in
    function
    | Tensor_type.Elem_unification_result.Failure msg ->
        Fusion_result.Failure msg
    | Matching_concrete _ | Matching_variables _ | Broadcasting _ ->
        Success { steps; equalities = []; result_shape }
    | Asserted_equality equality ->
        Success { steps; equalities = [ equality ]; result_shape }

  (* First promote, then broadcast, then matmul, then remove dim. *)
  let fuse unify_mode { l; r } =
    let unify = Tensor_type.Elem.unify unify_mode in
    match (l, r) with
    | [], _ | _, [] ->
        Fusion_result.Failure "both arguments to matmul need to be at least 1D"
    | [ n ], [ m ] -> mk_result [ Fusion_step.Dot_product ] [] (unify n m)
    | [ n ], [ n'; m ] ->
        mk_result
          Fusion_step.[ Promote_left; Matmul (Concrete 1, n, m); Remove_dim ]
          [ m ] (unify n n')
    | [ n; m ], [ m' ] ->
        mk_result
          Fusion_step.[ Promote_right; Matmul (n, m, Concrete 1); Remove_dim ]
          [ n ] (unify m m')
    | [ n; m ], [ m'; o ] ->
        mk_result [ Matmul (n, m, o) ] [ n; o ] (unify m m')
    | [ n ], batched ->
        let n', m = last_two batched in
        one_to_many Promote_left (Concrete 1, n, m) (unify n n')
    | batched, [ m ] ->
        let n, m' = last_two batched in
        one_to_many Promote_right (n, m, Concrete 1) (unify m m')
    | _ -> (
        let reverse_batch_left, n, m = last_two' l in
        let reverse_batch_right, m', o = last_two' r in
        match
          broadcast_batches unify_mode reverse_batch_left reverse_batch_right
        with
        | Error _msg -> failwith "TODO"
        | Ok (broadcast, batch_unified, batch_assertions) -> (
            let result_shape = batch_unified @ [ n; o ] in
            match unify m m' with
            | Tensor_type.Elem_unification_result.Failure msg ->
                Fusion_result.Failure msg
            | Matching_concrete _ | Matching_variables _ | Broadcasting _ ->
                Success
                  {
                    steps = [ broadcast; Matmul (n, m, o) ];
                    equalities = batch_assertions;
                    result_shape;
                  }
            | Asserted_equality assertion ->
                Success
                  {
                    steps = [ broadcast; Matmul (n, m, o) ];
                    equalities = assertion :: batch_assertions;
                    result_shape;
                  }))

  let%expect_test "fuse" =
    let open Tensor_type.Elem in
    let testcase ?(mode = Unify_mode.Unify_concrete_variables) l r =
      print_endline ("  " ^ Fusion_result.to_string (fuse mode { l; r }))
    in
    print_endline "no element:";
    testcase [] [];
    testcase [ Concrete 2 ] [];
    testcase [] [ Concrete 2 ];
    print_endline "single_element:";
    testcase [ Concrete 2 ] [ Concrete 2 ];
    testcase [ Concrete 2 ] [ Concrete 1 ];
    testcase [ Concrete 2 ] [ Variable "x" ];
    print_endline "promote single element:";
    testcase [ Concrete 2 ] [ Variable "x"; Variable "y" ];
    print_endline "regular matmul:";
    testcase [ Variable "x"; Variable "y" ] [ Variable "y"; Variable "z" ];
    print_endline "batched:";
    testcase
      [ Variable "j"; Concrete 1; Variable "n"; Variable "n" ]
      [ Variable "k"; Variable "n"; Variable "n" ];
    [%expect
      {|
      no element:
        both arguments to matmul need to be at least 1D
        both arguments to matmul need to be at least 1D
        both arguments to matmul need to be at least 1D
      single_element:
        Success { steps = Dot_product; equalities = []; result_shape = [] }
        Success { steps = Dot_product; equalities = []; result_shape = [] }
        Success { steps = Dot_product; equalities = [(Variable_concrete (x, 2))]; result_shape = [] }
      promote single element:
        Success { steps = Promote_left; Matmul (1, 2, y); Remove_dim; equalities = [(Variable_concrete (x, 2))]; result_shape = [y] }
      regular matmul:
        Success { steps = Matmul (x, y, z); equalities = []; result_shape = [x; z] }
      batched:
        Success { steps = Broadcast; Matmul (n, n, n); equalities = [(Variable_concrete (k, 1))]; result_shape = [j; k; n; n] }|}]
end

module Stack = struct
  type t = { arrays : Tensor_type.t; axis : int option }

  let fuse _ = [ (* TODO *) ]
end

module Reshape = struct
  type t = { tensor : Tensor_type.t; newshape : Tensor_type.t }

  let fuse _ = [ (* TODO *) ]
end

module Transpose = struct
  type t = Tensor_type.t (* TODO: axes? *)

  let fuse _ = [ (* TODO *) ]
end

module Squeeze = struct
  type t = { tensor : Tensor_type.t; axis : int list }

  let fuse _ = [ (* TODO *) ]
end

module Expand_dims = struct
  type t = { tensor : Tensor_type.t; axis : int list }

  let fuse _ = [ (* TODO *) ]
end

module Repeat = struct
  type t = { tensor : Tensor_type.t; repeats : int list; axis : int option }

  let fuse _ = [ (* TODO *) ]
end

module Tile = struct
  type t = { tensor : Tensor_type.t; reps : int list }

  let fuse _ = [ (* TODO *) ]
end

module Concatenate = struct
  type t = { tensors : Tensor_type.t list; axis : int option }

  let fuse _ = [ (* TODO *) ]
end
