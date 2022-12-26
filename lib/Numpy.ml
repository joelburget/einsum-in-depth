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
end

module Fusion_result = struct
  type t =
    | Failure of string
    | Success of {
        steps : Fusion_step.t list;
        equalities : Asserted_equality.t list;
        result_shape : Tensor_type.t;
      }
end

module type Op = sig
  type t

  val fuse : t (* -> Tensor_type.t *) -> Fusion_result.t
end

module Matmul : Op = struct
  type t = { l : Tensor_type.t; r : Tensor_type.t }

  let rec last_two = function
    | [] | [ _ ] ->
        failwith "Invariant violation -- last two called with short list"
    | [ x; y ] -> (x, y)
    | _ :: list -> last_two list

  let last_two' _ = (failwith "TODO", failwith "TODO", failwith "TODO")

  (* TODO: could fail *)
  let broadcast_batches _ _ =
    (Fusion_step.Broadcast (* TODO *), failwith "TODO", failwith "TODO")

  let mk_result steps result_shape = function
    | Tensor_type.Elem_unification_result.Failure msg ->
        Fusion_result.Failure msg
    | Concrete_concrete -> Success { steps; equalities = []; result_shape }
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
    | Concrete_concrete -> Success { steps; equalities = []; result_shape }
    | Asserted_equality equality ->
        Success { steps; equalities = [ equality ]; result_shape }

  (* First promote, then broadcast, then matmul, then remove dim. *)
  let fuse { l; r } =
    let unify = Tensor_type.Elem.unify in
    match (l, r) with
    | [], _ | _, [] ->
        Fusion_result.Failure "both arguments to matmul need to be at least 1D"
    | [ n ], [ m ] -> mk_result [ Fusion_step.Dot_product ] [] (unify n m)
    | [ n ], [ n'; m ] ->
        mk_result
          Fusion_step.[ Promote_left; Matmul (Concrete 1, n, m) ]
          [ Concrete 1; m ] (unify n n')
    | [ n; m ], [ m' ] ->
        mk_result
          Fusion_step.[ Promote_right; Matmul (n, m, Concrete 1) ]
          [ n; Concrete 1 ] (unify m m')
    | [ n; m ], [ m'; o ] ->
        mk_result [ Matmul (n, m, o) ] [ n; o ] (unify m m')
    | [ n ], batched ->
        let n', m = last_two batched in
        one_to_many Promote_left (Concrete 1, n, m) (unify n n')
    | batched, [ m ] ->
        let n, m' = last_two batched in
        one_to_many Promote_right (n, m, Concrete 1) (unify m m')
    | _ -> (
        let batch_left, n, m = last_two' l in
        let batch_right, m', o = last_two' r in
        let broadcast, batch_unified, batch_assertions =
          broadcast_batches batch_left batch_right
        in
        let result_shape = batch_unified @ [ n; o ] in
        match unify m m' with
        | Tensor_type.Elem_unification_result.Failure msg ->
            Fusion_result.Failure msg
        | Concrete_concrete ->
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
              })
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
