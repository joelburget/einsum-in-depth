(* TODO: rename module to Tensor_shape? *)

module Elem_unification_result = struct
  type t =
    | Failure of string
    | Concrete_concrete
    | Asserted_equality of Asserted_equality.t
end

module Elem = struct
  type t = Concrete of int | Variable of string

  let to_string = function Concrete n -> Int.to_string n | Variable v -> v
  let is_concrete = function Concrete _ -> true | _ -> false
  let get_var = function Concrete _ -> None | Variable v -> Some v

  let unify x y =
    match (x, y) with
    | Concrete x, Concrete y ->
        if x = y then Elem_unification_result.Concrete_concrete
        else
          Failure
            ("Expected " ^ Int.to_string x ^ " and " ^ Int.to_string y
           ^ " to be equal")
    | Concrete c, Variable v | Variable v, Concrete c ->
        Asserted_equality (Variable_concrete (v, c))
    | Variable v1, Variable v2 -> Asserted_equality (Variable_variable (v1, v2))
end

type t = Elem.t list

let to_string elems =
  let inside = elems |> List.map Elem.to_string |> Util.join ~sep:", " in
  "[" ^ inside ^ "]"

let is_concrete elems = elems |> List.for_all Elem.is_concrete

module StringSet = Set.Make (String)

let filter_map : ('a -> 'b option) -> 'a list -> 'b list =
 fun f xs ->
  let rec go = function
    | [] -> []
    | x :: xs -> ( match f x with None -> go xs | Some y -> y :: go xs)
  in
  go xs

let bound_vars elems = elems |> filter_map Elem.get_var |> StringSet.of_list
