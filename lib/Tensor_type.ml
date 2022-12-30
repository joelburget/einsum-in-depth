(* TODO: rename module to Tensor_shape? *)

type bracketed = Bracketed | Unbracketed

module Unify_mode = struct
  type t = Unify_concrete_variables | Dont_unify_concrete_variables
end

module Elem = struct
  type t = Concrete of int | Variable of string

  let pp ppf = function
    | Concrete n -> Fmt.pf ppf "%d" n
    | Variable x -> Fmt.pf ppf "%s" x

  let one = Concrete 1
  let to_string = function Concrete n -> Int.to_string n | Variable v -> v
  let is_concrete = function Concrete _ -> true | _ -> false
  let get_var = function Concrete _ -> None | Variable v -> Some v
end

type t = Elem.t list

let pp = Fmt.brackets (Fmt.list ~sep:Fmt.comma Elem.pp)

let to_string elems =
  let inside = elems |> List.map Elem.to_string |> Util.join ~sep:"; " in
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
