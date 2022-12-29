(* TODO: rename module to Tensor_shape? *)

type bracketed = Bracketed | Unbracketed

module Unify_mode = struct
  type t = Unify_concrete_variables | Dont_unify_concrete_variables
end

module Elem_unification_result = struct
  type t =
    | Failure of string
    | Matching_variables of string
    | Matching_concrete of int
    | Broadcasting of int
    | Asserted_equality of Asserted_equality.t

  let to_string = function
    | Failure msg -> "(Failure " ^ msg ^ ")"
    | Matching_variables x -> "(Matching_variables " ^ x ^ ")"
    | Matching_concrete n -> "(Matching_concrete " ^ Int.to_string n ^ ")"
    | Broadcasting n -> "(Broadcasting " ^ Int.to_string n ^ ")"
    | Asserted_equality x ->
        "(Asserted_equality " ^ Asserted_equality.to_string x ^ ")"
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

  let unify unify_mode x y =
    match (x, y) with
    | Concrete n, Concrete m ->
        if n = m then Elem_unification_result.Matching_concrete n
        else if n = 1 then Broadcasting m
        else if m = 1 then Broadcasting n
        else
          Failure
            ("Expected " ^ Int.to_string n ^ " and " ^ Int.to_string m
           ^ " to be equal")
    | Concrete c, Variable v | Variable v, Concrete c -> (
        (* Asserted_equality (Variable_concrete (v, c)) *)
        match unify_mode with
        | Unify_mode.Unify_concrete_variables ->
            Asserted_equality (Variable_concrete (v, c))
        (* XXX: does Dont_unify_concrete_variables apply to 1?? *)
        | Dont_unify_concrete_variables ->
            Failure
              ("Can't unify " ^ v ^ " and " ^ Int.to_string c
             ^ " in Dont_unify_concrete_variables mode"))
    | Variable v1, Variable v2 ->
        if v1 = v2 then Matching_variables v1
        else Asserted_equality (Variable_variable (v1, v2))

  let%expect_test "unify" =
    let testcase ~mode x y =
      print_endline (Elem_unification_result.to_string (unify mode x y))
    in

    print_endline "Unify_concrete_variables:";
    let go = testcase ~mode:Unify_concrete_variables in
    go (Concrete 2) (Concrete 2);
    go (Concrete 2) (Concrete 1);
    go (Concrete 1) (Concrete 2);
    go (Concrete 2) (Concrete 2);
    go (Concrete 2) (Variable "x");
    go (Variable "x") (Concrete 2);
    go (Variable "x") (Variable "x");
    go (Variable "x") (Variable "y");

    print_endline "Dont_unify_concrete_variables:";
    let go = testcase ~mode:Dont_unify_concrete_variables in
    go (Concrete 2) (Concrete 1);
    go (Concrete 2) (Concrete 3);
    go (Concrete 2) (Variable "x");
    go (Variable "x") (Concrete 2);
    go (Variable "x") (Variable "y");

    [%expect
      {|
      Unify_concrete_variables:
      (Matching_concrete 2)
      (Broadcasting 2)
      (Broadcasting 2)
      (Matching_concrete 2)
      (Asserted_equality (Variable_concrete (x, 2)))
      (Asserted_equality (Variable_concrete (x, 2)))
      (Matching_variables x)
      (Asserted_equality (Variable_variable (x, y)))
      Dont_unify_concrete_variables:
      (Broadcasting 2)
      (Failure Expected 2 and 3 to be equal)
      (Failure Can't unify x and 2 in Dont_unify_concrete_variables mode)
      (Failure Can't unify x and 2 in Dont_unify_concrete_variables mode)
      (Asserted_equality (Variable_variable (x, y))) |}]
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
