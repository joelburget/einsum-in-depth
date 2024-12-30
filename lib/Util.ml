module String_set = Set.Make (String)

let replicate n item = List.init n (fun _ -> item)
let pad n x xs = replicate (n - List.length xs) x @ xs
let sum = List.fold_left ( + ) 0

let find_repeats : string list -> string list =
 fun names ->
  let rec loop names seen repeats =
    match names with
    | [] -> repeats
    | name :: names ->
        if String_set.mem name seen then
          loop names seen (String_set.add name repeats)
        else loop names (String_set.add name seen) repeats
  in
  let repeats = loop names String_set.empty String_set.empty in
  String_set.elements repeats

let%expect_test "find_repeats" =
  find_repeats [ "a"; "b"; "c"; "a"; "d"; "e"; "b"; "f" ]
  |> List.iter print_endline;
  [%expect {|
    a
    b |}]

let rec delete_from_list i xs =
  match (xs, i) with
  | [], _ -> []
  | _ :: xs, 0 -> xs
  | x :: xs, i -> x :: delete_from_list (i - 1) xs
