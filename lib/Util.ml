module String_set = Set.Make (String)

let replicate n item = List.init n (fun _ -> item)
let pad n x xs = replicate (n - List.length xs) x @ xs
let sum = List.fold_left ( + ) 0

let rec transpose = function
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss ->
      (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

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
