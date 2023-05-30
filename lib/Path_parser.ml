module Scanning = Scanf.Scanning

type path = int list list

let parse_int str =
  try str |> String.trim |> int_of_string
  with Failure _ ->
    raise (Failure (Fmt.str "%S could not be parsed as an int" str))

let is_space str = String.trim str = ""

let parse_int_list s =
  try
    let int_list = Scanf.bscanf s "(%[^)])" (fun x -> x) in
    let int_list = String.split_on_char ',' int_list in
    match int_list with
    | [ x1; x2 ] when is_space x2 -> Ok [ parse_int x1 ]
    | _ -> Ok (List.map parse_int int_list)
  with
  | Scanf.Scan_failure s -> Error s
  | Failure s -> Error ("Invalid list: " ^ s)
  | End_of_file -> Error "Unexpected end of file"

(* TODO: "," then trim? *)
let parse_sep s =
  try Scanf.bscanf s ", " () with Scanf.Scan_failure _ | End_of_file -> ()

let strip_brackets s =
  if String.length s > 0 && s.[0] = '[' && s.[String.length s - 1] = ']' then
    (true, String.sub s 1 (String.length s - 2))
  else (false, s)

let parse : string -> (path * Tensor_type.bracketed, string) result =
 fun s ->
  let brackets_stripped, s = strip_brackets s in
  let stream = Scanning.from_string s in
  let rec loop () =
    parse_sep stream;
    if Scanning.end_of_input stream then Ok []
    else
      match parse_int_list stream with
      | Ok p -> loop () |> Result.map (fun path -> p :: path)
      | Error _ as e -> e
  in
  loop ()
  |> Result.map (fun path ->
         (path, if brackets_stripped then Tensor_type.Bracketed else Unbracketed))

let pp_int_list ppf xs =
  match xs with
  | [ x ] -> Fmt.pf ppf "%d," x
  | _ -> Fmt.(list ~sep:comma int) ppf xs

let%expect_test "parse_int_list" =
  let go s =
    match parse_int_list (Scanning.from_string s) with
    | Ok xs -> Fmt.(pr "@[(%a)@]@." pp_int_list xs)
    | Error msg -> Fmt.pr "%s@." msg
  in
  go "(1, 2)";
  go "(1 2)";
  go "(1, a)";
  go "(1, 2.0)";
  go "(1,";
  go "(1,)";
  [%expect
    {|
    (1, 2)
    Invalid list: "1 2" could not be parsed as an int
    Invalid list: " a" could not be parsed as an int
    Invalid list: " 2.0" could not be parsed as an int
    Unexpected end of file
    (1,)
    |}]

let%expect_test "parse" =
  let go s =
    match parse s with
    | Ok (lists, bracketed) ->
        let l_brack, r_brack =
          match bracketed with
          | Bracketed -> ("[", "]")
          | Unbracketed -> ("", "")
        in
        Fmt.(
          pr "@[%s%a%s@]@." l_brack
            (list ~sep:comma (parens (list ~sep:comma int)))
            lists r_brack)
    | Error msg -> Fmt.pr "%s@." msg
  in
  go "[]";
  go "";
  go "(1, 2)";
  go "(1, 2), (3, 4)";
  go "[(1, 2), (3, 4)]";
  go "[(1, 2, 3), (4, 5)]";
  [%expect
    {|
    []

    (1, 2)
    (1, 2), (3, 4)
    [(1, 2), (3, 4)]
    [(1, 2, 3), (4, 5)]
    |}]
