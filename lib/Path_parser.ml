module Scanning = Scanf.Scanning

type path = (int * int) list

let parse_pair s =
  try Scanf.bscanf s "(%d, %d)" (fun x y -> Ok (x, y)) with
  | Scanf.Scan_failure s -> Error s
  | Failure _ -> Error "Invalid pair"
  | End_of_file -> Error "Unexpected end of file"

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
      match parse_pair stream with
      | Ok p -> loop () |> Result.map (fun path -> p :: path)
      | Error _ as e -> e
  in
  loop ()
  |> Result.map (fun path ->
         (path, if brackets_stripped then Tensor_type.Bracketed else Unbracketed))

let%expect_test "parse_pair" =
  let go s =
    match parse_pair (Scanning.from_string s) with
    | Ok (x, y) -> Fmt.pr "(%d, %d)@." x y
    | Error msg -> Fmt.pr "%s@." msg
  in
  go "(1, 2)";
  go "(1 2)";
  go "(1, a)";
  go "(1, 2.0)";
  go "(1,";
  [%expect
    {|
    (1, 2)
    scanf: bad input at char number 2: looking for ',', found ' '
    scanf: bad input at char number 4: character 'a' is not a decimal digit
    scanf: bad input at char number 5: looking for ')', found '.'
    Unexpected end of file |}]

let%expect_test "parse" =
  let go s =
    match parse s with
    | Ok (pairs, bracketed) ->
        let l_brack, r_brack =
          match bracketed with
          | Bracketed -> ("[", "]")
          | Unbracketed -> ("", "")
        in
        Fmt.(
          pr "@[%s%a%s@]@." l_brack
            (list ~sep:comma (parens (pair ~sep:comma int int)))
            pairs r_brack)
    | Error msg -> Fmt.pr "%s@." msg
  in
  go "[]";
  go "";
  go "(1, 2)";
  go "(1, 2), (3, 4)";
  go "[(1, 2), (3, 4)]";
  [%expect {|
    []

    (1, 2)
    (1, 2), (3, 4)
    [(1, 2), (3, 4)]
    |}]
