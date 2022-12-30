let parse parser str =
  match Type_parser_runner.parse parser str with
  | Ok v -> Fmt.(pr "%a@." (brackets (list ~sep:comma int))) v
  | Error (location, indication, message) ->
      Fmt.pr "%s%s%s@." location indication message

let%expect_test _ =
  let go = parse Type_parser.Incremental.bracketed in
  go "[]";
  go "[1]";
  go "[123, 456]";
  let go = parse Type_parser.Incremental.unbracketed in
  go "";
  go "1";
  go "123, 456";
  go "123,";
  [%expect {|
    []
    [1]
    [123, 456]
    []
    [1]
    [123, 456]
    File "", line 1, characters 4-4:
    Syntax error after ',' and before ''.
    Expected list element after ","
  |}]
