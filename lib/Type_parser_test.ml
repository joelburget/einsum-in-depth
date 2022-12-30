type 'a parser = (Lexing.lexbuf -> Type_parser.token) -> Lexing.lexbuf -> 'a

let parse parser str =
  let lexbuf = Lexing.from_string str in
  let result = parser Type_lexer.token lexbuf in
  Fmt.(pr "%a\n" (brackets (list ~sep:comma int))) result;
  flush stdout
(* with Type_lexer.Eof -> Fmt.pr "EOF!\n" *)

let%expect_test _ =
  let go = parse Type_parser.bracketed in
  go "[]";
  go "[1]";
  go "[123, 456]";
  let go = parse Type_parser.unbracketed in
  go "";
  go "1";
  go "123, 456";
  [%expect {|
    []
    [1]
    [123, 456]
    []
    [1]
    [123, 456]
  |}]
