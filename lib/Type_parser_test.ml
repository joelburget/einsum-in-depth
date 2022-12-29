type 'a parser = (Lexing.lexbuf -> Type_parser.token) -> Lexing.lexbuf -> 'a

let parse parser str =
  try
    let lexbuf = Lexing.from_string str in
    let result = parser Type_lexer.token lexbuf in
    print_endline (Tensor_type.to_string result);
    flush stdout
  with Type_lexer.Eof -> ()

let%expect_test _ =
  let go = parse Type_parser.bracketed in
  go "[]";
  go "[a]";
  go "[1]";
  go "[abc, def, 123, 456]";
  let go = parse Type_parser.unbracketed in
  go "";
  go "a";
  go "1";
  go "abc, def, 123, 456";
  [%expect {|
    []
    [a]
    [1]
    [abc; def; 123; 456]
  |}]
