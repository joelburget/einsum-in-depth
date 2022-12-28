type 'a parser = (Lexing.lexbuf -> Type_parser.token) -> Lexing.lexbuf -> 'a

let parse str =
  try
    let lexbuf = Lexing.from_string str in
    let result = Type_parser.ty Type_lexer.token lexbuf in
    print_endline (Tensor_type.to_string result);
    flush stdout
  with Type_lexer.Eof -> ()

let%expect_test _ =
  parse "[]";
  parse "[a]";
  parse "[1]";
  parse "[abc, def, 123, 456]";
  [%expect {|
    []
    [a]
    [1]
    [abc; def; 123; 456]
  |}]
