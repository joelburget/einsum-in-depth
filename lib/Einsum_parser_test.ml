type 'a parser = (Lexing.lexbuf -> Einsum_parser.token) -> Lexing.lexbuf -> 'a

let parse : type a. (a -> string) -> a parser -> string -> unit =
 fun to_string parser str ->
  try
    let lexbuf = Lexing.from_string (str ^ "\n") in
    while true do
      let result =
        parser
          (fun lexbuf ->
            let tok = Einsum_lexer.token lexbuf in
            tok)
          lexbuf
      in
      print_endline (to_string result);
      flush stdout
    done
  with Einsum_lexer.Eof -> ()

let parse_rewrite : string -> unit =
  parse Einops.Rewrite.to_string Einsum_parser.rewrite

let parse_atom : string -> unit =
  parse Einops.Atom.to_string Einsum_parser.atom_top

let%expect_test "Atoms" =
  parse_atom "a";
  parse_atom "()";
  parse_atom "(a b c)";
  parse_atom "...";
  [%expect {|
    a
    ()
    (a b c)
    ...
  |}]

let%expect_test "Rewrites" =
  parse_rewrite "a -> a";
  parse_rewrite "() -> ...";
  parse_rewrite "... (a b) () -> a b";
  [%expect {|
    a -> a
    () -> ...
    ... (a b) () -> a b |}]
