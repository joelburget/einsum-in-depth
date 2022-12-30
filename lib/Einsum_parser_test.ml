type 'a parser = (Lexing.lexbuf -> Einsum_parser.token) -> Lexing.lexbuf -> 'a

let parse : type a. a Fmt.t -> a parser -> string -> unit =
 fun pp parser str ->
  try
    let lexbuf = Lexing.from_string (str ^ "\n") in
    while true do
      parser Einsum_lexer.token lexbuf |> Fmt.pr "%a@." pp
    done
  with Einsum_lexer.Eof -> ()

let parse_rewrite : string -> unit =
  parse Einops.Rewrite.pp Einsum_parser.rewrite

let parse_atom : string -> unit = parse Einops.Atom.pp Einsum_parser.atom_top

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
