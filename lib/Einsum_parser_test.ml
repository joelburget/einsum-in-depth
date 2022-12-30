type 'a parser = Lexing.position -> 'a Einsum_parser_runner.I.checkpoint

let parse : type a. a Fmt.t -> a parser -> string -> unit =
 fun pp parser str ->
  match Einsum_parser_runner.parse parser (str ^ "\n") with
  | Ok v -> Fmt.(pr "%a@." pp) v
  | Error (location, indication, message) ->
      Fmt.pr "%s%s%s@." location indication message

let parse_rewrite : string -> unit =
  parse Einops.Rewrite.pp Einsum_parser.Incremental.rewrite

let parse_atom : string -> unit =
  parse Einops.Atom.pp Einsum_parser.Incremental.atom_top

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

let%expect_test "Errors" =
  parse_atom ")";
  parse_rewrite "(";
  [%expect
    {|
    File "", line 1, characters 0-1:
    Syntax error before ')'.
    Unexpected token.

    File "", line 1, characters 1-2:
    Syntax error after '(' and before ' '.
    After "(", expected a list of variable names. |}]
