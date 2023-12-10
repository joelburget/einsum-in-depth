type 'a parser = Lexing.position -> 'a Einsum_parser_runner.I.checkpoint

let parse : type a. a Fmt.t -> a parser -> string -> unit =
 fun pp parser str ->
  match Einsum_parser_runner.parse parser str with
  | Ok v -> Fmt.pr "%a@." pp v
  | Error (location, indication, message) ->
      Fmt.pr "%s%s%s@." location indication message

let parse_rewrite : string -> unit =
  parse Einops.Rewrite.pp Einsum_parser.Incremental.rewrite

let parse_group : string -> unit =
  parse Einops.Group.pp Einsum_parser.Incremental.group_top

let%expect_test "Groups" =
  parse_group "a b c";
  [%expect {| a b c |}]

let%expect_test "Rewrites" =
  parse_rewrite "a -> a";
  parse_rewrite "i, i j -> j";
  parse_rewrite "i, i j ->";
  [%expect {|
    a -> a
    i, i j -> j
    i, i j -> |}]

let%expect_test "Errors" =
  parse_rewrite "(";
  [%expect
    {|
    File "", line 1, characters 0-1:
    Syntax error before '('.
    Unexpected token. |}]
