type 'a parser = Einsum_parser.Token.t list -> 'a

let parse : type a. a Fmt.t -> a parser -> string -> unit =
 fun pp parser str ->
  match Einsum_parser.lex str with
  | Ok tokens -> Fmt.pr "%a@." pp (parser tokens)
  | Error msg -> Fmt.pr "%s@." msg

let parse_rewrite : string -> unit =
  parse Einops.Rewrite.pp Einsum_parser.rewrite_unsafe

let parse_groups : string -> unit =
  parse
    Fmt.(box (list ~sep:comma Einops.Group.pp))
    (fun toks -> toks |> Einsum_parser.groups_unsafe |> fst)

let%expect_test "Groups" =
  parse_groups "a b c";
  parse_groups "a b c, d e f";
  [%expect {|
    a b c
    a b c, d e f
  |}]

let%expect_test "Rewrites" =
  parse_rewrite "a -> a";
  parse_rewrite "i, i j -> j";
  parse_rewrite "i, i j ->";
  [%expect {|
    a -> a
    i, i j -> j
    i, i j -> |}]
