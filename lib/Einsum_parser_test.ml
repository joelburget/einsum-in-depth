module P = Einsum_parser.Friendly

type 'a parser = P.Token.t list -> 'a

let parse : type a. a Fmt.t -> a parser -> string -> unit =
 fun pp parser str ->
  match P.lex str with
  | Ok tokens -> Fmt.pr "%a@." pp (parser tokens)
  | Error msg -> Fmt.pr "%s@." msg

let parse_rewrite : string -> unit = parse Einops.Rewrite.pp P.rewrite_unsafe

let parse_groups : string -> unit =
  parse
    Fmt.(box (list ~sep:comma Einops.Group.pp))
    (fun toks -> toks |> P.groups_unsafe |> fst)

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

let%expect_test "errors" =
  let go str =
    match P.parse str with
    | Ok x -> Fmt.pr "%a\n" Einops.Rewrite.pp x
    | Error e -> Fmt.pr "Error: %s\n" e
  in
  go "a,b->c";
  [%expect
    {| Error: Result indices must be a subset of the input indices ([c] are not) |}];
  go "i -> i i";
  [%expect {| Error: Result indices must not be repeated ([i]) |}]
