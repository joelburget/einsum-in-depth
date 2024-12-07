module P = Einsum_parser.Friendly

let go : string -> unit =
 fun str ->
  match P.parse str with
  | Ok rewrite -> Fmt.pr "%a@." Einops.Rewrite.pp rewrite
  | Error msg -> Fmt.pr "Error: %s@." msg

let%expect_test "Rewrites" =
  go "a -> a";
  go "i, i j -> j";
  go "i, i j ->";
  [%expect {|
    a -> a
    i, i j -> j
    i, i j -> |}]

let%expect_test "errors" =
  go "a,b->c";
  [%expect
    {| Error: Result indices must be a subset of the input indices ([c] are not) |}];
  go "i -> i i";
  [%expect {| Error: Result indices must not be repeated ([i]) |}]
