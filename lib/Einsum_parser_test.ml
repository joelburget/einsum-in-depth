let go_friendly str =
  match Einsum_parser.Friendly.parse str with
  | Ok rewrite -> Fmt.pr "%a@." Einops.Rewrite.pp_friendly rewrite
  | Error msg -> Fmt.pr "Error: %s@." msg

let go_original str =
  match Einsum_parser.Original.parse str with
  | Ok rewrite -> Fmt.pr "%a@." Einops.Rewrite.pp_original rewrite
  | Error msg -> Fmt.pr "Error: %s@." msg

let%expect_test "Rewrites" =
  go_friendly "a -> a";
  go_friendly "i, i j -> j";
  go_friendly "i, i j ->";
  go_original "i, ij ->";
  go_original "i, ij";
  [%expect
    {|
    a -> a
    i, i j -> j
    i, i j ->
    i, ij ->
    i, ij ->
  |}]

let%expect_test "errors" =
  go_friendly "a,b->c";
  go_original "a,b->c";
  [%expect
    {| 
    Error: Result indices must be a subset of the input indices ([c] are not)
    Error: Result indices must be a subset of the input indices ([c] are not) |}];
  go_friendly "i -> i i";
  go_original "i -> ii";
  [%expect
    {| 
    Error: Result indices must not be repeated ([i])
    Error: Result indices must not be repeated ([i]) |}]
