exception Error of string

module Token : sig
  type t = Str of string | Arrow | Comma | Eof

  val pp : Format.formatter -> t -> unit
end = struct
  type t = Str of string | Arrow | Comma | Eof

  let pp ppf = function
    | Str s -> Format.fprintf ppf "Str %s" s
    | Arrow -> Format.fprintf ppf "Arrow"
    | Comma -> Format.fprintf ppf "Comma"
    | Eof -> Format.fprintf ppf "Eof"
end

open Token

module Lex_impl : sig
  val lex_unsafe : string -> Token.t list
end = struct
  open Token

  let lex_unsafe input =
    let rec aux i acc =
      if i >= String.length input then List.rev (Eof :: acc)
      else
        match input.[i] with
        | ' ' | '\t' -> aux (i + 1) acc
        | '-' when i + 1 < String.length input && input.[i + 1] = '>' ->
            aux (i + 2) (Arrow :: acc)
        | ',' -> aux (i + 1) (Comma :: acc)
        | c
          when (c >= 'a' && c <= 'z')
               || (c >= 'A' && c <= 'Z')
               || (c >= '0' && c <= '9')
               || c == '_' ->
            let start = i in
            let rec find_end i =
              if i >= String.length input then i
              else
                match input.[i] with
                | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> find_end (i + 1)
                | _ -> i
            in
            let end_ = find_end i in
            aux end_ (Str (String.sub input start (end_ - start)) :: acc)
        | _ ->
            raise
              (Error
                 (Printf.sprintf "At offset %d: unexpected character '%c'.\n" i
                    input.[i]))
    in
    aux 0 []
end

module Friendly = struct
  let rec group_unsafe = function
    | Str s :: t ->
        let strs, t' = group_unsafe t in
        (s :: strs, t')
    | toks -> ([], toks)

  let groups_unsafe tokens =
    let rec go current_group acc = function
      | Str s :: ts -> go (current_group @ [ s ]) acc ts
      | Comma :: ts -> go [] (acc @ [ current_group ]) ts
      | ((Arrow | Eof) as tok) :: ts -> (acc @ [ current_group ], tok :: ts)
      | [] -> (acc @ [ current_group ], [])
    in
    go [] [] tokens

  let rewrite_unsafe tokens =
    let lhs, rest = groups_unsafe tokens in
    match rest with
    | Arrow :: t -> (
        match group_unsafe t with
        | g, ([] | [ Eof ]) -> (lhs, g)
        | _, toks ->
            raise
              (Error
                 Fmt.(
                   str "Expected a group, got [@[%a@]]"
                     (list ~sep:comma Token.pp) toks)))
    | ts ->
        raise
          (Error
             Fmt.(
               str "Expected an arrow, got [@[%a@]]" (list ~sep:comma Token.pp)
                 ts))

  let parse_unsafe str =
    let candidate = str |> Lex_impl.lex_unsafe |> rewrite_unsafe in
    match Einops.Rewrite.validate candidate with
    | None -> candidate
    | Some e -> raise (Error e)

  let parse str =
    match parse_unsafe str with x -> Ok x | exception Error e -> Error e
end

module Original = struct
  let chars_to_strings str =
    String.to_seq str |> Seq.map (String.make 1) |> List.of_seq

  let groups_unsafe tokens =
    let rec go acc toks =
      match toks with
      | Str s :: ts -> (
          let group = chars_to_strings s in
          match ts with
          | Str _ :: _ ->
              raise
                (Error
                   Fmt.(
                     str "Expected a comma or arrow, got [@[%a@]]"
                       (list Token.pp) ts))
          | [] -> raise (Error "Expected more tokens")
          | Comma :: ts -> go (group :: acc) ts
          | ((Arrow | Eof) as tok) :: ts -> (List.rev (group :: acc), tok :: ts)
          )
      | [] -> (List.rev acc, [])
      | (Comma | Arrow | Eof) :: _ ->
          raise
            (Error
               Fmt.(str "Expected a group, got [@[%a@]]" (list Token.pp) toks))
    in
    go [] tokens

  let rewrite_unsafe tokens =
    let lhs, rest = groups_unsafe tokens in
    match rest with
    | Arrow :: ts -> (
        match ts with
        | [ Str s; Eof ] -> (lhs, chars_to_strings s)
        | [ Eof ] -> (lhs, [])
        | toks ->
            raise
              (Error
                 Fmt.(
                   str "Expected a group or EOF, got [@[%a@]]"
                     (list ~sep:comma Token.pp) toks)))
    | [ Eof ] -> (lhs, [])
    | ts ->
        raise
          (Error
             Fmt.(
               str "Expected an arrow or EOF, got [@[%a@]]"
                 (list ~sep:comma Token.pp) ts))

  let parse_unsafe str =
    let candidate = str |> Lex_impl.lex_unsafe |> rewrite_unsafe in
    match Einops.Rewrite.validate candidate with
    | None -> candidate
    | Some e -> raise (Error e)

  let parse str =
    match parse_unsafe str with x -> Ok x | exception Error e -> Error e
end
