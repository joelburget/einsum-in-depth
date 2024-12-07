exception Error of string

module Friendly = struct
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
                  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' ->
                      find_end (i + 1)
                  | _ -> i
              in
              let end_ = find_end i in
              aux end_ (Str (String.sub input start (end_ - start)) :: acc)
          | _ ->
              raise
                (Error
                   (Printf.sprintf "At offset %d: unexpected character '%c'.\n"
                      i input.[i]))
      in
      aux 0 []
  end

  module Parse_impl : sig
    val parse : string -> (Einops.Rewrite.t, string) result
  end = struct
    open Token

    let rec group_unsafe = function
      | Str s :: t ->
          let strs, t' = group_unsafe t in
          (s :: strs, t')
      | toks -> ([], toks)

    let groups_unsafe tokens =
      let rec go current_group acc = function
        | Str s :: t -> go (current_group @ [ s ]) acc t
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
                   (Fmt.str "Expected a group, got %a" (Fmt.list Token.pp) toks))
          )
      | ts ->
          raise
            (Error (Fmt.str "Expected an arrow, got %a" (Fmt.list Token.pp) ts))

    let parse_unsafe str =
      let candidate = str |> Lex_impl.lex_unsafe |> rewrite_unsafe in
      match Einops.Rewrite.validate candidate with
      | None -> candidate
      | Some e -> raise (Error e)

    let parse str =
      match parse_unsafe str with x -> Ok x | exception Error e -> Error e
  end

  include Lex_impl
  include Parse_impl
end
