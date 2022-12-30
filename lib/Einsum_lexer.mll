{
  open Einsum_parser

  exception Error of string
}

rule token = parse
| [' ' '\t']
    { token lexbuf }
| ['a'-'z' 'A'-'Z' '0'-'9']+ as str
    { STR str }
| "->"
    { ARROW }
| ","
    { COMMA }
| "..."
    { ELLIPSIS }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| eof
    { EOF }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

