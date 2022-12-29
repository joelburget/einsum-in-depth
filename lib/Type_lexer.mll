{
  open Type_parser

  exception Error of string
  exception Eof
}

rule token = parse
| [' ' '\t']
    { token lexbuf }
| ['0'-'9']+ as num
    { NUM num }
| ","
    { COMMA }
| '['
    { LBRACKET }
| ']'
    { RBRACKET }
(* | '\n' { EOL } *)
| eof
    { raise Eof }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

