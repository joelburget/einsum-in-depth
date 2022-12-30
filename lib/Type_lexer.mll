{
  open Type_parser

  exception Error of string
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
| eof
  { EOF }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

