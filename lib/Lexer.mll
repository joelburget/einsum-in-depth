{
  open Parser

  exception Error of string
  exception Eof
}

rule token = parse
| [' ' '\t']
    { token lexbuf }
| ['a'-'z' 'A'-'Z' '0'-'9']+ as str
    { STR str }
| "->"
    { ARROW }
| "..."
    { ELLIPSIS }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '\n' { EOL }
| eof
    { raise Eof }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

