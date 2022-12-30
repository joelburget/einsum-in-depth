%token <string> NUM
%token COMMA
%token LBRACKET RBRACKET
%token EOF

%start <int list> bracketed
%start <int list> unbracketed
%start <int list * Tensor_type.bracketed> lax

%%

item:
  | n = NUM { int_of_string n }

unbracketed:
  | items = unbracketed_ EOF
  { items }

unbracketed_:
  | items = separated_list(COMMA, item)
  { items }

bracketed:
  | LBRACKET items = unbracketed_ RBRACKET
  { items }

lax:
  | items = bracketed
  { items, Tensor_type.Bracketed }
  | items = unbracketed
  { items, Tensor_type.Unbracketed }
