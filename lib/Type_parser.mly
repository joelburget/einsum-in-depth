%token <string> VAR
%token <string> NUM
%token COMMA
%token LBRACKET RBRACKET

%start <Tensor_type.t> bracketed
%start <Tensor_type.t> unbracketed
%start <Tensor_type.t * Tensor_type.bracketed> lax

%%

item:
  | n = NUM { Concrete (int_of_string n) }
  | v = VAR { Variable v }

unbracketed:
  | items = separated_list(COMMA, item)
  { items }

bracketed:
  | LBRACKET items = unbracketed RBRACKET
  { items }

lax:
  | items = bracketed
  { items, Tensor_type.Bracketed }
  | items = unbracketed
  { items, Tensor_type.Unbracketed }
