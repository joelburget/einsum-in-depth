%token <string> VAR
%token <string> NUM
%token COMMA
%token LBRACKET RBRACKET

%start <Tensor_type.t> ty

%%

item:
  | n = NUM { Concrete (int_of_string n) }
  | v = VAR { Variable v }

ty:
  | LBRACKET items = separated_list(COMMA, item) RBRACKET
  { items }
