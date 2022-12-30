%{
  (* Work around ocaml/dune#2450 *)
  module Tensor_playground = struct end
%}

%token <string> STR
%token ARROW ELLIPSIS COMMA
%token LPAREN RPAREN
%token EOF

(* %nonassoc ARROW *)

%start <Einops.Rewrite.t> rewrite
%start <Einops.Atom.t>    atom_top
%start <Einops.Group.t>   group_top

%%

atom_top: atom = atom EOF { atom }
group_top: group = group EOF { group }

atom:
| name = STR
  { Name name }
| LPAREN names = STR* RPAREN
  { Parenthesized names }
| ELLIPSIS
  { Ellipsis }

group:
| group = atom+
  { group }

rewrite:
| bindings = separated_nonempty_list(COMMA, group) ARROW rhs = atom+ EOF
  { bindings, rhs }
