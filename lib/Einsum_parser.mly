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
%start <Einops.Group.t>   group_top

%%

group_top: group = group EOF { group }

group:
| group = STR+
  { group }

rewrite:
| bindings = separated_nonempty_list(COMMA, group) ARROW rhs = STR* EOF
  { bindings, rhs }
