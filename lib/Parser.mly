%{
  (* Work around ocaml/dune#2450 *)
  module Tensor_playground = struct end
%}

%token <string> STR
%token ARROW ELLIPSIS
%token LPAREN RPAREN
%token EOL

(* %nonassoc ARROW *)

%start <Einops.Rewrite.t> rewrite
%start <Einops.Atom.t>    atom_top

%%

rewrite:
| left = atom+ ARROW right = atom+ EOL
  { left, right }

atom_top: atom = atom EOL { atom }

atom:
| name = STR
  { Name name }
| LPAREN names = STR* RPAREN
  { Group names }
| ELLIPSIS
  { Ellipsis }
