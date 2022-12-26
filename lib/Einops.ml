let space_sep = Util.join ~sep:" "

module Atom = struct
  type t = Name of string | Ellipsis | Group of string list

  let to_string = function
    | Name name -> name
    | Ellipsis -> "..."
    | Group names -> "(" ^ space_sep names ^ ")"
end

module Bindings = struct
  type t = Atom.t list

  let to_string atoms = atoms |> List.map Atom.to_string |> space_sep
end

module Rewrite = struct
  type t = Bindings.t * Bindings.t

  let to_string (lhs, rhs) =
    Bindings.to_string lhs ^ " -> " ^ Bindings.to_string rhs
end

module Op_kind = struct
  type t = Rearrange | Reduce | Repeat

  let to_string = function
    | Rearrange -> "rearrange"
    | Reduce -> "reduce"
    | Repeat -> "repeat"
end

module Equation = struct
  type t = string * int

  let to_string (name, i) = name ^ "=" ^ Int.to_string i
end

module Op = struct
  type t = Op of Op_kind.t * Rewrite.t * Equation.t list

  let to_string (Op (op_kind, rewrite, eqns)) =
    let eqns_str = eqns |> List.map Equation.to_string |> space_sep in
    space_sep [ Op_kind.to_string op_kind; Rewrite.to_string rewrite; eqns_str ]
end
