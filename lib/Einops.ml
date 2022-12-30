module Atom = struct
  type t = Name of string | Ellipsis | Group of string list

  let pp ppf = function
    | Name name -> Fmt.string ppf name
    | Ellipsis -> Fmt.string ppf "..."
    | Group names -> Fmt.(parens (list ~sep:sp string)) ppf names
end

module Bindings = struct
  type t = Atom.t list

  let pp = Fmt.(list ~sep:(any " ") Atom.pp)
end

module Rewrite = struct
  type t = Bindings.t * Bindings.t

  let pp = Fmt.(pair ~sep:(any " -> ") Bindings.pp Bindings.pp)
end

module Op_kind = struct
  type t = Rearrange | Reduce | Repeat

  let pp ppf = function
    | Rearrange -> Fmt.pf ppf "rearrange"
    | Reduce -> Fmt.pf ppf "reduce"
    | Repeat -> Fmt.pf ppf "repeat"
end

module Equation = struct
  type t = string * int

  let pp ppf (name, i) = Fmt.pf ppf "%s = %d" name i
end

module Op = struct
  type t = Op of Op_kind.t * Rewrite.t * Equation.t list

  let pp ppf (Op (op_kind, rewrite, eqns)) =
    Fmt.(
      pf ppf "Op @[(%a,@ %a,@ %a)@]" Op_kind.pp op_kind Rewrite.pp rewrite
        (brackets (list ~sep:(any " ") Equation.pp))
        eqns)
end
