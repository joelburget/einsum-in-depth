module Atom = struct
  type t = Name of string | Ellipsis | Parenthesized of string list

  let pp ppf = function
    | Name name -> Fmt.string ppf name
    | Ellipsis -> Fmt.string ppf "..."
    | Parenthesized names -> Fmt.(parens (list ~sep:sp string)) ppf names
end

module Group = struct
  type t = Atom.t list

  let pp = Fmt.(box (list ~sep:sp Atom.pp))
end

module Bindings = struct
  type t = Group.t list

  let pp = Fmt.(box (list ~sep:comma Group.pp))
end

module Rewrite = struct
  type t = Bindings.t * Group.t

  let pp = Fmt.(box (pair ~sep:(any " -> ") Bindings.pp Group.pp))
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
