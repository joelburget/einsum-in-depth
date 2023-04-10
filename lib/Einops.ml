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
