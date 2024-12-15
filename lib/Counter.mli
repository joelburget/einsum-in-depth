module String_map :
  Map.S with type 'a t = 'a Map.Make(String).t and type key = string

module String_set : Set.S with type t = Set.Make(String).t and type elt = string

type t = int String_map.t

val make : string list -> t
(** Creates a map from a list of strings, counting their occurrences *)

val diff : t -> t -> t
(** Computes the difference between two maps *)

val get : t -> string -> int
(** Gets the count for a given string. Returns 0 if not found *)

val key_set : t -> String_set.t
(** Returns the set of all keys in the map *)

val to_list : t -> (string * int) list
(** Converts the map to an association list *)
