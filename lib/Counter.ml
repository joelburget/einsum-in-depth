module String_map = Map.Make (String)
module String_set = Set.Make (String)

type t = int String_map.t

let add_to_count map x =
  String_map.update x (function None -> Some 1 | Some n -> Some (n + 1)) map

let make = List.fold_left add_to_count String_map.empty
let key_set m = String_map.to_seq m |> Seq.map fst |> String_set.of_seq
let to_list m = String_map.bindings m

let diff a b =
  let keys = String_set.(union (key_set a) (key_set b) |> to_list) in
  List.fold_left
    (fun acc key ->
      let a_val = String_map.find_opt key a |> Option.value ~default:0 in
      let b_val = String_map.find_opt key b |> Option.value ~default:0 in
      if a_val = b_val then acc else String_map.add key (a_val - b_val) acc)
    String_map.empty keys

let get m k = String_map.find_opt k m |> Option.value ~default:0
