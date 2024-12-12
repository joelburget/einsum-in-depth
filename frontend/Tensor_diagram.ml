open Tensor_playground.Einops

(* open Cytoscape *)
module String_set = Set.Make (String)

module Node : sig
  type node_type = Edge | Tensor
  type t = { id : string; label : string; node_type : node_type }

  val to_jv : t -> Jv.t
end = struct
  type node_type = Edge | Tensor
  type t = { id : string; label : string; node_type : node_type }

  let to_jv { id; label; node_type } =
    Jv.obj
      [|
        ( "data",
          Jv.obj
            [|
              ("id", Jv.of_string id);
              ("label", Jv.of_string label);
              ( "type",
                Jv.of_string
                  (match node_type with Edge -> "edge" | Tensor -> "tensor") );
            |] );
      |]
end

module Edge : sig
  type t = { color : string; label : string; source : string; target : string }

  val to_jv : t -> Jv.t
end = struct
  type t = { color : string; label : string; source : string; target : string }

  let to_jv { color; label; source; target } =
    Jv.obj
      [|
        ( "data",
          Jv.obj
            [|
              ("color", Jv.of_string color);
              ("label", Jv.of_string label);
              ("source", Jv.of_string source);
              ("target", Jv.of_string target);
            |] );
      |]
end

let classes = Frontend_util.classes

(* let list_subtraction l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1 *)
(* let contracted_color () = if Colors.prefers_dark () then "#fff" else "#000" *)
(* let node_color () = if Colors.prefers_dark () then "#fff" else "#000" *)
let bg_color () = if Colors.prefers_dark () then "#00000080" else "#fff"

let rec list_count needle = function
  | [] -> 0
  | x :: xs -> (if x = needle then 1 else 0) + list_count needle xs

let draw_einsum edge_attributes lhs rhs =
  let get_color name = (Hashtbl.find edge_attributes name).Colors.color in

  let Rewrite.{ free; summation } = Rewrite.indices (lhs, rhs) in
  let flat_lhs = List.concat lhs in
  let self_contracted, contracted_in_pair, contracted_in_group =
    List.fold_left
      (fun (selves, pairs, groups) edge ->
        let count = list_count edge flat_lhs in
        if count = 1 then (String_set.add edge selves, pairs, groups)
        else if count = 2 then (selves, String_set.add edge pairs, groups)
        else (selves, pairs, String_set.add edge groups))
      (String_set.empty, String_set.empty, String_set.empty)
      (String_set.to_list summation)
  in

  let tensor_nodes =
    lhs
    |> List.mapi (fun i _x ->
           let id = Fmt.str "tensor-%d" i in
           Node.{ id; label = ""; node_type = Tensor })
    |> Array.of_list
  in

  let uncontracted_nodes =
    rhs
    |> List.map (fun x -> Node.{ id = x; label = x; node_type = Edge })
    |> Array.of_list
  in

  let contraction_group_nodes =
    contracted_in_group |> String_set.to_list
    |> List.map (fun x ->
           Node.{ id = Fmt.str "group-%s" x; label = ""; node_type = Edge })
    |> Array.of_list
  in

  let nodes =
    Array.concat [ tensor_nodes; uncontracted_nodes; contraction_group_nodes ]
  in

  let edges = Queue.create () in
  let pair_edges_seen = Hashtbl.create 10 in

  let find_partner i edge_name =
    let found_ix =
      List.fold_left
        (fun acc (j, edge_names) ->
          if i = j || Option.is_some acc then acc
          else if List.mem edge_name edge_names then
            Some (Fmt.str "tensor-%d" j)
          else acc)
        None
        (List.mapi (fun i x -> (i, x)) lhs)
    in
    match found_ix with Some j -> j | None -> raise Not_found
  in

  List.iteri
    (fun source_n edge_names ->
      List.iter
        (fun name ->
          let contract_in_pair = String_set.mem name contracted_in_pair in
          if contract_in_pair && Hashtbl.mem pair_edges_seen name then ()
          else
            let target =
              if String_set.mem name free then name
              else if String_set.mem name self_contracted then
                Fmt.str "tensor-%d" source_n
              else if contract_in_pair then find_partner source_n name
              else Fmt.str "group-%s" name
            in
            if contract_in_pair then Hashtbl.add pair_edges_seen name ();
            let edge =
              Edge.
                {
                  color = get_color name;
                  label = name;
                  source = Fmt.str "tensor-%d" source_n;
                  target;
                }
            in
            Queue.add edge edges)
        edge_names)
    lhs;

  let edges = edges |> Queue.to_seq |> Array.of_seq in

  let el =
    Brr.El.div
      ~at:
        (Brr.At.
           [ style (Jstr.v (Fmt.str "background-color: %s;" (bg_color ()))) ]
        @ classes "mx-auto")
      []
  in

  let _ =
    Jv.call Jv.global "renderTensorDiagram"
      [|
        Brr.El.to_jv el;
        Jv.of_array Node.to_jv nodes;
        Jv.of_array Edge.to_jv edges;
      |]
  in

  el
