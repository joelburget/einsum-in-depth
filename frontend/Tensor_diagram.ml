open Tensor_playground.Einops
module String_set = Set.Make (String)

let classes = Frontend_util.classes
let label_class_name = function Colors.{ fill_classes; _ } -> fill_classes

module Node : sig
  type node_type = Edge | Tensor

  type t = {
    id : string;
    label : string;
    node_type : node_type;
    class_name : string;
  }

  val to_jv : t -> Jv.t
end = struct
  type node_type = Edge | Tensor

  type t = {
    id : string;
    label : string;
    node_type : node_type;
    class_name : string;
  }

  let to_jv { id; label; class_name; node_type } =
    Jv.obj
      [|
        ("id", Jv.of_string id);
        ("label", Jv.of_string label);
        ("class_name", Jv.of_string class_name);
        ( "type",
          Jv.of_string
            (match node_type with Edge -> "edge" | Tensor -> "tensor") );
      |]
end

module Edge : sig
  type t = {
    class_name : string;
    label : string;
    source : string;
    target : string;
  }

  val to_jv : t -> Jv.t
end = struct
  type t = {
    class_name : string;
    label : string;
    source : string;
    target : string;
  }

  let to_jv { class_name; label; source; target } =
    Jv.obj
      [|
        ("class_name", Jv.of_string class_name);
        ("label", Jv.of_string label);
        ("source", Jv.of_string source);
        ("target", Jv.of_string target);
      |]
end

(** The number of lists this target appears in *)
let rec nested_list_count target = function
  | [] -> 0
  | lst :: lsts ->
      (if List.mem target lst then 1 else 0) + nested_list_count target lsts

let draw_einsum edge_attributes lhs rhs =
  let get_color name = Hashtbl.find edge_attributes name in

  let Rewrite.{ free; summation } = Rewrite.indices (lhs, rhs) in
  let self_contracted, contracted_in_pair, contracted_in_group =
    List.fold_left
      (fun (selves, pairs, groups) edge ->
        let count = nested_list_count edge lhs in
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
           Node.{ id; label = ""; node_type = Tensor; class_name = "" })
    |> Array.of_list
  in

  let uncontracted_nodes =
    rhs
    |> List.map (fun x ->
           let class_name = label_class_name (get_color x) in
           Node.{ id = x; label = x; node_type = Edge; class_name })
    |> Array.of_list
  in

  let contraction_group_nodes =
    contracted_in_group |> String_set.to_list
    |> List.map (fun x ->
           let id = Fmt.str "group-%s" x in
           Node.{ id; label = ""; node_type = Edge; class_name = "" })
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
    match found_ix with
    | Some j -> j
    | None ->
        Fmt.(
          pf stderr "@[find_partner %d %s lhs=([@[%a@]]) -> Not_found@]@." i
            edge_name
            (list ~sep:semi
               (parens (pair ~sep:comma int (brackets (list ~sep:sp string)))))
            (List.mapi (fun i x -> (i, x)) lhs));
        raise Not_found
  in

  List.iteri
    (fun source_n edge_names ->
      let self_contracted' =
        List.filter (fun x -> String_set.mem x self_contracted) edge_names
        |> String_set.of_list |> String_set.to_list
      in
      (match self_contracted' with
      | [] -> ()
      | _ ->
          let class_name =
            match self_contracted' with
            | [ x ] -> label_class_name (get_color x)
            | _ -> ""
          in
          let node_id = Fmt.str "tensor-%d" source_n in
          let edge =
            Edge.
              {
                class_name;
                label = Fmt.(str "%a" (list ~sep:comma string) self_contracted');
                source = node_id;
                target = node_id;
              }
          in
          Queue.add edge edges);

      List.iter
        (fun name ->
          let contracted_in_pair' = String_set.mem name contracted_in_pair in
          if
            (contracted_in_pair' && Hashtbl.mem pair_edges_seen name)
            || String_set.mem name self_contracted
          then ()
          else
            let target, label =
              if String_set.mem name free then (name, "")
              else if contracted_in_pair' then (find_partner source_n name, name)
              else (Fmt.str "group-%s" name, "")
            in
            if contracted_in_pair' then Hashtbl.add pair_edges_seen name ();
            Brr.Console.log [ "edge"; name; get_color name ];
            let edge =
              Edge.
                {
                  class_name = label_class_name (get_color name);
                  label;
                  source = Fmt.str "tensor-%d" source_n;
                  target;
                }
            in
            Queue.add edge edges)
        edge_names)
    lhs;

  let edges = edges |> Queue.to_seq |> Array.of_seq in

  let el = Brr.El.div ~at:(classes "mx-auto bg-white dark:bg-[#00000080]") [] in

  let _ =
    Jv.call Jv.global "renderTensorDiagram"
      [|
        Brr.El.to_jv el;
        Jv.of_array Node.to_jv nodes;
        Jv.of_array Edge.to_jv edges;
      |]
  in

  el
