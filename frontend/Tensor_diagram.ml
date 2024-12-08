open Tensor_playground.Einops
open Cytoscape

let classes = Frontend_util.classes
let list_subtraction l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1
let contracted_color () = if Colors.prefers_dark () then "#fff" else "#000"
let node_color () = if Colors.prefers_dark () then "#fff" else "#000"
let bg_color () = if Colors.prefers_dark () then "#00000080" else "#fff"

let draw_unary_contraction edge_attributes
    Unary_contraction.{ contracted; preserved; _ } =
  let get_color name = (Hashtbl.find edge_attributes name).Colors.color in
  let dangling_nodes =
    preserved
    |> List.map (fun x ->
           Node.{ id = x; color = get_color x; label = x; node_type = Edge })
    |> Array.of_list
  in
  let nodes =
    Array.append
      [|
        Node.
          {
            id = "tensor";
            color = node_color ();
            label = "";
            node_type = Tensor;
          };
      |]
      dangling_nodes
  in

  let contracted_edges =
    contracted
    |> List.map (fun name ->
           Edge.
             {
               id = Fmt.str "contracted-%s" name;
               color = get_color name;
               label = name;
               source = "tensor";
               target = "tensor";
             })
    |> Array.of_list
  in

  let preserved_edges =
    preserved
    |> List.map (fun name ->
           Edge.
             {
               id = Fmt.str "preserved-%s" name;
               color = get_color name;
               label = "";
               source = "tensor";
               target = name;
             })
    |> Array.of_list
  in

  let elements =
    Elements.{ nodes; edges = Array.append contracted_edges preserved_edges }
  in
  let el =
    Brr.El.div
      ~at:
        Brr.At.
          [ style (Jstr.v (Fmt.str "background-color: %s;" (bg_color ()))) ]
      []
  in
  let opts =
    Cytoscape.opts ~container:el ~elements
      ~fixed:[| ("tensor", 250, 160) |]
      ~zipped:[] ()
  in
  let _ : Cytoscape.t = Cytoscape.create ~opts () in

  el

let draw_binary_contraction edge_attributes l_tensor r_tensor
    Binary_contraction.{ contracted; aligned; _ } =
  let left_uncontracted : string list =
    list_subtraction l_tensor (contracted @ aligned)
  in
  let right_uncontracted : string list =
    list_subtraction r_tensor (contracted @ aligned)
  in

  let get_color name = (Hashtbl.find edge_attributes name).Colors.color in

  let tensor_nodes =
    Node.
      [
        { id = "left"; color = node_color (); label = ""; node_type = Tensor };
        { id = "right"; color = node_color (); label = ""; node_type = Tensor };
      ]
  in

  let left_uncontracted_nodes =
    left_uncontracted
    |> List.map (fun x ->
           Node.
             {
               id = Fmt.str "left-uncontracted-%s" x;
               color = get_color x;
               label = x;
               node_type = Edge;
             })
  in
  let right_uncontracted_nodes =
    right_uncontracted
    |> List.map (fun x ->
           Node.
             {
               id = Fmt.str "right-uncontracted-%s" x;
               color = get_color x;
               label = x;
               node_type = Edge;
             })
  in
  let zipped = aligned |> List.filter (fun x -> not (List.mem x contracted)) in
  let zipped_nodes =
    zipped
    |> List.map (fun x ->
           Node.{ id = x; color = get_color x; label = x; node_type = Edge })
  in

  let l_uncontracted_edges =
    left_uncontracted
    |> List.map (fun name ->
           Edge.
             {
               id = Fmt.str "%s-left" name;
               color = get_color name;
               label = "";
               source = Fmt.str "left-uncontracted-%s" name;
               target = "left";
             })
  in
  let r_uncontracted_edges =
    right_uncontracted
    |> List.map (fun name ->
           Edge.
             {
               id = Fmt.str "%s-right" name;
               color = get_color name;
               label = "";
               source = Fmt.str "right-uncontracted-%s" name;
               target = "right";
             })
  in
  let zipped_edges =
    zipped
    |> List.map (fun name ->
           let color = get_color name in
           Edge.
             [
               {
                 id = Fmt.str "zipped-left-%s" name;
                 color;
                 label = "";
                 source = "left";
                 target = name;
               };
               {
                 id = Fmt.str "zipped-%s-right" name;
                 color;
                 label = "";
                 source = name;
                 target = "right";
               };
             ])
    |> List.flatten
  in

  let contracted_edges =
    match contracted with
    | [] -> []
    | _ ->
        let color =
          match contracted with
          | [ name ] -> get_color name
          | _ -> contracted_color ()
        in
        [
          Edge.
            {
              id = Fmt.str "%s-contracted" (String.concat "-" contracted);
              color;
              label = String.concat ", " contracted;
              source = "left";
              target = "right";
            };
        ]
  in

  let elements =
    Elements.
      {
        nodes =
          Array.of_list
            (tensor_nodes @ left_uncontracted_nodes @ right_uncontracted_nodes
           @ zipped_nodes);
        edges =
          Array.of_list
            (l_uncontracted_edges @ r_uncontracted_edges @ zipped_edges
           @ contracted_edges);
      }
  in

  let el =
    Brr.El.div
      ~at:
        (Brr.At.
           [ style (Jstr.v (Fmt.str "background-color: %s;" (bg_color ()))) ]
        @ classes "mx-auto")
      []
  in
  let opts =
    Cytoscape.opts ~container:el ~elements
      ~fixed:[| ("left", 150, 160); ("right", 350, 160) |]
      ~zipped ()
  in
  let _ : Cytoscape.t = Cytoscape.create ~opts () in

  el

let draw_einsum _edge_attributes _lhs _rhs =
  let elements =
    (* TODO *)
    Elements.{ nodes = [||]; edges = [||] }
  in

  (* TODO *)
  let zipped = [] in

  let el =
    Brr.El.div
      ~at:
        (Brr.At.
           [ style (Jstr.v (Fmt.str "background-color: %s;" (bg_color ()))) ]
        @ classes "mx-auto")
      []
  in
  let opts =
    Cytoscape.opts ~container:el ~elements
      ~fixed:[| ("left", 150, 160); ("right", 350, 160) |]
      ~zipped ()
  in
  let _ : Cytoscape.t = Cytoscape.create ~opts () in

  el
