open Tensor_playground.Einops
open Cytoscape

let list_subtraction l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

(*
let node_colors =
  [| "0000CD"; "8B0000"; "4B0082"; "556B2F"; "FA8072"; "008080" |]

let get_color i =
  "#" ^ if i < Array.length node_colors then node_colors.(i) else "000000"
  *)

let draw_unary_contraction Unary_contraction.{ contracted; preserved; _ } =
  let dangling_nodes =
    preserved
    |> List.map (fun x -> Node.{ id = x; label = x; node_type = Edge })
    |> Array.of_list
  in
  let nodes =
    Array.append
      [| Node.{ id = "tensor"; label = ""; node_type = Tensor } |]
      dangling_nodes
  in

  let edges1 =
    contracted
    |> List.map (fun name ->
           Edge.
             {
               id = Fmt.str "contracted-%s" name;
               label = name;
               source = "tensor";
               target = "tensor";
             })
    |> Array.of_list
  in

  let edges2 =
    preserved
    |> List.map (fun name ->
           Edge.
             {
               id = Fmt.str "contracted-%s" name;
               label = "";
               source = "tensor";
               target = Fmt.str "preserved-%s" name;
             })
    |> Array.of_list
  in

  let edges = Array.append edges1 edges2 in

  let elements = Elements.{ nodes; edges } in
  let el =
    Brr.El.div ~at:Brr.At.[ style (Jstr.v "background-color: white;") ] []
  in
  let opts =
    Cytoscape.opts ~container:el ~elements
      ~fixed:[| ("tensor", 300, 300) |]
      ~zipped:[] ()
  in
  let _ : Cytoscape.t = Cytoscape.create ~opts () in

  el

let draw_binary_contraction l_tensor r_tensor
    Binary_contraction.{ contracted; aligned; _ } =
  let left_uncontracted : string list =
    list_subtraction l_tensor (contracted @ aligned)
  in
  let right_uncontracted : string list =
    list_subtraction r_tensor (contracted @ aligned)
  in

  let tensor_nodes =
    Node.
      [
        { id = "left"; label = ""; node_type = Tensor };
        { id = "right"; label = ""; node_type = Tensor };
      ]
  in

  let left_uncontracted_nodes =
    left_uncontracted
    |> List.map (fun x ->
           Node.
             {
               id = Fmt.str "left-uncontracted-%s" x;
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
               label = x;
               node_type = Edge;
             })
  in
  let zipped_nodes =
    aligned |> List.map (fun x -> Node.{ id = x; label = x; node_type = Edge })
  in

  let l_uncontracted_edges =
    left_uncontracted
    |> List.map (fun name ->
           Edge.
             {
               id = Fmt.str "%s-left" name;
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
               label = "";
               source = Fmt.str "right-uncontracted-%s" name;
               target = "right";
             })
  in
  let zipped_edges =
    aligned
    |> List.map (fun name ->
           Edge.
             [
               {
                 id = Fmt.str "zipped-left-%s" name;
                 label = "";
                 source = "left";
                 target = name;
               };
               {
                 id = Fmt.str "zipped-%s-right" name;
                 label = "";
                 source = name;
                 target = "right";
               };
             ])
    |> List.flatten
  in

  let contracted_edge =
    Edge.
      {
        id = Fmt.str "%s-contracted" (String.concat "-" contracted);
        label = String.concat ", " contracted;
        source = "left";
        target = "right";
      }
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
           @ [ contracted_edge ]);
      }
  in

  let el =
    Brr.El.div ~at:Brr.At.[ style (Jstr.v "background-color: white;") ] []
  in
  let opts =
    Cytoscape.opts ~container:el ~elements
      ~fixed:[| ("left", 200, 300); ("right", 400, 300) |]
      ~zipped:aligned ()
  in
  let _ : Cytoscape.t = Cytoscape.create ~opts () in

  el
