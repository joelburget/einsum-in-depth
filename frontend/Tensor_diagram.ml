open Tensor_playground.Einops

let list_subtraction l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

let draw_contraction (l_tensor, r_tensor)
    Binary_contraction.{ contracted; zipped; _ } =
  let open Cytoscape in
  let left_uncontracted : string list =
    list_subtraction l_tensor (contracted @ zipped)
  in
  let right_uncontracted : string list =
    list_subtraction r_tensor (contracted @ zipped)
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
    zipped |> List.map (fun x -> Node.{ id = x; label = x; node_type = Edge })
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
    zipped
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
      ~zipped ()
  in
  Brr.Console.log [ opts ];
  let _ : Cytoscape.t = Cytoscape.create ~opts () in

  el
