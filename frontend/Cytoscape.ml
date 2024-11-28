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
  type t = {
    id : string;
    color : string;
    label : string;
    source : string;
    target : string;
  }

  val to_jv : t -> Jv.t
end = struct
  type t = {
    id : string;
    color : string;
    label : string;
    source : string;
    target : string;
  }

  let to_jv { id; color; label; source; target } =
    Jv.obj
      [|
        ( "data",
          Jv.obj
            [|
              ("id", Jv.of_string id);
              ("color", Jv.of_string color);
              ("label", Jv.of_string label);
              ("source", Jv.of_string source);
              ("target", Jv.of_string target);
            |] );
      |]
end

module Elements : sig
  type t = { nodes : Node.t array; edges : Edge.t array }

  val to_jv : t -> Jv.t
end = struct
  type t = { nodes : Node.t array; edges : Edge.t array }

  let to_jv { nodes; edges } =
    Jv.obj
      [|
        ("nodes", Jv.of_array Node.to_jv nodes);
        ("edges", Jv.of_array Edge.to_jv edges);
      |]
end

module String_set = Set.Make (String)

module Cytoscape : sig
  type t
  type opts

  val opts :
    container:Brr.El.t ->
    elements:Elements.t ->
    fixed:(string * int * int) array ->
    zipped:string list ->
    unit ->
    opts

  val create : opts:opts -> unit -> t
end = struct
  type t = Jv.t
  type opts = Jv.t

  type relative_constraint =
    | Horizontal of { left : string; right : string }
    | Vertical of { top : string; bottom : string }

  let appears_once (arr : string array) =
    let seen = Hashtbl.create 2 in
    let seen_once = Hashtbl.create 2 in
    Array.iter
      (fun x ->
        if not (Hashtbl.mem seen x) then (
          Hashtbl.add seen_once x ();
          Hashtbl.add seen x ())
        else Hashtbl.remove seen_once x)
      arr;
    Hashtbl.fold (fun k _ acc -> k :: acc) seen_once [] |> String_set.of_list

  let get_relative_constraints (elements : Elements.t) (zipped : string list) =
    let relative_constraints : relative_constraint Queue.t = Queue.create () in
    let non_fixed_nodes =
      elements.nodes
      |> Array.fold_left
           (fun acc Node.{ id; node_type; _ } ->
             match node_type with
             | Edge -> String_set.add id acc
             | Tensor -> acc)
           String_set.empty
    in
    let appears_once_nodes =
      appears_once
        (elements.edges |> Array.to_list
        |> List.map (fun Edge.{ source; target; _ } -> [| source; target |])
        |> Array.concat)
    in
    let is_dangling node_id =
      String_set.mem node_id non_fixed_nodes
      && String_set.mem node_id appears_once_nodes
    in
    let dangling_nodes =
      elements.edges
      |> Array.fold_left
           (fun acc Edge.{ source; target; _ } ->
             let acc = if is_dangling source then source :: acc else acc in
             if is_dangling target then target :: acc else acc)
           []
      |> String_set.of_list
    in
    Array.iter
      (fun Edge.{ source; target; _ } ->
        if
          not
            (String_set.mem source dangling_nodes
            || String_set.mem target dangling_nodes)
        then ()
        else
          (* normalize *)
          let fixed_side, unfixed_side =
            if source = "left" || source = "right" then (source, target)
            else (target, source)
          in
          let n_prev_connections =
            Queue.fold
              (fun acc relative ->
                match relative with
                | Horizontal { left = x; right = y }
                | Vertical { top = x; bottom = y } ->
                    if x = source || y = fixed_side then acc + 1 else acc)
              0 relative_constraints
          in
          match (fixed_side, n_prev_connections) with
          | _, 0 ->
              let left, right =
                if fixed_side = "left" then (unfixed_side, fixed_side)
                else (fixed_side, unfixed_side)
              in
              Queue.add (Horizontal { left; right }) relative_constraints
          | _, n when n = 1 || n = 2 ->
              let top, bottom =
                if n = 1 then (unfixed_side, fixed_side)
                else (fixed_side, unfixed_side)
              in
              Queue.add (Vertical { top; bottom }) relative_constraints
          | _, _ -> ())
      elements.edges;
    List.iter
      (fun bottom ->
        Queue.add (Vertical { top = "left"; bottom }) relative_constraints;
        Queue.add (Vertical { top = "right"; bottom }) relative_constraints)
      zipped;
    relative_constraints |> Queue.to_seq |> Array.of_seq

  let opts ~container ~elements ~fixed ~zipped () =
    Brr.El.set_inline_style (Jstr.v "height") (Jstr.v "400px") container;
    Brr.El.set_inline_style (Jstr.v "width") (Jstr.v "800px") container;
    Brr.Console.log [ "elements"; Elements.to_jv elements ];

    Jv.obj
      [|
        ("container", Brr.El.to_jv container);
        ("elements", Elements.to_jv elements);
        ("zoomingEnabled", Jv.of_bool false);
        ("panningEnabled", Jv.of_bool false);
        ("boxSelectionEnabled", Jv.of_bool false);
        ( "layout",
          Jv.obj
            [|
              ("name", Jv.of_string "fcose");
              ("animate", Jv.of_bool false);
              ( "fixedNodeConstraint",
                Jv.of_array
                  (fun (name, x, y) ->
                    Jv.obj
                      [|
                        ("nodeId", Jv.of_string name);
                        ( "position",
                          Jv.obj [| ("x", Jv.of_int x); ("y", Jv.of_int y) |] );
                      |])
                  fixed );
              ( "relativePlacementConstraint",
                get_relative_constraints elements zipped
                |> Jv.of_array (function
                     | Vertical { top; bottom } ->
                         Jv.obj
                           [|
                             ("top", Jv.of_string top);
                             ("bottom", Jv.of_string bottom);
                           |]
                     | Horizontal { left; right } ->
                         Jv.obj
                           [|
                             ("left", Jv.of_string left);
                             ("right", Jv.of_string right);
                           |]) );
            |] );
        ( "style",
          Jv.of_jv_array
            [|
              Jv.obj
                [|
                  ("selector", Jv.of_string "node");
                  ("style", Jv.obj [| ("label", Jv.of_string "data(label)") |]);
                |];
              Jv.obj
                [|
                  ("selector", Jv.of_string "edge");
                  ( "style",
                    Jv.obj
                      [|
                        ("label", Jv.of_string "data(label)");
                        ("line-color", Jv.of_string "data(color)");
                        ("color", Jv.of_string "data(color)");
                      |] );
                |];
              Jv.obj
                [|
                  ("selector", Jv.of_string "node[type='tensor']");
                  ( "style",
                    Jv.obj
                      [|
                        ("shape", Jv.of_string "ellipse");
                        ("background-color", Jv.of_string "white");
                        ("border-width", Jv.of_string "4px");
                      |] );
                |];
              Jv.obj
                [|
                  ("selector", Jv.of_string "node[type='edge']");
                  ( "style",
                    Jv.obj
                      [|
                        ("shape", Jv.of_string "triangle");
                        ("background-opacity", Jv.of_string "0");
                      |] );
                |];
            |] );
      |]

  let create ~opts () = Jv.call Jv.global "cytoscape" [| opts |]
end
