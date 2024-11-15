module Node : sig
  type t = {
    id : string;
  }

  val to_jv : t -> Jv.t
end = struct 
  type t = {
    id : string;
  }
  
  let to_jv {id} =
    Jv.obj [|"data", 
      Jv.obj [|
        "id", Jv.of_string id; 
        "label", Jv.of_string id
      |]
    |]
end

module Edge : sig
  type t = {
    id : string;
    source : string;
    target : string;
  }

  val to_jv : t -> Jv.t
end = struct 
  type t = {
    id : string;
    source : string;
    target : string;
  }

  let to_jv {id; source; target} =
    Jv.obj [|"data", 
      Jv.obj [|
        "id", Jv.of_string id; 
        "source", Jv.of_string source; 
        "target", Jv.of_string target
      |]
    |]
end

module Elements : sig
  type t = {
    nodes : Node.t array;
    edges : Edge.t array
  }

  val to_jv : t -> Jv.t
end = struct
  type t = {
    nodes : Node.t array;
    edges : Edge.t array
  }

  let to_jv {nodes; edges} =
    Jv.obj [|
      "nodes", Jv.of_array Node.to_jv nodes;
      "edges", Jv.of_array Edge.to_jv edges;
    |]
end

module Cytoscape : sig
  type t
  type opts
  val opts : container:Brr.El.t -> ?elements:Elements.t -> unit -> opts
  val create : opts:opts -> unit -> t
end = struct
  type t = Jv.t
  type opts = Jv.t
  let opts ~container ?(elements = Elements.{nodes=[||]; edges=[||]}) () =
    Brr.El.set_inline_style (Jstr.v "height") (Jstr.v "300px") container;
    Brr.El.set_inline_style (Jstr.v "width") (Jstr.v "300px") container;
    Jv.obj [|
      ("container", Brr.El.to_jv container);
      ("elements", Elements.to_jv elements);
      ("layout", Jv.obj [|
        "name", Jv.of_string "grid";
        (* Cytoscape crashes if you don't provide the boundingBox *)
        "boundingBox", Jv.obj [|
          "x1", Jv.of_float 0.;
          "y1", Jv.of_float 0.;
          "w", Jv.of_float 300.;
          "h", Jv.of_float 300.; 
        |]
      |]);
      ("style", Jv.of_jv_array [|
        Jv.obj [|
          "selector", Jv.of_string "node";
          "style", Jv.obj [|
            "label", Jv.of_string "data(label)";
          |]
        |]
      |])
    |]

  let create ~opts () = Jv.call Jv.global "cytoscape" [|opts|]
end
