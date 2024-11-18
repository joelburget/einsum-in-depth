(* TODO: do all text drawing last *)
open Tensor_playground.Einops

let list_subtraction l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1
(*
let float f = Jstr.v (string_of_float f)

let curve ?(color' = "hsl(203.42deg 92.41% 69.02%)") pt1 pt2 =
  let open Brr_svg in
  let x1', y1' = pt1 in
  let x2', y2' = pt2 in
  El.path
    ~at:
      At.
        [
          d
            (Jstr.v
               (Fmt.str
                  "M %f,%f Q %f,%f %f,%f"
                  x1' y1'
                  x2' y1'
                  x2' y2'));
          fill (Jstr.v "none");
          stroke (Jstr.v color');
          strokewidth (Jstr.v "0.01");
          filter (Jstr.v "url(#noise)");
        ]
    []

let line ?(color' = "hsl(203.42deg 92.41% 69.02%)") (x1', y1') (x2', y2') =
  let open Brr_svg in
  El.line
    ~at:
      At.
        [
          x1 (float x1');
          y1 (float y1');
          x2 (float x2');
          y2 (float y2');
          stroke (Jstr.v color');
          strokewidth (Jstr.v "0.01");
          filter (Jstr.v "url(#noise)");
        ]
    []

let ellipse' ?(id = None) ?(fill' = "hsl(337, 92%, 69%)") (x', y') (rx', ry') =
  let open Brr_svg in
  let at =
    At.
      [
        cx (float x');
        cy (float y');
        rx (float rx');
        ry (float ry');
        fill (Jstr.v fill');
        filter (Jstr.v "url(#noise)");
      ]
  in
  let at = match id with None -> at | Some id -> At.id (Jstr.v id) :: at in
  El.ellipse ~at []

let text (x', y') str =
  Brr_svg.(
    El.(
      v (Jstr.v "text")
        ~at:
          At.[ x (float x'); y (float y'); style (Jstr.v "font: 0.05px serif") ]
        [ txt' str ]))

let double_curve str pt1 pt2 x =
  let midpoint = x, 0.9 in
  [curve pt1 midpoint; curve pt2 midpoint; text midpoint str]

let draw_radial_lines (x, y) names is_rhs =
  let direction = if is_rhs then Float.pi *. -0.5 else Float.pi *. 0.5 in
  let radius = 0.2 in
  let angle_increment = Float.pi /. float_of_int (List.length names) in
  List.mapi
    (fun i name ->
      let angle = direction +. (angle_increment *. float_of_int i) in
      let x_dir = radius *. cos angle in
      let x_end = x +. x_dir in
      let y_dir = radius *. sin angle in
      let y_end = y +. y_dir in
      [
        line (x, y) (x_end, y_end);
        text
          ((x_end +. if is_rhs then 0.01 else -0.01), y_end +. (y_dir *. 0.1))
          name;
      ])
    names

let tensor (x', y') (rx', ry') label =
  [ ellipse' (x', y') (rx', ry'); text (x' +. rx', y' -. ry') label ]

let draw_contraction ((l_tensor, r_tensor), Binary_contraction.{ contracted; zipped; _ })
    =
    let left_x = 0.3 in
    let right_x = 0.7 in
    let y = 0.5 in
    let left_center = (left_x, y) in
    let right_center = (right_x, y) in
  let left_uncontracted : string list = list_subtraction l_tensor (contracted @ zipped) in
  let right_uncontracted : string list = list_subtraction r_tensor (contracted @ zipped) in
  let left_lines = draw_radial_lines left_center left_uncontracted false in
  let right_lines = draw_radial_lines left_center right_uncontracted true in
  let n_contracted = List.length contracted in
  let n_zipped = List.length zipped in
  let ry, start_y = if n_contracted <= 1 then (0.05, 0.5) else (0.3, 0.2) in
  let contraction_lines =
    List.mapi
      (fun i name ->
        let y =
          if n_contracted <= 1 then 0.5
          else
            start_y +. 0.05
            +. (ry -. 0.05) *. 2.0 *. float_of_int i
               /. float_of_int (n_contracted - 1)
        in
        let middle_x = left_x +. (right_x -. left_x) /. 2. in
        [ line (left_x, y) (right_x, y); text (middle_x, y) name ])
      contracted
  in
  let zipped_lines =
    List.mapi
      (fun i name -> 
        (* 0/0 -> left_x +. diff *. 0.5

           0/1 -> left_x +. diff *. 0.33
           1/1 -> left_x +. diff *. 0.66
         *)
        let x = left_x +. 
          (right_x -. left_x) *. float_of_int (i + 1) /. float_of_int (n_zipped + 1) 
        in
        double_curve name left_center right_center x)
      zipped
  in
  let elements =
    List.flatten (left_lines @ right_lines @ contraction_lines @ zipped_lines)
    @ tensor left_center (0.05, ry) "X"
    @ tensor right_center (0.05, ry) "Y"
  in
  let open Brr_svg in
  El.svg
    ~at:
      At.
        [
          width (Jstr.v "500");
          height (Jstr.v "500");
          viewbox (Jstr.v "0 0 1.3 1.3");
        ]
    elements
*)

let draw_contraction
    ((l_tensor, r_tensor), Binary_contraction.{ contracted; zipped; _ }) =
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
