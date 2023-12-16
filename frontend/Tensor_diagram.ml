open Tensor_playground.Einops

let list_subtraction l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

let line ?(color' = "black") (x1', y1') (x2', y2') =
  let open Brr_svg in
  El.line
    ~at:
      At.
        [
          x1 (Jstr.v (string_of_float x1'));
          y1 (Jstr.v (string_of_float y1'));
          x2 (Jstr.v (string_of_float x2'));
          y2 (Jstr.v (string_of_float y2'));
          stroke (Jstr.v color');
          strokewidth (Jstr.v "0.005");
        ]
    []

let ellipse ?(fill' = "blue") ?(stroke' = "black") (x', y') (rx', ry') =
  let open Brr_svg in
  El.ellipse
    ~at:
      At.
        [
          cx (Jstr.v (string_of_float x'));
          cy (Jstr.v (string_of_float y'));
          rx (Jstr.v (string_of_float rx'));
          ry (Jstr.v (string_of_float ry'));
          fill (Jstr.v fill');
          strokewidth (Jstr.v "0.01");
          stroke (Jstr.v stroke');
        ]
    []

let text (x', y') str =
  Brr_svg.(
    El.(
      v (Jstr.v "text")
        ~at:
          At.
            [
              x (Jstr.v (string_of_float x'));
              y (Jstr.v (string_of_float y'));
              style (Jstr.v "font: 0.05px serif");
            ]
        [ txt' str ]))

let draw_radial_lines (x, y) names is_rhs =
  let direction = if is_rhs then 0. else Float.pi in
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
  [ ellipse (x', y') (rx', ry'); text (x' +. rx', y' -. ry') label ]

let draw_contraction (contracted_tensors, Single_contraction.{ contracted; _ })
    =
  let left_uncontracted : string list =
    list_subtraction List.(hd contracted_tensors) contracted
  in
  let right_uncontracted : string list =
    list_subtraction List.(hd (tl contracted_tensors)) contracted
  in
  let left_lines = draw_radial_lines (0.3, 0.5) left_uncontracted false in
  let right_lines = draw_radial_lines (0.7, 0.5) right_uncontracted true in
  let n_contracted = List.length contracted in
  let ry, start_y = if n_contracted <= 1 then (0.05, 0.5) else (0.3, 0.2) in
  let contraction_lines =
    List.mapi
      (fun i name ->
        let y =
          if n_contracted <= 1 then 0.5
          else
            start_y
            +. (ry *. 2.0 *. float_of_int i /. float_of_int (n_contracted - 1))
        in
        [ line (0.3, y) (0.7, y); text (0.5, y) name ])
      contracted
  in
  let elements =
    List.flatten (left_lines @ right_lines @ contraction_lines)
    @ tensor (0.3, 0.5) (0.05, ry) "X"
    @ tensor (0.7, 0.5) (0.05, ry) "Y"
  in
  let open Brr_svg in
  El.svg
    ~at:
      At.
        [
          width (Jstr.v "400");
          height (Jstr.v "400");
          viewbox (Jstr.v "0 0 1 1");
        ]
    elements
