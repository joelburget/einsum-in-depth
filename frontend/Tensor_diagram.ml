module Rel_pos = struct
  type t = string

  let start = "start"
  let right = "right"
  let down = "down"
end

module Pos = struct
  type t = string

  let left = "left"
  let right = "right"
  let up = "up"
  let down = "down"
end

module Shape = struct
  type t = string

  let circle = "circle"
  let dot = "dot"
  let asterisk = "asterisk"
  let square = "square"
  let triangle_up = "triangleUp"
  let triangle_down = "triangleDown"
  let triangle_left = "triangleLeft"
  let triangle_right = "triangleRight"
  let rectangle = "rectangle"
end

module Tensor_opts : sig
  type t = Jv.t

  val make :
    ?shape:Shape.t ->
    ?show_label:bool ->
    ?label_pos:Pos.t ->
    ?color:string ->
    ?size:int ->
    unit ->
    t
end = struct
  type t = Jv.t

  let make ?shape ?show_label ?label_pos ?color ?size () =
    let opts = Jv.obj [||] in
    Jv.set_if_some opts "shape" (Option.map Jv.of_string shape);
    Jv.set_if_some opts "showLabel" (Option.map Jv.of_bool show_label);
    Jv.set_if_some opts "labelPos" (Option.map Jv.of_string label_pos);
    Jv.set_if_some opts "color" (Option.map Jv.of_string color);
    Jv.set_if_some opts "size" (Option.map Jv.of_int size);
    opts
end

module Tensor_diagram : sig
  type t

  include Jv.CONV with type t := t

  val new_diagram : unit -> t

  val add_tensor :
    name:string ->
    position:Rel_pos.t ->
    ?left:string list ->
    ?right:string list ->
    ?up:string list ->
    ?down:string list ->
    ?opts:Tensor_opts.t ->
    t ->
    t

  val add_contraction : int -> int -> string -> ?pos:Pos.t -> t -> t
  val draw : Brr.El.t -> t -> unit
end = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  let new_diagram () = Jv.call (Jv.get Jv.global "TensorDiagram") "new" [||]

  let add_tensor ~name ~position ?left ?right ?up ?down
      ?(opts = Tensor_opts.make ()) t =
    let args =
      [|
        Jv.of_string name;
        Jv.of_string position;
        Jv.of_list Jv.of_string (Option.value ~default:[] left);
        Jv.of_list Jv.of_string (Option.value ~default:[] right);
        Jv.of_list Jv.of_string (Option.value ~default:[] up);
        Jv.of_list Jv.of_string (Option.value ~default:[] down);
        opts;
      |]
    in
    Jv.call t "addTensor" args

  let add_contraction i j name ?(pos = Pos.up) t =
    let args =
      [| Jv.of_int i; Jv.of_int j; Jv.of_string name; Jv.of_string pos |]
    in
    Jv.call t "addContraction" args

  let draw elem t =
    let (_ : Jv.t) = Jv.call t "draw" [| Brr.El.to_jv elem |] in
    ()
end

module Drawing : sig
  open Tensor_playground.Einops

  val draw_contraction :
    string list list * Single_contraction.t -> Tensor_diagram.t

  val draw_rewrite : Rewrite.t -> Tensor_diagram.t list
end = struct
  open Tensor_playground.Einops

  let list_subtraction l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

  let draw_contraction (contracted_tensors, Single_contraction.{ contracted; _ })
      =
    (* TODO: handle contracted_tensors of size > 2, and with more than 4 indices *)
    let drawing =
      Tensor_diagram.new_diagram ()
      |> Tensor_diagram.add_tensor ~name:"X" ~position:"start" ~right:contracted
           ~up:(list_subtraction List.(hd contracted_tensors) contracted)
      |> Tensor_diagram.add_tensor ~name:"Y" ~position:"right" ~left:contracted
           ~up:(list_subtraction List.(hd (tl contracted_tensors)) contracted)
    in
    List.fold_left
      (fun drawing dim_name ->
        Tensor_diagram.add_contraction 0 1 dim_name drawing)
      drawing contracted

  let draw_rewrite rewrite =
    rewrite |> Explain.get_contractions |> List.map draw_contraction
end
