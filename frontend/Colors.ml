type base_attributes = {
  light_bg_color : string;
  dark_bg_color : string;
  length : float;
}

type attributes = { color : string; length : float }

let prefers_dark () = Brr.Window.prefers_dark_color_scheme Brr.G.window

(* colorhunt.co *)
let base_edge_attributes =
  [|
    { light_bg_color = "#0A3981"; dark_bg_color = "#F6D6D6"; length = 5. };
    { light_bg_color = "#AE445A"; dark_bg_color = "#F6F7C4"; length = 3. };
    { light_bg_color = "#441752"; dark_bg_color = "#A1EEBD"; length = 7. };
    { light_bg_color = "#4B4376"; dark_bg_color = "#7BD3EA"; length = 9. };
    { light_bg_color = "#FA812F"; dark_bg_color = "#F7F9F2"; length = 11. };
    { light_bg_color = "#FA4032"; dark_bg_color = "#E8C5E5"; length = 2. };
    { light_bg_color = "#0B192C"; dark_bg_color = "#FFCFB3"; length = 2. };
    { light_bg_color = "#295F98"; dark_bg_color = "#C5D3E8"; length = 2. };
    { light_bg_color = "#2A3335"; dark_bg_color = "#A6AEBF"; length = 2. };
    { light_bg_color = "#006A67"; dark_bg_color = "#E0E5B6"; length = 2. };
  |]

let edge_attributes () : attributes array =
  let f { dark_bg_color; light_bg_color; length } =
    match prefers_dark () with
    | true -> { color = dark_bg_color; length }
    | false -> { color = light_bg_color; length }
  in
  Array.map f base_edge_attributes

let get_edge_attributes i =
  let edge_attributes = edge_attributes () in
  if i < Array.length edge_attributes then edge_attributes.(i)
  else { color = (if prefers_dark () then "#000" else "#fff"); length = 4. }

type edge_attributes = (string, attributes) Hashtbl.t

let assign_edge_attributes tensors =
  let edge_attributes = Hashtbl.create 10 in
  let i = ref 0 in
  List.iter
    (fun tensor ->
      List.iter
        (fun edge ->
          if not (Hashtbl.mem edge_attributes edge) then (
            let attrs = get_edge_attributes !i in
            i := !i + 1;
            Hashtbl.add edge_attributes edge attrs))
        tensor)
    tensors;
  edge_attributes
