type attributes = { color : string; length : float }

(* colorhunt.co *)
let edge_attributes =
  [|
    { color = "#432E54"; length = 5. };
    { color = "#8EA3A6"; length = 3. };
    { color = "#AE445A"; length = 7. };
    { color = "#4B4376"; length = 9. };
    { color = "#E8BCB9"; length = 11. };
    { color = "#E6E9AF"; length = 2. };
    { color = "#FFCFEF"; length = 2. };
    { color = "#0A97B0"; length = 2. };
    { color = "#2A3335"; length = 2. };
    { color = "#006A67"; length = 2. };
  |]

let get_edge_attributes i =
  if i < Array.length edge_attributes then edge_attributes.(i)
  else { color = "#000000"; length = 4. }

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
