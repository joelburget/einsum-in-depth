type attributes = {
  fill_classes : string;
  text_classes : string;
  bg_classes : string;
  length : float;
}

(* colorhunt.co *)
let edge_attributes =
  [|
    {
      fill_classes = "fill-indigo-800 dark:fill-rose-200";
      text_classes = "text-indigo-800 dark:text-rose-200";
      bg_classes = "bg-indigo-800 dark:bg-rose-200";
      length = 5.;
    };
    {
      fill_classes = "fill-pink-800 dark:fill-yellow-100";
      text_classes = "text-pink-800 dark:text-yellow-100";
      bg_classes = "bg-pink-800 dark:bg-yellow-100";
      length = 3.;
    };
    {
      fill_classes = "fill-fuchsia-800 dark:fill-emerald-200";
      text_classes = "text-fuchsia-800 dark:text-emerald-200";
      bg_classes = "bg-fuchsia-800 dark:bg-emerald-200";
      length = 7.;
    };
    {
      fill_classes = "fill-purple-900 dark:fill-cyan-200";
      text_classes = "text-purple-900 dark:text-cyan-200";
      bg_classes = "bg-purple-900 dark:bg-cyan-200";
      length = 9.;
    };
    {
      fill_classes = "fill-orange-800 dark:fill-stone-100";
      text_classes = "text-orange-800 dark:text-stone-100";
      bg_classes = "bg-orange-800 dark:bg-stone-100";
      length = 11.;
    };
    {
      fill_classes = "fill-red-700 dark:fill-pink-100";
      text_classes = "text-red-700 dark:text-pink-100";
      bg_classes = "bg-red-700 dark:bg-pink-100";
      length = 2.;
    };
    {
      fill_classes = "fill-teal-800 dark:fill-orange-200";
      text_classes = "text-teal-800 dark:text-orange-200";
      bg_classes = "bg-teal-800 dark:bg-orange-200";
      length = 2.;
    };
    {
      fill_classes = "fill-blue-900 dark:fill-blue-100";
      text_classes = "text-blue-900 dark:text-blue-100";
      bg_classes = "bg-blue-900 dark:bg-blue-100";
      length = 2.;
    };
    {
      fill_classes = "fill-cyan-800 dark:fill-slate-300";
      text_classes = "text-cyan-800 dark:text-slate-300";
      bg_classes = "bg-cyan-800 dark:bg-slate-300";
      length = 2.;
    };
    {
      fill_classes = "fill-emerald-800 dark:fill-lime-100";
      text_classes = "text-emerald-800 dark:text-lime-100";
      bg_classes = "bg-emerald-800 dark:bg-lime-100";
      length = 2.;
    };
  |]

let get_edge_attributes i =
  if i < Array.length edge_attributes then edge_attributes.(i)
  else
    {
      fill_classes = "fill-[#000] dark:fill-[#fff]";
      text_classes = "text-[#000] dark:text-[#fff]";
      bg_classes = "bg-[#000] dark:bg-[#fff]";
      length = 4.;
    }

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
