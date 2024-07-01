(* This file was auto-generated based on "Type_parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message s =
  match s with
  | 17 -> "Expected a bracketed or unbracketed list\n"
  | 12 -> "Expected a bracketed or unbracketed list\n"
  | 11 -> "Expected a bracketed or unbracketed list\n"
  | 8 -> "Expected list element after \",\"\n"
  | 7 -> "Expected \",\" or \"]\" after list element\n"
  | 3 -> "Expected \"]\" after list elements\n"
  | 1 -> "Expected a list after \"[\"\n"
  | 0 -> "Unexpected \"]\"\n"
  | _ -> raise Not_found
