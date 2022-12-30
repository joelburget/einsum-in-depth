
(* This file was auto-generated based on "Einsum_parser.new.new.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 25 ->
        "Unexpected token after \",\".\n"
    | 14 ->
        "Unexpected token after a group.\n"
    | 16 ->
        "Unexpected token after an atom.\n"
    | 21 ->
        "Unexpected token after an atom.\n"
    | 20 ->
        "Unexpected token after \"->\".\n"
    | 24 ->
        "Unexpected token.\n"
    | 18 ->
        "Unexpected token.\n"
    | 11 ->
        "Unexpected token.\n"
    | 9 ->
        "Unexpected token after an atom.\n"
    | 3 ->
        "After a variable name, expected a list of variable names or \")\".\n"
    | 2 ->
        "After \"(\", expected a list of variable names.\n"
    | 0 ->
        "Unexpected token.\n"
    | _ ->
        raise Not_found
