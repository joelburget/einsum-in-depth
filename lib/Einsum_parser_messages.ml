
(* This file was auto-generated based on "Einsum_parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 17 ->
        "Unexpected token.\n"
    | 15 ->
        "Unexpected token.\n"
    | 14 ->
        "Expected a list of variable names after \"->\".\n"
    | 13 ->
        "Unexpected token.\n"
    | 11 ->
        "Unexpected token.\n"
    | 9 ->
        "Unexpected token.\n"
    | 3 ->
        "After a variable name, expected a list of variable names or \")\".\n"
    | 2 ->
        "After \"(\", expected a list of variable names.\n"
    | 0 ->
        "Unexpected token.\n"
    | _ ->
        raise Not_found
