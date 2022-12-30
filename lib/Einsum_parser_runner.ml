module I = Einsum_parser.MenhirInterpreter
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil

type 'a parser = (Lexing.lexbuf -> Einsum_parser.token) -> Lexing.lexbuf -> 'a

let env = function I.HandlingError env -> env | _ -> assert false

let state checkpoint : int =
  match I.top (env checkpoint) with
  | Some (I.Element (s, _, _, _)) -> I.number s
  | None ->
      (* Hmm... The parser is in its initial state. The incremental API
         currently lacks a way of finding out the number of the initial
         state. It is usually 0, so we return 0. This is unsatisfactory
         and should be fixed in the future. *)
      0

let show text positions =
  E.extract text positions |> E.sanitize |> E.compress
  |> E.shorten 20 (* max width 43 *)

let get text checkpoint i =
  match I.get i (env checkpoint) with
  | Some (I.Element (_, _, pos1, pos2)) -> show text (pos1, pos2)
  | None ->
      (* The index is out of range. This should not happen if [$i]
         keywords are correctly inside the syntax error message
         database. The integer [i] should always be a valid offset
         into the known suffix of the stack. *)
      "???"

let succeed v = Ok v

let fail text buffer (checkpoint : _ I.checkpoint) =
  (* Indicate where in the input file the error occurred. *)
  let location = L.range (E.last buffer) in
  (* Show the tokens just before and just after the error. *)
  let indication = Fmt.str "Syntax error %s.\n" (E.show (show text) buffer) in
  (* Fetch an error message from the database. *)
  let message = Einsum_parser_messages.message (state checkpoint) in
  (* Expand away the $i keywords that might appear in the message. *)
  let message = E.expand (get text checkpoint) message in
  (* Show these three components. *)
  Error (location, indication, message)

let parse parser str =
  let lexbuf = Lexing.from_string str in
  let supplier = I.lexer_lexbuf_to_supplier Einsum_lexer.token lexbuf in
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint = parser lexbuf.lex_curr_p in
  I.loop_handle succeed (fail str buffer) supplier checkpoint
