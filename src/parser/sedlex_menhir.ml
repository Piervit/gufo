(* Lexer / parser integration from
 * https://github.com/unhammer/ocaml_cg_streamparse/blob/match-singlechar-example/sedlex_menhir.ml
 * *)


(** The state of the parser, a stream and a position. *)
type lexbuf = {
  stream : Sedlexing.lexbuf ;
  mutable pos : Lexing.position ;
}

(** Initialize with the null position. *)
let create_lexbuf ?(file="") stream =
  let pos = {Lexing.
    pos_fname = file;
    pos_lnum = 1; (* Start lines at 1, not 0 *)
    pos_bol = 0;
    pos_cnum = 0;
  }
  in { pos ; stream }

(** Register a new line in the lexer's position. *)
let new_line ?(n=0) lexbuf =
  let open Lexing in
  let lcp = lexbuf.pos in
  lexbuf.pos <-
    {lcp with
       pos_lnum = lcp.pos_lnum + 1;
       pos_bol = lcp.pos_cnum;
    }

(** Update the position with the stream. *)
let update lexbuf =
  let new_pos = Sedlexing.lexeme_end lexbuf.stream in
  let p = lexbuf.pos in
  lexbuf.pos <- {p with Lexing.pos_cnum = new_pos }

(** The last matched word. *)
let lexeme { stream } = Sedlexing.Utf8.lexeme stream


(** [ParseError (file, line, col, token, reason)] *)
exception ParseError of (string * int * int * string * string option)


let string_of_ParseError (file, line, cnum, tok, reason) =
  let file_to_string file =
    if file = "" then ""
    else " on file " ^ file
  in
  match reason with
    | None -> 
  Printf.sprintf
        "Parse error%s line %i, column %i, token %s"
        (file_to_string file)
        line cnum tok 
    | Some reason -> 
        Printf.sprintf
        "Parse error%s line %i, column %i, token %s: %s"
        (file_to_string file)
        line cnum tok reason

let raise_ParseError lexbuf =
  let { pos } = lexbuf in
  let tok = lexeme lexbuf in
  let open Lexing in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  raise @@ ParseError (pos.pos_fname, line, col, tok, None)

let raise_ParseErrorWithMsg lexbuf msg =
  let { pos } = lexbuf in
  let tok = lexeme lexbuf in
  let open Lexing in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  raise @@ ParseError (pos.pos_fname, line, col, tok, Some msg)



let sedlex_with_menhir lexer parseur lexbuf =
  let thelexer () =
    let ante_position = lexbuf.pos in
    let token = lexer lexbuf in
    let post_position = lexbuf.pos
    in (token, ante_position, post_position) in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised parseur
  in
  try
    parser thelexer 
  with
    | Gufo_parser.Error
    | Sedlexing.MalFormed
    | Sedlexing.InvalidCodepoint _
-> raise_ParseError lexbuf



