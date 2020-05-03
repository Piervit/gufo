(*
  This file is part of Gufo.

    Gufo is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Gufo is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Gufo. If not, see <http://www.gnu.org/licenses/>. 

    Author: Pierre Vittet
*)
 
  
(** [ParseError (file, line_start, line_end , col_start, col_end , token, reason)] *)
exception ParseError of (string * int * int * int * int * string * string option)

let string_of_ParseError (file, line_start, line_end, col_start, col_end , tok, reason) =
  let file_to_string file =
    if file = "" then ""
    else " on file " ^ file
  in
  
  match reason with
    | None -> 
        Printf.sprintf
        "Parse error%s starting at line %i column %i until line %i column %i , token %s"
        (file_to_string file)
        line_start col_start line_end col_end tok 
    | Some reason -> 
        Printf.sprintf
        "Parse error%s starting at line %i column %i until line %i column %i , token %s: %s"
        (file_to_string file)
        line_start col_start line_end col_end tok reason



let raise_ParseError lexbuf =
  let tok = Sedlexing.Utf8.lexeme lexbuf in
  let open Lexing in
  let (start_pos, end_pos) = Sedlexing.lexing_positions lexbuf in 
  raise @@ ParseError (start_pos.pos_fname, 
                       start_pos.pos_lnum, 
                       end_pos.pos_lnum,
                       start_pos.pos_cnum,
                       end_pos.pos_cnum, 
                       tok, None)


let raise_ParseErrorWithMsg lexbuf msg =
  let tok = Sedlexing.Utf8.lexeme lexbuf in
  let open Lexing in
  let (start_pos, end_pos) = Sedlexing.lexing_positions lexbuf in
  raise @@ ParseError (start_pos.pos_fname, 
                       start_pos.pos_lnum, 
                       end_pos.pos_lnum,
                       start_pos.pos_cnum,
                       end_pos.pos_cnum, 
                       tok, Some msg)



let sedlex_with_menhir lexer parseur lexbuf =
  let thelexer () =
(*     let ante_position = lexbuf.pos in *)
    let token, (pos_start, pos_end) = lexer lexbuf in
    (token, pos_start, pos_end) 
  in
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

