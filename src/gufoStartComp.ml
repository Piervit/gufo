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

open Sedlexing
open Sedlex_menhir
open Printf
open GufoParsed
open Gufo_lexer
open Gufo_lexing
open GenUtils


let parse_shell content = 
  debug_info (debug_title1 "Start converting file to a gufo program.");
  try(
    let lexbuf = create_lexbuf (Sedlexing.Utf8.from_stream (Stream.of_string content)) in
    let prog = sedlex_with_menhir Gufo_lexer.read Gufo_parser.shell lexbuf in
  
    match prog with
      | Some _ -> 
        debug_info ("File converted to gufo program.\n");
        prog
      | None ->
        debug_info ("Invalid program (unable to convert to valid program)\n");
        None
  )
  with e ->
    debug_info ("Invalid program (unable to convert to valid program)\n");
    raise e

let parse_file filename =
      try(
        let inx = open_in filename in
        let lexbuf = create_lexbuf ~file:filename (Sedlexing.Utf8.from_channel inx) in
        let prog = sedlex_with_menhir Gufo_lexer.read Gufo_parser.prog lexbuf in
        close_in inx;
        prog
      )
      with _ -> None
