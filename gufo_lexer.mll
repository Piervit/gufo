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

{
open Lexing
open Gufo_lexing
open Gufo_parser


let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

(* basics *)
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let comment = '#' [^ '\r' '\n' ]*

(* types *)

let int = '-'? ['0'-'9'] ['0'-'9']*

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?



let varname = '$'['a'-'z' ] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let varfield= '$'['a'-'z' ] ['a'-'z' 'A'-'Z' '0'-'9' '_']*'.'['a'-'z' 'A'-'Z' '_']['.''a'-'z' 'A'-'Z' '0'-'9' '_']*
let modulVar= '$'['A'-'Z' ] ['a'-'z' 'A'-'Z' '0'-'9' '_']* '.' ['a'-'z']['a'-'z''.''A'-'Z' '0'-'9' '_']*
let modul = '$'['A'-'Z' ] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let freetype= '\''['a'-'z' 'A'-'Z' ]

let arg = '-'*['-' 'a'-'z' 'A'-'Z']* 
let word = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']* 
let file = ['a'-'z' 'A'-'Z' '0'-'9' '_' '.' '-' '/' ':' '*' '.'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '.' '/' '-' '@' '&' '$' ':' '.' '*']*
(* let file = ['a'-'z' 'A'-'Z' '0'-'9' '_' '.' '-' '/'] [^ ' ']* *)

rule read =
  parse
  (* generic *)
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | comment  { next_line lexbuf; read lexbuf }
  | "struct" { STRUCT }
  | "let"    { LET }
  | "fun"    { FUN }
  | "_"      { JOKER }
  | ":"      { COLON }
  | "="      { AFFECTATION }
  | "True"   { TRUE }
  | "False"  { FALSE }
  (* pattern matching *)
  | "match"      { MATCH }
  | "%"      { PERCENT }
  | "->"      { ARROW }
  | "with"   { WITH }
  | "without"   { WITHOUT }
  | "*"      { STAR }
  | "**"     { DBL_STAR }
  | "~"      { TILDE }
  | ">"      { CLOSING_CHEVRON }
  | ">-"      { MINUS_CLOSING_CHEVRON }
  | ">>"      { DOUBLE_CLOSING_CHEVRON }
  | "2>"      { WRITE_ERROR_TO }
  | "2>>"      { WRITE_ERROR_NEXT_TO }
  | "2>&1"      { WRITE_ERROR_TO_STD }
  | ">&"      { WRITE_ALL_TO }
  | "&"      { SIMPLE_AND }
  | "&&"      { AND }
  | "||"      { OR }
  | "|"      { PIPE }
  | "<"      { OPENING_CHEVRON }
  | "-<"      { MINUS_OPENING_CHEVRON }
  | "exist"      { FILE_EXIST }
  | "+"     		{ PLUS }
  | "--"     		{ DOUBLE_MINUS }
  | "-"     		{ MINUS }
  | "/"     		{ DIVISION }
  | "mod"   		{ MODULO }
  | "=="    		{ EQUALITY }
  | "!="    		{ INEQUALITY }
  | "["     		{ OPEN_SQRBRACKET }
  | ".["     		{ OPEN_SQRIDXBRACKET }
  | "]"     		{ CLOSE_SQRBRACKET }
  | "("     		{ OPEN_BRACKET }
  | ")"     		{ CLOSE_BRACKET }
  | "list_length"     	{ LIST_LENGTH }
  | "list_append"     	{ LIST_APPEND }
  | "list_prepend"     	{ LIST_PREPEND }
  | "list_rm_last"     	{ LIST_RM_LAST }
  | "list_rm_first"    	{ LIST_RM_FIRST }
  | "int" 		{ INTTYPE }
  | "float" 		{ FLOATTYPE }
  | "string" 		{ STRINGTYPE }
  | "bool" 		{ BOOLTYPE}
  | "cmd" 		{ CMDTYPE}
  | "list" 		{ LISTTYPE }
  | "set" 		{ SETTYPE }
  | "map" 		{ MAPTYPE }
  | "option" 		{ OPTIONTYPE }
  | freetype            { FREETYPE (Lexing.lexeme lexbuf)}
  | "extends"           { EXTENDS }
  | "in"    		{ IN }
  | "if"    		{ IF }
  | "then"    		{ THEN }
  | "else"    		{ ELSE }
  | '.'                 { DOT }
  | int                 { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float               { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "None"              { NONE}
  | "Some"              { SOME}
  | "**start**"              { START }
  | "**end**"              { END }
  | ";"                 { SEMICOLON }
  | ";;"                { DOUBLE_SEMICOLON }
  | '{'                 { OPEN_BRACE }
  | '}'                 { CLOSE_BRACE }
  | ','                 { COMMA }
  | '"'                 { read_string (Buffer.create 17) lexbuf }
  | varname             { VARNAME (Lexing.lexeme lexbuf) }
  | varfield            { VARFIELD (Lexing.lexeme lexbuf) }
  | modulVar            { MODULVAR (Lexing.lexeme lexbuf) }
  | modul               { MODUL (Lexing.lexeme lexbuf) }
  | word                { WORD (Lexing.lexeme lexbuf) }
  | arg           { ARG (Lexing.lexeme lexbuf)} 
  | file                { FILE (Lexing.lexeme lexbuf)} 
(*   | cmdarg              { CMDARG (Lexing.lexeme lexbuf)}  *)
  | _                   { raise (GufoParsed.SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }


and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (GufoParsed.SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
| eof { raise (GufoParsed.SyntaxError ("String is not terminated")) }
