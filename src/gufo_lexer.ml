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

(*This file is the new utf8 compliant lexer (sedlex) 
*)
open Sedlex_menhir
open GufoParsed


let ascii_min_letter = [%sedlex.regexp? ('a' .. 'z' )]
let ascii_maj_letter = [%sedlex.regexp? ('A' .. 'Z' )]
let ascii_letter = [%sedlex.regexp? (ascii_min_letter | ascii_maj_letter )]
let digit = [%sedlex.regexp? '0'..'9']
let ascii_base = [%sedlex.regexp? ascii_letter | digit | '_' ]
let ascii_base_star = [%sedlex.regexp? Star (ascii_base) ]

let ascii_min_then_star = [%sedlex.regexp? ascii_min_letter, ascii_base_star]

let newline = [%sedlex.regexp? ('\r' | '\n' | "\r\n")]

let comment = [%sedlex.regexp? '#', Star( Compl( '\r' | '\n' ))] 

let freetype= [%sedlex.regexp? ('\'', ascii_letter )]

let int_rep = [%sedlex.regexp? Opt '-', Plus digit]
let float_rep = [%sedlex.regexp? Plus (digit), Opt ('.') , Opt(digit) ]

let envvar = [%sedlex.regexp? '$', '$', ascii_min_letter , ascii_base_star]

(*varname: a 'normal variable name, or the 'JOKER' pattern. *)
let varname = [%sedlex.regexp? ('$',  ascii_min_letter , ascii_base_star | '_' )]

let ascii_base_star_with_dot = [%sedlex.regexp? Star (ascii_base | '.') ]
let varfield= [%sedlex.regexp? '$', ascii_min_letter, ascii_base_star , '.', ascii_letter , ascii_base_star_with_dot ]

let modul =  [%sedlex.regexp? '$', ascii_maj_letter, ascii_base_star]
let modulVar= [%sedlex.regexp? modul , '.', ascii_min_then_star ]

let word =  [%sedlex.regexp? (xml_letter | '_'),  Star (xml_letter | '0' .. '9' | '_' | '-') ]
let arg = [%sedlex.regexp? '-', Opt('-'), Star ('-' | xml_letter ) ]
let file = [%sedlex.regexp? (xml_letter | '0' .. '9' | '_' | '.' | '-' | '/' | '*' | '.' | '~'), Star ( xml_letter | '0' .. '9' | '_' | '.' | '/' | '-' | '@' | '&' | '$' | '.' | '*')  ]


let rec read_string inBuf genBuf =
  match%sedlex genBuf with
  | '"' -> Gufo_parser.STRING (Buffer.contents inBuf) 
  | '\\', '/' -> Buffer.add_char inBuf '/'; read_string inBuf genBuf 
  | '\\', '\\' -> Buffer.add_char inBuf '\\'; read_string inBuf genBuf 
  | '\\', 'b'  -> Buffer.add_char inBuf '\b'; read_string inBuf genBuf 
  | '\\', 'f'  -> Buffer.add_char inBuf '\012'; read_string inBuf genBuf 
  | '\\', 'n'  -> Buffer.add_char inBuf '\n'; read_string inBuf genBuf 
  | '\\', 'r'  -> Buffer.add_char inBuf '\r'; read_string inBuf genBuf
  | '\\', 't'  -> Buffer.add_char inBuf '\t'; read_string inBuf genBuf 
  | Sub (any,('"' | '\\') ) ->
    Buffer.add_string inBuf (Sedlexing.Utf8.lexeme genBuf);
    read_string inBuf genBuf
  | eof ->  
      let lex_pos_start, lex_pos_end = (Sedlexing.lexing_positions genBuf) in
      raise (SyntaxError (GufoParsedHelper.with_lexing_pos  lex_pos_start lex_pos_end 
              "String is not terminated" ) )
  | _ -> 
      let lex_pos_start, lex_pos_end = (Sedlexing.lexing_positions genBuf) in
      raise (SyntaxError (GufoParsedHelper.with_lexing_pos lex_pos_start lex_pos_end 
              ("Illegal string character: " ^ Sedlexing.Utf8.lexeme genBuf)))




let try_read_str lexbuf =
  match%sedlex lexbuf with
   | '"' ->
     let start_pos, _end_pos = Sedlexing.lexing_positions lexbuf in
     let token = read_string (Buffer.create 17) lexbuf in
     let _start_pos, end_pos =  Sedlexing.lexing_positions lexbuf in
     Some (token, (start_pos, end_pos))
   | _ -> None


let rec read_ lexbuf = 
  match%sedlex lexbuf with
   | white_space -> read_ lexbuf
   | newline  -> read_ lexbuf 
   | comment  -> read_ lexbuf 
   | "struct" -> Gufo_parser.STRUCT 
   | "let"    -> Gufo_parser.LET  
   | "fun"    -> Gufo_parser.FUN  
(*    | "_"      -> Gufo_parser.JOKER   *)
   | ":"      -> Gufo_parser.COLON  
   | "="      -> Gufo_parser.AFFECTATION  
   | "True"   -> Gufo_parser.TRUE  
   | "False"  -> Gufo_parser.FALSE  
   (* pattern matching *)
   | "->"     -> Gufo_parser.ARROW  
   | "With"   -> Gufo_parser.WITH  
   | "SWith"  -> Gufo_parser.WITH_SET 
   | "MWith"  -> Gufo_parser.WITH_MAP
   | "SWout"   -> Gufo_parser.WITHOUT_SET 
   | "MWout"   -> Gufo_parser.WITHOUT_MAP
   | "*"      -> Gufo_parser.STAR  
   | "*."     -> Gufo_parser.STAR_DOT
   | "~"      -> Gufo_parser.TILDE  
   | ">"      -> Gufo_parser.CLOSING_CHEVRON  
   | ">-"     -> Gufo_parser.MINUS_CLOSING_CHEVRON  
   | ">>"     -> Gufo_parser.DOUBLE_CLOSING_CHEVRON  
   | "2>"     -> Gufo_parser.WRITE_ERROR_TO  
   | "2>>"    -> Gufo_parser.WRITE_ERROR_NEXT_TO  
   | "2>&1"   -> Gufo_parser.WRITE_ERROR_TO_STD  
   | ">&"     -> Gufo_parser.WRITE_ALL_TO  
   | "&"      -> Gufo_parser.SIMPLE_AND  
   | "&&"     -> Gufo_parser.AND  
   | "||"     -> Gufo_parser.OR  
   | "|"      -> Gufo_parser.PIPE  
   | "<"      -> Gufo_parser.OPENING_CHEVRON  
   | "-<"     -> Gufo_parser.MINUS_OPENING_CHEVRON  
   | "+"      -> Gufo_parser.PLUS  
   | "+."      -> Gufo_parser.PLUS_DOT
   | "^"      -> Gufo_parser.PLUS_STR 
   | "--"     -> Gufo_parser.DOUBLE_MINUS  
   | "-"      -> Gufo_parser.MINUS  
   | "-."      -> Gufo_parser.MINUS_DOT
   | "%"      -> Gufo_parser.DIVISION  
   | "%."      -> Gufo_parser.DIVISION_DOT
   | "mod"    -> Gufo_parser.MODULO  
   | "mod."    -> Gufo_parser.MODULO_DOT
   | "=="     -> Gufo_parser.EQUALITY  
   | "!="     -> Gufo_parser.INEQUALITY  
   | "["      -> Gufo_parser.OPEN_SQRBRACKET  
   | ".["     -> Gufo_parser.OPEN_SQRIDXBRACKET  
   | "]"      -> Gufo_parser.CLOSE_SQRBRACKET  
   | "("      -> Gufo_parser.OPEN_BRACKET  
   | ")"      -> Gufo_parser.CLOSE_BRACKET  
   | "int"    -> Gufo_parser.INTTYPE  
   | "float"  -> Gufo_parser.FLOATTYPE  
   | "string" -> Gufo_parser.STRINGTYPE  
   | "bool"   -> Gufo_parser.BOOLTYPE 
   | "cmd"    -> Gufo_parser.CMDTYPE 
   | "list"   -> Gufo_parser.LISTTYPE  
   | "set"    -> Gufo_parser.SETTYPE  
   | "map"    -> Gufo_parser.MAPTYPE  
   | "option" -> Gufo_parser.OPTIONTYPE  

   | freetype            -> Gufo_parser.FREETYPE (Sedlexing.Utf8.lexeme lexbuf)
   | "extends"           -> Gufo_parser.EXTENDS  
   | "SHas?"                -> Gufo_parser.SHAS
   | "MHas?"                -> Gufo_parser.MHAS
   | "in"                -> Gufo_parser.IN  
   | "if"                -> Gufo_parser.IF  
   | "then"              -> Gufo_parser.THEN  
   | "else"              -> Gufo_parser.ELSE  
   | '.'                 -> Gufo_parser.DOT  
   | int_rep             -> Gufo_parser.INT (int_of_string (Sedlexing.Utf8.lexeme lexbuf)) 
   | float_rep           -> Gufo_parser.FLOAT (float_of_string (Sedlexing.Utf8.lexeme lexbuf)) 
   | "None"              -> Gufo_parser.NONE 
   | "Some"              -> Gufo_parser.SOME 
   | "**START**"         -> Gufo_parser.START  
   | ";"                 -> Gufo_parser.SEMICOLON  
   | ";;"                -> Gufo_parser.DOUBLE_SEMICOLON  
   | '{'                 -> Gufo_parser.OPEN_BRACE  
   | '}'                 -> Gufo_parser.CLOSE_BRACE  
   | ','                 -> Gufo_parser.COMMA  

   | varname             -> Gufo_parser.VARNAME (Sedlexing.Utf8.lexeme lexbuf)
   | envvar              -> Gufo_parser.ENVVAR (Sedlexing.Utf8.lexeme lexbuf)

   | varfield            -> Gufo_parser.VARFIELD (Sedlexing.Utf8.lexeme lexbuf)
   | modulVar            -> Gufo_parser.MODULVAR (Sedlexing.Utf8.lexeme lexbuf)
   | modul               -> Gufo_parser.MODUL (Sedlexing.Utf8.lexeme lexbuf)
   | word                -> Gufo_parser.WORD (Sedlexing.Utf8.lexeme lexbuf) 
   | arg                 -> Gufo_parser.ARG (Sedlexing.Utf8.lexeme lexbuf)
   | file                -> Gufo_parser.FILE (Sedlexing.Utf8.lexeme lexbuf)
   | eof                 -> Gufo_parser.EOF 
   | any                   -> raise_ParseErrorWithMsg lexbuf "unexpected character."
   | _ -> raise_ParseErrorWithMsg lexbuf "unexpected character."




let read lexbuf =
  let data = try_read_str lexbuf in
  match data with 
    | None -> 
        let data = read_ lexbuf in
        data, (Sedlexing.lexing_positions lexbuf)
    | Some (data,pos) -> data, pos
