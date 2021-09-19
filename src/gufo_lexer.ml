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




let rec read_ lexbuf = 
  match%sedlex lexbuf with
   | '"' ->
     let start_pos, _end_pos = Sedlexing.lexing_positions lexbuf in
     let token = read_string (Buffer.create 17) lexbuf in
     let _start_pos, end_pos =  Sedlexing.lexing_positions lexbuf in
     token, (start_pos, end_pos)
   | white_space -> read_ lexbuf
   | newline  -> read_ lexbuf 
   | comment  -> read_ lexbuf 
   | "struct" -> Gufo_parser.STRUCT, (Sedlexing.lexing_positions lexbuf)
   | "let"    -> Gufo_parser.LET, (Sedlexing.lexing_positions lexbuf)
   | "fun"    -> Gufo_parser.FUN    , (Sedlexing.lexing_positions lexbuf)
(*    | "_"      -> Gufo_parser.JOKER   *)
   | ":"      -> Gufo_parser.COLON  , (Sedlexing.lexing_positions lexbuf)
   | "="      -> Gufo_parser.AFFECTATION  , (Sedlexing.lexing_positions lexbuf)
   | "True"   -> Gufo_parser.TRUE  , (Sedlexing.lexing_positions lexbuf)
   | "False"  -> Gufo_parser.FALSE  , (Sedlexing.lexing_positions lexbuf)
   (* pattern matching *)
   | "->"     -> Gufo_parser.ARROW  , (Sedlexing.lexing_positions lexbuf)
   | "With"   -> Gufo_parser.WITH  , (Sedlexing.lexing_positions lexbuf)
   | "SWith"  -> Gufo_parser.WITH_SET , (Sedlexing.lexing_positions lexbuf)
   | "MWith"  -> Gufo_parser.WITH_MAP, (Sedlexing.lexing_positions lexbuf)
   | "SWout"   -> Gufo_parser.WITHOUT_SET , (Sedlexing.lexing_positions lexbuf)
   | "MWout"   -> Gufo_parser.WITHOUT_MAP, (Sedlexing.lexing_positions lexbuf)
   | "*"      -> Gufo_parser.STAR  , (Sedlexing.lexing_positions lexbuf)
   | "*."     -> Gufo_parser.STAR_DOT, (Sedlexing.lexing_positions lexbuf)
   | "~"      -> Gufo_parser.TILDE  , (Sedlexing.lexing_positions lexbuf)
   | ">"      -> Gufo_parser.CLOSING_CHEVRON  , (Sedlexing.lexing_positions lexbuf)
   | ">-"     -> Gufo_parser.MINUS_CLOSING_CHEVRON  , (Sedlexing.lexing_positions lexbuf)
   | ">>"     -> Gufo_parser.DOUBLE_CLOSING_CHEVRON  , (Sedlexing.lexing_positions lexbuf)
   | "2>"     -> Gufo_parser.WRITE_ERROR_TO  , (Sedlexing.lexing_positions lexbuf)
   | "2>>"    -> Gufo_parser.WRITE_ERROR_NEXT_TO  , (Sedlexing.lexing_positions lexbuf)
   | "2>&1"   -> Gufo_parser.WRITE_ERROR_TO_STD  , (Sedlexing.lexing_positions lexbuf)
   | ">&"     -> Gufo_parser.WRITE_ALL_TO  , (Sedlexing.lexing_positions lexbuf)
   | "&"      -> Gufo_parser.SIMPLE_AND  , (Sedlexing.lexing_positions lexbuf)
   | "&&"     -> Gufo_parser.AND  , (Sedlexing.lexing_positions lexbuf)
   | "||"     -> Gufo_parser.OR  , (Sedlexing.lexing_positions lexbuf)
   | "|"      -> Gufo_parser.PIPE  , (Sedlexing.lexing_positions lexbuf)
   | "<"      -> Gufo_parser.OPENING_CHEVRON  , (Sedlexing.lexing_positions lexbuf)
   | "-<"     -> Gufo_parser.MINUS_OPENING_CHEVRON  , (Sedlexing.lexing_positions lexbuf)
   | "+"      -> Gufo_parser.PLUS  , (Sedlexing.lexing_positions lexbuf)
   | "+."      -> Gufo_parser.PLUS_DOT, (Sedlexing.lexing_positions lexbuf)
   | "^"      -> Gufo_parser.PLUS_STR , (Sedlexing.lexing_positions lexbuf)
   | "--"     -> Gufo_parser.DOUBLE_MINUS  , (Sedlexing.lexing_positions lexbuf)
   | "-"      -> Gufo_parser.MINUS  , (Sedlexing.lexing_positions lexbuf)
   | "-."      -> Gufo_parser.MINUS_DOT, (Sedlexing.lexing_positions lexbuf)
   | "%"      -> Gufo_parser.DIVISION  , (Sedlexing.lexing_positions lexbuf)
   | "%."      -> Gufo_parser.DIVISION_DOT, (Sedlexing.lexing_positions lexbuf)
   | "mod"    -> Gufo_parser.MODULO  , (Sedlexing.lexing_positions lexbuf)
   | "mod."    -> Gufo_parser.MODULO_DOT, (Sedlexing.lexing_positions lexbuf)
   | "=="     -> Gufo_parser.EQUALITY  , (Sedlexing.lexing_positions lexbuf)
   | "<="     -> Gufo_parser.EQ_OPENING_CHEVRON, (Sedlexing.lexing_positions lexbuf)
   | ">="     -> Gufo_parser.EQ_CLOSING_CHEVRON, (Sedlexing.lexing_positions lexbuf)
   | "!="     -> Gufo_parser.INEQUALITY  , (Sedlexing.lexing_positions lexbuf)
   | "["      -> Gufo_parser.OPEN_SQRBRACKET  , (Sedlexing.lexing_positions lexbuf)
   | ".["     -> Gufo_parser.OPEN_SQRIDXBRACKET  , (Sedlexing.lexing_positions lexbuf)
   | "]"      -> Gufo_parser.CLOSE_SQRBRACKET  , (Sedlexing.lexing_positions lexbuf)
   | "("      -> Gufo_parser.OPEN_BRACKET  , (Sedlexing.lexing_positions lexbuf)
   | ")"      -> Gufo_parser.CLOSE_BRACKET  , (Sedlexing.lexing_positions lexbuf)
   | "int"    -> Gufo_parser.INTTYPE  , (Sedlexing.lexing_positions lexbuf)
   | "float"  -> Gufo_parser.FLOATTYPE  , (Sedlexing.lexing_positions lexbuf)
   | "string" -> Gufo_parser.STRINGTYPE  , (Sedlexing.lexing_positions lexbuf)
   | "bool"   -> Gufo_parser.BOOLTYPE , (Sedlexing.lexing_positions lexbuf)
   | "cmd"    -> Gufo_parser.CMDTYPE , (Sedlexing.lexing_positions lexbuf)
   | "list"   -> Gufo_parser.LISTTYPE  , (Sedlexing.lexing_positions lexbuf)
   | "set"    -> Gufo_parser.SETTYPE  , (Sedlexing.lexing_positions lexbuf)
   | "map"    -> Gufo_parser.MAPTYPE  , (Sedlexing.lexing_positions lexbuf)
   | "option" -> Gufo_parser.OPTIONTYPE  , (Sedlexing.lexing_positions lexbuf)

   | freetype            -> Gufo_parser.FREETYPE (Sedlexing.Utf8.lexeme lexbuf),
                            (Sedlexing.lexing_positions lexbuf)
   | "extends"           -> Gufo_parser.EXTENDS  , (Sedlexing.lexing_positions lexbuf)
   | "SHas?"                -> Gufo_parser.SHAS, (Sedlexing.lexing_positions lexbuf)
   | "MHas?"                -> Gufo_parser.MHAS, (Sedlexing.lexing_positions lexbuf)
   | "in"                -> Gufo_parser.IN  , (Sedlexing.lexing_positions lexbuf)
   | "if"                -> Gufo_parser.IF  , (Sedlexing.lexing_positions lexbuf)
   | "then"              -> Gufo_parser.THEN  , (Sedlexing.lexing_positions lexbuf)
   | "else"              -> Gufo_parser.ELSE  , (Sedlexing.lexing_positions lexbuf)
   | '.'                 -> Gufo_parser.DOT  , (Sedlexing.lexing_positions lexbuf)
   | int_rep             -> Gufo_parser.INT (int_of_string (Sedlexing.Utf8.lexeme lexbuf)) , (Sedlexing.lexing_positions lexbuf)
   | float_rep           -> Gufo_parser.FLOAT (float_of_string (Sedlexing.Utf8.lexeme lexbuf)) , (Sedlexing.lexing_positions lexbuf)
   | "None"              -> Gufo_parser.NONE , (Sedlexing.lexing_positions lexbuf)
   | "Some"              -> Gufo_parser.SOME , (Sedlexing.lexing_positions lexbuf)
   | "**START**"         -> Gufo_parser.START  , (Sedlexing.lexing_positions lexbuf)
   | ";"                 -> Gufo_parser.SEMICOLON  , (Sedlexing.lexing_positions lexbuf)
   | ";;"                -> Gufo_parser.DOUBLE_SEMICOLON  , (Sedlexing.lexing_positions lexbuf)
   | '{'                 -> Gufo_parser.OPEN_BRACE  , (Sedlexing.lexing_positions lexbuf)
   | '}'                 -> Gufo_parser.CLOSE_BRACE  , (Sedlexing.lexing_positions lexbuf)
   | ','                 -> Gufo_parser.COMMA  , (Sedlexing.lexing_positions lexbuf)

   | varname             -> Gufo_parser.VARNAME (Sedlexing.Utf8.lexeme lexbuf), (Sedlexing.lexing_positions lexbuf)
   | envvar              -> Gufo_parser.ENVVAR (Sedlexing.Utf8.lexeme lexbuf), (Sedlexing.lexing_positions lexbuf)

   | varfield            -> Gufo_parser.VARFIELD (Sedlexing.Utf8.lexeme lexbuf), (Sedlexing.lexing_positions lexbuf)
   | modulVar            -> Gufo_parser.MODULVAR (Sedlexing.Utf8.lexeme lexbuf), (Sedlexing.lexing_positions lexbuf)
   | modul               -> Gufo_parser.MODUL (Sedlexing.Utf8.lexeme lexbuf), (Sedlexing.lexing_positions lexbuf)
   | word                -> Gufo_parser.WORD (Sedlexing.Utf8.lexeme lexbuf) , (Sedlexing.lexing_positions lexbuf)
   | arg                 -> Gufo_parser.ARG (Sedlexing.Utf8.lexeme lexbuf), (Sedlexing.lexing_positions lexbuf)
   | file                -> Gufo_parser.FILE (Sedlexing.Utf8.lexeme lexbuf), (Sedlexing.lexing_positions lexbuf)
   | eof                 -> Gufo_parser.EOF , (Sedlexing.lexing_positions lexbuf)
   | any                   -> raise_ParseErrorWithMsg lexbuf "unexpected character."
   | _ -> raise_ParseErrorWithMsg lexbuf "unexpected character."




let read lexbuf =
  read_ lexbuf 
