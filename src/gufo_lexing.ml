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



type token =
  (* generic *)
  | STRUCT (* or DEF *)
  | LET
  | JOKER (*_*)
  | COLON (* : *)
  | AFFECTATION (* = *)
  | TRUE 
  | FALSE
  | ARROW (* -> *)
  | WITH (* also array rilated *)
  | WITHOUT (* also array rilated *)
  (* file/dir shortcut *)
  | TILDE(* userdir *)
  (* command rilated *)
  | PIPE
  | CLOSING_CHEVRON (* > *)
  | DOUBLE_CLOSING_CHEVRON (* >> *)
  | WRITE_ERROR_TO (* 2> *)
  | WRITE_ERROR_NEXT_TO (* 2>> *)
  | WRITE_ERROR_TO_STD (* 2>&1 *)
  | WRITE_ALL_TO (* >& *)
  | OPENING_CHEVRON (* < *)
  (* mathematic *)
  | ADDITION
  | SUBSTRACTION
  | DIVISION
  | STAR
  | MODULO
  | EQUALITY (* == *)
  | INEQUALITY (* != *)
  | GREATER_THAN (* gt *)
  | GREATER_OR_EQUAL (* gte *)
  | LOWER_THAN (* lt *)
  | LOWER_OR_EQUAL (* lte *)
  (* array rilated *)
  | SETMAP_OPEN (* [ *)
  | SETMAP_CLOSE (* ] *)

  (* condition *)
  | IF
  | THEN
  | ELSE
  (* type *)
  | INTTYPE 
  | FLOATTYPE
  | STRINGTYPE
  | LISTTYPE
  | OPTIONTYPE
  | BOOLTYPE
  | CMDTYPE
  | INT of int
  | FLOAT of float
  | STRING of string
  (* others *)
  | IN
  | OPEN_BRACE
  | CLOSE_BRACE
  | OPEN_BRACKET
  | CLOSE_BRACKET
  | SEMICOLON (* ; *)
  | VARNAME of string
  | WORD of string
  | NONE
  | COMMA
  | EOF







