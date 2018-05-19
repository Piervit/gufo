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
  this is work in progress not used for now.

*)

let newline = %sedlex.regexp? ('\r' | '\n' | "\r\n")

let rec token buf =
  match%sedlex lexbuf with
  | white_space -> token buf
  | newline  { next_line lexbuf; read lexbuf }


