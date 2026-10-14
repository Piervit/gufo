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

(*Parse a line of shell and return it as a low level program if it is valid.*)
val parse_shell : string -> GufoParsed.mprogram option

(*Parse a file and return it as a low level program if it is valid.*)
val parse_file : string -> GufoParsed.mprogram option

(*This is the base function to transform a parsed program into a fullprogram.*)
val handle_program : GufoParsed.mprogram -> GenUtils.StringMap.key -> GufoParsed.fullprog

(*handle_consolprog consolProg previous_optfullprog 
 Merge consolProg (a program from the console) and a previous high level fully
valid program into a new high level fully valid program.
*)
val handle_consolprog: GufoParsed.mprogram -> Gufo.MCore.fullprogopt -> GufoParsed.fullprog
