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

type debug_level =
   | DBG_NO_DEBUG (* NO DEBUG INFORMATION *)
   | DBG_INFO     (* ONLY MAIN INFORMATION *)
   | DBG_FULL     (* FULL INFORMATION *)
 

let debug = ref DBG_NO_DEBUG


let set_debug level =
  debug := level

let get_debug_level () = 
  !debug

let max_char_print_buffer = 10000

let get_max_char_PrintBuffer () = 
  max_char_print_buffer

let max_char_printerr_buffer = 10000

let get_max_char_printErrBuffer() = 
  max_char_printerr_buffer
