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

(*Count the line of a Zed_utf8.t *)
val count_lines_str : Zed_utf8.t -> int
(*Count the line of a Zed_rope.t *)
val count_lines_rope : Zed_rope.rope -> int
(*Count the line of a Zed_edit.context *)
val count_lines: 'a Zed_edit.context -> int

val create_empty_expr : unit -> 'a Zed_edit.context
val is_empty_expr : 'a Zed_edit.context -> bool

val get_edit_:'a Zed_edit.context -> 'a Zed_edit.t

val ctx_to_zed_string : 'a Zed_edit.context -> Zed_string.t
val ctx_to_string: 'a Zed_edit.context -> string
val str_to_zed_rope : string -> Zed_rope.rope

val utf8_to_expr: Zed_string.t -> 'a Zed_edit.context

(*
  Get the coordinate of the zed cursor in an expression.
  Return a pair of integer (x,y).
*)
val get_coord: 'a Zed_edit.context -> int * int
val get_lines_count: 'a Zed_edit.context -> int

val insert_in_expr: 'a Zed_edit.context -> Zed_char.t -> 'a Zed_edit.context
val delete_in_expr: 'a Zed_edit.context -> 'a Zed_edit.context

val to_uchar : string -> Zed_char.t

val insert_newline: 'a Zed_edit.context -> 'a Zed_edit.context

(*Return the word on which the cursor currently is(as well as the cursor position.*)
val get_current_word: 'a Zed_edit.context -> Zed_utf8.t * (int * int)

(*transform a multiline expr into a 1 line one by removing the line return.
  it emphasize at letting the cursor on the same position.
  one_line_expr : expr -> one_line_expr, curseur_pos
*)
val to_one_line_expr: 'a Zed_edit.context -> string * int

(* exprList_to_splittedMessage: str_lst max_line_size space_size :

  Transform the list of string str_lst into a string with correct line return
according to the specified max_line_size. Space size is the number of space
positionned between every part of str_lst.
*)
val exprList_to_splittedMessage: string list -> int -> int -> Zed_rope.rope

(*split_rope_message str max_line_size : Convert a string as a rope with
correct line return according to max_line_size. It internally use
exprList_to_splittedMessage. *)
val split_rope_message: string -> int -> Zed_rope.rope

(*get_partial_expr: string -> int -> (string, int) 
 * for an expression and a position in this expression, return a partial expr
 * and the offset at which the reduced expr starts within the initial
 * expression.
 *)
val get_partial_expr : string -> int -> string * int
