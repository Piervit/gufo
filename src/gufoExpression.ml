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

open CamomileLibrary
(******************************* EXPR ****************************************)
(*expression manipulation: an expression is made from a zed_edit context *)

let count_lines_str str = 
  Zed_utf8.fold 
    (fun achar acc -> 
      match UChar.uint_code achar with
        | 0x000A (* newline *) -> acc + 1
        | _ -> acc
    )
    str 1 

(*
  str: a rope
  res: integer
*)
let count_lines_rope str = 
  Zed_rope.fold 
    (fun achar acc -> 
      match UChar.uint_code (Zed_char.core achar) with
        | 0x000A (* newline *) -> acc + 1
        | _ -> acc
    )
    str 1 


let create_empty_expr () = 
  let zedit = Zed_edit.create ?editable:(Some (fun _ _ -> true)) () in
  let zcursor = Zed_edit.new_cursor zedit in
  Zed_edit.context zedit zcursor

let is_empty_expr expr = Zed_rope.is_empty (Zed_edit.text (Zed_edit.edit expr))

let get_edit_ expr = Zed_edit.edit expr

let ctx_to_zed_string expr = Zed_rope.to_string (Zed_edit.text (Zed_edit.edit expr))
let ctx_to_string expr = Zed_string.to_utf8 (ctx_to_zed_string expr)

let str_to_zed_rope str = Zed_rope.of_string (Zed_string.of_utf8 str)

let utf8_to_expr str = 
  let nctx = create_empty_expr () in
  Zed_edit.insert nctx (Zed_rope.of_string str);
  nctx

(*
  Get the coordinate of the zed cursor in an expression.
  Return a pair of integer (x,y).
*)
let get_coord expr = Zed_cursor.get_coordinates (Zed_edit.cursor expr)

let get_lines_count expr = (Zed_lines.count (Zed_edit.lines (get_edit_ expr) )) + 1

let insert_in_expr expr achar = Zed_edit.insert expr (Zed_rope.make 1 achar); expr

let delete_in_expr expr = Zed_edit.remove_prev expr 1; expr

let to_uchar achar = Zed_char.of_utf8 achar

let insert_newline expr = Zed_edit.newline expr; 
                          insert_in_expr  (insert_in_expr expr (to_uchar " ")) (to_uchar " ")

let count_lines expr = (Zed_lines.count (Zed_edit.lines (get_edit_ expr))) + 1

(*Return the word on which the cursor currently is.*)
let get_current_word expr = 
  let row, col = get_coord expr in
  let curline =  Zed_edit.get_line (Zed_edit.edit expr) row in
  let curword = Zed_utf8.init 0 (fun i -> UChar.of_char ' ') in
  let rec get_word rop pos curword  = 
    match pos with 
      | -1 -> curword
      | _ -> 
      (let curchar = Zed_rope.get rop pos in
      match UChar.uint_code (Zed_char.core curchar) with
        | 32 (* space *) -> curword
        | _ -> get_word rop 
                 (pos - 1)(Zed_utf8.insert curword 0 (Zed_utf8.singleton (Zed_char.core curchar)))
      )
  in
  get_word curline (col - 1 ) curword, (row, col)

(*transform a multilie expr into a 1 line one by removing the line return.
  it emphasize at letting the cursor on the same position.
  one_line_expr : expr -> one_line_expr, curseur_pos
*)
let to_one_line_expr expr =
  let row, col = get_coord expr in
  let new_rop_buf = Zed_rope.Buffer.create () in
  let (_,_, new_curs_col) = 
    Zed_rope.fold
      (fun uchar (curs_row, curs_col, new_curs_col) ->
        (match UChar.uint_code (Zed_char.core uchar) with
          | 0x000A
          | 0x000D (*newline *) ->
            (curs_row - 1, curs_col, new_curs_col)
          | uchari -> 
              let _ =  Zed_rope.Buffer.add new_rop_buf uchar in
              match curs_row, curs_col with 
                | 0, 0 -> (0, 0, new_curs_col)
                | 0, i ->
                  (0, curs_col - 1, new_curs_col + 1)
                | _, _ ->
                  (curs_row , curs_col , new_curs_col + 1)
      ))
      (Zed_edit.text (Zed_edit.edit expr)) (row, col, -1)
  in
    (Zed_string.to_utf8 (Zed_rope.to_string (Zed_rope.Buffer.contents new_rop_buf)), new_curs_col)



(* The message will be splitted in such a way that it has no lines longer than
 * max_line_size.*)
let split_rope_message str max_line_size = 
  let new_rop_buf = Zed_rope.Buffer.create () in
  let  _ = 
    Zed_rope.fold 
      (fun uchar cur_col_num ->
        (match UChar.uint_code (Zed_char.core uchar) with
          | 0x000A
          | 0x000D (*newline *) ->
              let _ = Zed_rope.Buffer.add new_rop_buf (Zed_char.unsafe_of_uChar (UChar.chr 0x000A)) in 0
          | uchari -> 
              match cur_col_num with
                | i when i = (max_line_size - 1) ->
                    let _ = Zed_rope.Buffer.add new_rop_buf (Zed_char.unsafe_of_uChar (UChar.chr 0x000A)) in
                    let _ = Zed_rope.Buffer.add new_rop_buf uchar in
                    1
                | _ -> 
                    let _ =  Zed_rope.Buffer.add new_rop_buf uchar in
                    cur_col_num + 1
      ))
      str 0
  in
    Zed_rope.Buffer.contents new_rop_buf



(*get_partial_expr: string -> int -> (string, int) 
 * for an expression and a position in this expression, return a partial expr
 * and the offset at which the reduced expr starts within the initial
 * expression.
 *)
let get_partial_expr expr pos_expr =
  (*we first exploit parenthesis*)
  let end_parenthesis_pos = String.index_from_opt expr pos_expr ')' in 
  let first_parenthesis_pos = String.rindex_from_opt expr pos_expr '(' in 
  let new_expr,pos_expr = 
    match end_parenthesis_pos, first_parenthesis_pos with
      | Some end_pos, Some first_pos -> 
          String.sub expr first_pos (end_pos - first_pos), first_pos
      | None, _ 
      | _, None -> expr, pos_expr
  in
  (*we check if we are within a function or affectation scope*)
  let first_fun_scope = 
    try Str.search_backward (Str.regexp "->") new_expr pos_expr
    with Not_found -> -1
  in
  let first_affectation_scope = 
    try Str.search_backward (Str.regexp "=") new_expr pos_expr
    with Not_found -> -1 
  in
  match first_fun_scope, first_affectation_scope with
    | -1, -1 
    | 0, 0 ->  expr, 0
    | -1, i ->
      String.sub new_expr (first_affectation_scope + 1) (String.length new_expr - first_affectation_scope - 1), pos_expr - first_affectation_scope
    | j, i when i > j ->
      String.sub new_expr (first_affectation_scope + 1) (String.length new_expr - first_affectation_scope - 1), pos_expr - first_affectation_scope
    | i, -1 ->
      String.sub new_expr (first_fun_scope + 1) (String.length new_expr - first_fun_scope - 1), pos_expr - first_fun_scope
    | i, j when i > j ->
      String.sub new_expr (first_fun_scope + 1) (String.length new_expr - first_fun_scope - 1), pos_expr - first_fun_scope 
    | _ -> assert false




