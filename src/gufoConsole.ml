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

(* Implementation of the interactive console. *)
(*This console is considered to be always at least 80 collums wide. If we have
 * message that (main expr) or error that are wider that this size, they should
 * be splitted automatically. *)


open GenUtils
open GufoParsed
open GufoExpression
open CamomileLibrary
open React
open Lwt
open LTerm_style
open LTerm_text
open LTerm_geom
open Printf
open GenUtils
open Gufo
open Gufo.MCore
open LTerm_event
open LTerm_key
open Sedlex_menhir

exception ExitTerm of (LTerm.t * LTerm.mode Lwt.t)

(*internal mod (raw mode) *)
let term_rawmod = ref None

(*state if we are in search mod or no.*)
let term_search_mod = ref None

let size = ref {
              rows = 0;
              cols = 0;
              }

(******************************* COLORS **************************************)
(* foreground color *)

(* 
  List of colors

val red : color
val green : color
val yellow : color
val blue : color
val magenta : color
val cyan : color
val white : color

val lred : color
val lgreen : color
val lyellow : color
val lblue : color
val lmagenta : color
val lcyan : color
val lwhite : color
*)
 
let c_error = red
let c_search = yellow
let c_result = yellow

let c_keyword = lcyan
let c_bool = lgreen
let c_int = green
let c_float = blue
let c_string = lred
let c_cmd = magenta
let c_fun = lyellow
let c_comp = lmagenta
let c_unknown = lwhite
let c_comment = cyan
let c_aggregate = lblue (*list, set or map*)


(* background color *)
let cb_option = blue

let completion_separator_size = 4


(******************************* END COLORS **********************************)


(******************************* PREFIX **************************************)
(*A prefix printed at the beginning of a terminal command *)
let prefix_len = 2
(* prefix printing *)
let prefix = "% "

let print_prefix term =  
  LTerm.move term 0 (-500) >>= 
  (fun () -> LTerm.fprints term (eval [B_fg c_result; S prefix ]))

(******************************* END PREFIX **********************************)
(******************************* EXPR ****************************************)

(*get the lenght of current line in expr (including eventual prefix) *)
let get_line_length expr line_num = 
  match line_num with
    | 0 -> (Zed_rope.length (Zed_edit.get_line (get_edit_ expr) line_num)) + prefix_len
    | _ -> Zed_rope.length (Zed_edit.get_line (get_edit_ expr) line_num)

(*
  Get the coordinate of the cursor in the terminal expression (consider prefix
  for exemple).
  Return a pair of integer (x,y).
*)
let get_cursor_coord expr = 
  let row, col = get_coord expr in
    match row with 
      | 0 -> row , col + prefix_len
      | _ -> row, col



(******************************* END EXPR ************************************)
(******************************* HISTORY *************************************)

let filtered_contents filter history =
  let ctt = LTerm_history.contents history in
    List.filter (fun str -> Zed_utf8.contains (Zed_string.to_utf8 str) filter) ctt

  (*If search_expr doesn t match, we keep cur_expr.*)
let find_hist_expr cur_expr (history, hist_id) search_expr =
  try
    let str = List.nth (filtered_contents search_expr history) hist_id in 
    utf8_to_expr str
  with Failure _ ->
    create_empty_expr ()

let search_prefix = "search: "

let search_len search_expr = 
  8 + String.length search_expr


(******************************* END HISTORY *********************************)

(******************************* ERROR PRINTING ******************************)

let nb_lines_error = ref 0
let nb_lines_completion = ref 0

(*used to clear error or completion display *)
let clear_res_generic term cur_expr nb_lines_to_del = 
  let rec recursively_clear nb_lines = 
    match nb_lines with 
      | 0 -> return ()
      | i -> LTerm.clear_line term >>= 
             (fun () -> LTerm.move term 1 0) >>= 
             (fun () -> recursively_clear (nb_lines - 1))
  in

  match (nb_lines_to_del) with
    | 0 -> return ()
    | nb_error_line ->
        let expr_cursor_row, expr_cursor_col = get_cursor_coord cur_expr in
        let expr_lines_count = (get_lines_count cur_expr) - 1 in
        LTerm.move term (expr_lines_count - expr_cursor_row + 1) (-500) >>=
        (fun () -> recursively_clear (nb_lines_to_del))
        >>=
        (fun () ->  LTerm.move term 0 (-500))
        >>=
        (fun () -> LTerm.move term (expr_cursor_row - expr_lines_count - (nb_lines_to_del + 1) ) expr_cursor_col)

(*clear error display*)
let clear_res_err term cur_expr = 
  (clear_res_generic term cur_expr !nb_lines_error)
  >>=
  (fun () -> nb_lines_error:= 0; return ())

(*clear completion display*)
let clear_res_completion term cur_expr = 
  match !nb_lines_completion with
  | 0 -> (*do nothing *) return ()
  | _ -> 
    (clear_res_generic term cur_expr !nb_lines_completion)
    >>=
    (fun () -> nb_lines_completion:= 0; return ())

(*clear completion display*)
let clear_res_err_and_comple term cur_expr = 
  (clear_res_generic term cur_expr (!nb_lines_error + !nb_lines_completion))
  >>=
  (fun () -> nb_lines_error:= 0; nb_lines_completion:= 0;  return ())
let clear_extra_infos term cur_expr = 
  match !nb_lines_error, !nb_lines_completion with
    | 0, 0 -> return ()
    | 0, i -> clear_res_completion term cur_expr
    | i, 0 -> clear_res_err term cur_expr 
    | i, y -> clear_res_err_and_comple term cur_expr 

let print_err term cur_expr str_err = 
  (*str is splitted so it has no line longer than term size. *)
  let str_err = split_rope_message str_err (!size.cols - 1 ) in 
    debug_print (sprintf "str_error '%s' \n"  (Zed_string.to_utf8 (Zed_rope.to_string str_err))); 
  let nb_error_line = count_lines_rope str_err in
    debug_print (sprintf "Nb error lines: %d\n" nb_error_line); 
  let expr_cursor_row, expr_cursor_col = get_cursor_coord cur_expr in
  let expr_lines_count = get_lines_count cur_expr in
(*   clear_res_err_and_comple term cur_expr >>= *)
(*   (fun () ->  *)
  LTerm.fprints term (eval [B_fg c_error; S "\n"]) >>=
  (fun () -> nb_lines_error:= nb_error_line;
    LTerm.move term (expr_lines_count - expr_cursor_row - 1 ) (-500)) >>=
  (fun () -> 
  LTerm.fprints term (eval [B_fg c_error; R (str_err)]))
  >>=
  (fun () -> LTerm.move term 0 (-500)) >>=
  (fun () -> LTerm.move term (expr_cursor_row  + 1 - expr_lines_count - nb_error_line  ) (expr_cursor_col))

(******************************* END ERROR PRINTING **************************)

(******************************* CLEARING ************************************)

let clear_expr term expr = 
  clear_res_err_and_comple term expr >>=
  (fun () -> 
    let nb_lines = count_lines expr in
    let curs_row, curs_col = 
      get_cursor_coord expr 
    in
    let rec del_until nb_to_del = 
      match nb_to_del with
        | 0 -> 
              LTerm.move term 1 0 
        | nb_to_del ->
            LTerm.clear_line term >>= 
              (fun () -> LTerm.move term (-1) 0 )  >>=
              (fun () -> del_until (nb_to_del - 1))
    in
      LTerm.move term (nb_lines - 1 - curs_row) (- curs_col)  >>=
      (fun () -> del_until nb_lines )
  )


let clear_search_mod term shell_env hist cur_expr search_expr = 
  let expr_cursor_row, expr_cursor_col = get_cursor_coord cur_expr in
  let expr_lines_count = get_lines_count cur_expr in 
  term_search_mod := None;
  LTerm.clear_line term >>=
  (fun () -> 
    LTerm.move term (expr_cursor_row - expr_lines_count  ) (expr_cursor_col - ( search_len search_expr )))
    
  
(******************************* END CLEARING ********************************)
(******************************* PRINTING ************************************)

  (*This function is to be called after the expr has been printed. The cursor
   * is expected at the end of the terminal printing.*)
let place_cursor_after_print term expr =
  let nb_lines_expr = get_lines_count expr in
  let curs_row, curs_col = get_cursor_coord expr in
  let len_last_line =  get_line_length expr (nb_lines_expr - 1 ) in
  LTerm.move term (curs_row + 1 - nb_lines_expr) (curs_col - len_last_line)
  

let is_integer word = 
  Str.string_match (Str.regexp "[0-9]+") word 0

let is_float word = 
  Str.string_match (Str.regexp "[0-9]+\\.[0-9]+")  word 0 

let is_bool word = 
  match word with 
    | "True" | "False" -> true 
    | _ -> false

let is_cmd word = 
  Str.string_match (Str.regexp "[a-z0-9]+")  word 0 


(*The basic printing: no color *)
let print_expr term expr = 
  print_prefix term  >>=
  (fun () -> LTerm.fprints term (eval [B_fg c_unknown; R (Zed_edit.text (Zed_edit.edit expr))]))  >>=
  (fun () -> place_cursor_after_print term expr) 

(*The basic printing: no color, except for error position. *)
let print_expr_with_err term expr error_pos = 
  (*We divide expr in 3 part:
    - the first "valid part"
    - the "error higlighted" part
    - the last "valid part".
  *)
  let error_start = error_pos.ppos_start.pos_cnum in
  let error_start = match error_start with 
                      | -1 -> 0
                      | _ -> error_start
  in 
  let error_end = error_pos.ppos_end.pos_cnum in
  let error_end = match error_end with 
                      | -1 -> 0
                      | _ -> error_end
  in 
  let full_expr = (Zed_edit.text (Zed_edit.edit expr)) in 
  let expr_before = Zed_rope.sub full_expr 0 error_start in
  let expr_err= Zed_rope.sub full_expr error_start (error_end - error_start)  in
  let expr_after= Zed_rope.sub full_expr error_end ((Zed_rope.length full_expr) - error_end) in
  print_prefix term  >>=
  (fun () -> LTerm.fprints term (eval [B_fg c_unknown; R expr_before;B_fg c_error; R expr_err; B_fg c_unknown; R expr_after]))  >>=
  (fun () -> place_cursor_after_print term expr) 




(*The advanced color printing *)
let print_color_expr term expr optprog types = 
  let print_type_not_found word = 
    [ S word ; B_fg c_unknown]
  in
  let print_type typ word = 
    let (typ,_pars_pos) = typ in
    (match typ with 
      | MOComposed_type _  -> 
          [ S word ; B_fg c_comp]
      | MOBase_type(   MTypeString) -> 
          [ S word ; B_fg c_string]
      | MOBase_type( MTypeBool) -> 
          [ S word; B_fg c_bool]
      | MOBase_type(   MTypeInt) -> 
          [S word ; B_fg c_int]
      | MOBase_type(   MTypeFloat) -> 
          [ S word ; B_fg c_float]
      | MOBase_type(   MTypeCmd) ->
          [ S word; B_fg c_cmd]
      | MOList_type(_ ) 
      | MOSet_type(_ ) 
      | MOMap_type(_ ) ->
          [ S word; B_fg c_aggregate]
      | MOFun_type _ ->
          [ S word; B_fg c_fun]
      | _  -> print_type_not_found word
                  
    )
  in
  let print_curword word = 
    (match UChar.uint_code (UTF8.get word 0) with
      | 0x0024 (*$*) ->
          (*TODO : it could be a module *)
          (*word is a var, we try to deduce its type *)
          (*this part is extremely buggy as it mainly use the string name of
           * the var to know the type. The fact is that the name is not unique.
           * For now we check if the name is a known topvar name to get the type.
           * If it is not, we inspect the mopg_topvar_bind_vars which has been
           * added for the use.
           *
           * This part should be rewritten and we should have a way to know if
             * word is a topvar or a binding var inside a topvar.
          *)
          (let var = String.sub word 1 ((String.length word) - 1) in
          let search_topvar2int () = 
                (match StringMap.find_opt var optprog.mofp_mainprog.mopg_topvar2int with
                  | None -> 
                      None
                  | Some id_var -> 
                      let typ = IntMap.find id_var (IntMap.find 0 types) 
                      in Some typ
                )
          in
          let search_bind_var () = 
            try (
              let bind_in_topvars = 
                match (IntMap.is_empty optprog.mofp_mainprog.mopg_topvar_bind_vars)
                with 
                | false -> 
                    let _, bind_in_topvars = 
                      IntMap.max_binding optprog.mofp_mainprog.mopg_topvar_bind_vars 
                    in 
                    bind_in_topvars
                | true -> 
                    optprog.mofp_mainprog.mopg_topcal_bind_vars
            in
              let ids = StringMap.find var bind_in_topvars in
              let b_type = (IntMap.find (IntSet.choose ids) (IntMap.find 0 types)) in
              (*TODO: we should check b_type is the same for every type, else, no color.
               *This require a good type_compare function.
               * *)
              (
                match IntSet.for_all (fun id -> true ) ids
              with
                | true -> 
                    Some b_type 
                | false -> None
                )
              )
                with | _ -> 
                  None
            in
              (match search_topvar2int (), search_bind_var () with
                | Some typ, None -> print_type typ word
                | None, Some typ -> print_type typ word
                | Some _ , Some _ -> print_type_not_found word
                | None, None ->
                  (match search_bind_var () with
                    | Some typ -> print_type typ word
                    | None -> print_type_not_found word
                  )
              )
          )
      | _ ->
        (match GufoUtils.is_keyword word, is_integer word, is_float word, is_bool word, is_cmd word with
          | true,_,_,_,_ -> (*keyword*)
              [S word ; B_fg c_keyword ]
          | _,_,true,_,_ -> (*float*)
              [S word ; B_fg c_float]
          | _,true,_,_,_ -> (*int*)
              [S word ; B_fg c_int; ]
          | _,_,_,true,_ -> (*bool*)
              [S word ; B_fg c_bool]
(*
          | _,_,_,_,true -> (*cmd*)
              [S word ; B_fg c_cmd]
*)
          | _,_,_,_,_ -> (*others*)
              [S word ; B_fg c_unknown] 
          )
    )
  in
  let colorize new_char (acc,curword,in_quote,in_comment) = 
    (match in_comment,in_quote with
      | true,_-> 
                [ S (sprintf "%s%c" curword (UChar.char_of new_char) );B_fg c_comment] @ acc, "",false,true
      | _,false ->
          (match UChar.uint_code new_char with
            | 0x000D (* newline *)
            | 0x000A (* newline *) 
            | 0x0028 (* ( *)
            | 0x0029 (* ) *)
            | 32 (* space *) ->
                let acc = if String.length curword > 0 
                  then print_curword curword @ acc else acc
                in
                [ S (sprintf "%c" (UChar.char_of new_char) );B_fg c_unknown] @ acc , "", false,false
            | 0x0022  -> (* quote symbol *)  
                let acc = 
                  if String.length curword > 0 
                    then print_curword curword @ acc else acc
                in 
                [ S (sprintf "%c" (UChar.char_of new_char) ); B_fg c_string] @ acc , "",true,false
            | 0x0023  -> (* comment symbol *)  
                [ S (sprintf "%c" (UChar.char_of new_char) ); B_fg c_comment] @ acc , "",false,true
(*
            | 0x002B  (* + symbol *)  
            | 0x002D  (* - symbol *)  
            | 0x002A  (* * symbol *)  
            | 0x002F  -> (* / symbol *)  
                let acc = if String.length curword > 0 
                  then print_curword curword @ acc else acc
                in
                [ S (sprintf "%c" (UChar.char_of new_char) );B_fg c_keyword] @ acc , "", false,false
*)
            | _ ->
              acc, sprintf "%s%s" curword (UTF8.init 1 (fun _ -> new_char) ), false,false
          )
      | _,true ->  (*in a string*)
          (match UChar.uint_code new_char with
            | 0x0022  -> (* quote symbol *)
                [ S (sprintf "%s%c" curword (UChar.char_of new_char) );B_fg c_string] @ acc, "",false,false
            | _ -> 
              acc, (sprintf "%s%s" curword (UTF8.init 1 (fun _ -> new_char) )), true , false
          )
    )
  in
  (if (not (is_empty_expr expr) )
  then (print_prefix term)
  else (return ()))
  >>=
  (fun () -> 
  let to_print, last_str,_,_ = 
    Zed_utf8.fold colorize (ctx_to_string expr) ([], "",false,false) (*list to print,curword, and a boolean to know if we are in quote*)
  in
  let to_print = 
    if String.length last_str > 0 then ((print_curword last_str) @ to_print) else to_print
  in
  LTerm.fprints term (eval (List.rev to_print)) >>=
    (fun () -> place_cursor_after_print term expr )
  ) 


(*Print the result of a program. 
  Simple one color output. 
*)
let print_res term str_res = 
  LTerm.fprints term (eval [B_fg c_result; R (str_to_zed_rope (str_res^"\n"))])

(******************************* END PRINTING ********************************)

(******************************* EXPRESSION CHECKING *************************)
(*Using the Gufo language to check if an expression is a valid one or not. *)

(*The reference to the current program*)
let fulloprog = ref empty_ofullprog

let check_full_expr cur_expr = 
  let str_expr = Zed_rope.to_string (Zed_edit.text (Zed_edit.edit cur_expr)) in 
  GufoStart.parse_shell (Zed_string.to_utf8 str_expr)

let analyse_and_print term cur_expr = 
    try (
      match check_full_expr cur_expr with 
      | Some p ->
          debug_info (debug_title0 "Creating a Gufo program.");
          (try (
             let prog = GufoStart.handle_consolprog p (!fulloprog) in 
(*             let prog = GufoStart.handle_program p "shell prog" in *)
            let opt_prog, types = 
            match  Gufo.MCore.is_empty_ofullprog (!fulloprog) with
              | true -> GufoParsedToOpt.parsedToOpt prog
              | false ->
                  let res = GufoParsedToOpt.add_prog_to_optprog !fulloprog prog in
                  res
            in
            print_color_expr term cur_expr opt_prog types
          )
          with | TypeError msg
               | SyntaxError msg ->
                  print_expr_with_err term cur_expr msg.loc_pos >>=
                  (fun () -> print_err term cur_expr msg.loc_val) >>=
                  (fun () -> print_err term cur_expr (Printexc.get_backtrace ()))

               | InternalError msg 
               | Sys_error msg  
               | VarError msg as e ->
                  print_expr term cur_expr >>=
                  (fun () -> print_err term cur_expr (Printexc.to_string e)) >>=
                  (fun () -> print_err term cur_expr (Printexc.get_backtrace ()))


               | Not_found as e -> 
                  print_expr term cur_expr >>=
                  (fun () -> print_err term cur_expr (Printexc.to_string e)) >>=
                  (fun () -> print_err term cur_expr (Printexc.get_backtrace ()))
        )
      | None ->
          print_expr term cur_expr
    )
    with 
      | TypeError (reason) 
      | SyntaxError (reason) -> 
         print_expr_with_err term cur_expr reason.loc_pos >>=
            (fun () -> print_err term cur_expr reason.loc_val)
      | ParseError(fname, line_start, line_end, col_start, col_end , tok, reason) -> 
       print_expr term cur_expr >>=
         let err_msg = string_of_ParseError (fname, 
                                             line_start, 
                                             line_end, 
                                             col_start, 
                                             col_end, 
                                             tok, reason) in
       (fun () -> print_err term cur_expr err_msg)
      | Failure msg -> 
       print_expr term cur_expr >>=
       (fun () -> print_err term cur_expr (sprintf "Failed : %s \n"msg)) >>=
       (fun () -> print_err term cur_expr (Printexc.get_backtrace ()))


let print_completion term shell_env hist cur_expr possibles_expr = 
      let str_possibles = exprList_to_splittedMessage 
                            possibles_expr (!size.cols - 1) completion_separator_size
      in 
      let nb_poss_line = count_lines_rope str_possibles in
      let expr_cursor_row, expr_cursor_col = get_cursor_coord cur_expr in
      let expr_lines_count = get_lines_count cur_expr in 
      (*if there is already a completion displayed, we clear it. *)
(*       clear_res_err_and_comple term cur_expr >>=  *)
(*       (fun () ->  *)
        LTerm.fprints term (eval [B_fg c_search; S "\n"]) >>=
      (fun () -> 
        LTerm.move term (expr_lines_count + !nb_lines_error - expr_cursor_row - 1 ) (-500)) >>=
      (fun () -> 
      LTerm.fprints term (eval [B_fg c_search; R  str_possibles;  ])) >>=
  (fun () -> 
      nb_lines_completion := nb_poss_line;
      LTerm.move term 0 (-500)) >>=
  (fun () -> LTerm.move term (expr_cursor_row  + 1 - expr_lines_count - !nb_lines_error - nb_poss_line) (expr_cursor_col))



  (*always reprint cur_expr because it can be a different one.*)
let print_search term shell_env hist cur_expr search_expr new_expr = 
  match (!term_search_mod) with 
    | None -> (*We just entered search mod*)
      let expr_cursor_row, expr_cursor_col = get_cursor_coord cur_expr in
      let expr_lines_count = get_lines_count cur_expr in 
      LTerm.fprints term (eval [B_fg c_search; S "\n"]) >>=
      (fun () -> 
        LTerm.move term (expr_lines_count - expr_cursor_row - 1 ) (-500)) >>=
      (fun () -> 
      LTerm.fprints term (eval [B_fg c_search; S search_prefix ; S  search_expr;  ]))
    | Some _ -> (*We already were in search mod*)
      LTerm.clear_line term >>=
      (fun () -> LTerm.move term (-1) (- (search_len search_expr))) >>=
      (fun () -> clear_expr term cur_expr) >>=
      (fun () -> analyse_and_print term new_expr) >>=
      (fun () -> 
        LTerm.fprints term (eval [B_fg c_search; S "\n"; S search_prefix; S  search_expr;  ]))




(*Do nothing: for non implemented function of keys. *)
let do_nothing term shell_env hist cur_expr = Lwt.return (Some (cur_expr, shell_env, hist))

(*Handle command fail:
  - or a bad action from the user
  - or a gufo execution issue.*)
let do_fail term shell_env hist cur_expr exc = 
  (*TODO*)
   clear_res_err_and_comple term cur_expr >>=
   (fun () -> print_err term cur_expr ("Unable to execute the previous line: " ^ (Printexc.to_string exc) ^ (Printexc.get_backtrace ()))) >>=
        (fun () -> LTerm.fprints term (eval [B_fg c_unknown; S "\n\n"])) >>=
        (fun () -> LTerm.flush term) >>=
   (fun () -> 
      let expr = insert_newline cur_expr in
        print_expr term expr )>>=
   (fun () -> return (Some (create_empty_expr (), shell_env, hist)))



let search_hist term shell_env hist cur_expr =
  match !term_search_mod with
    | None -> 
        print_search term shell_env hist cur_expr "" (create_empty_expr ()) >>=
      (fun () ->
        term_search_mod := Some "";
        return (Some (cur_expr, shell_env, hist)))
    | Some search_expr -> 
        clear_search_mod term shell_env hist cur_expr search_expr >>=
        (fun () -> return (Some (cur_expr, shell_env, hist)))

(*This function is a hack for an issue that I haven't been able to correct.
  Without call to this function, when I reach the bottom line of my screen, I
  got some unmanaged line change (current line goes up causing potentiel bad
  writing). This function print four new line and goes back to its position. It
  means we don't exploit the last lines but don't have issues with tem.
*)
let prepare_term term cur_expr = 
  LTerm.fprints term (eval [ S "\n"]) >>=
  (fun () ->  LTerm.fprints term (eval [ S "\n"]) )>>=
  (fun () ->  LTerm.fprints term (eval [ S "\n"]) )>>=
  (fun () ->  LTerm.fprints term (eval [ S "\n"]) )>>=
  (fun () -> LTerm.move term (-4) 0)

let print_char term shell_env hist cur_expr akey = 
    (prepare_term term cur_expr) >>=
    (fun () ->
    match (!term_search_mod) with
      | None -> (*standard mod*)
        clear_expr term cur_expr >>=
        (fun () ->
        let expr = 
          match akey.code with 
            | Char i  ->insert_in_expr cur_expr (Zed_char.unsafe_of_uChar i)
            | _ -> assert false
        in
          analyse_and_print term cur_expr >>= 
            (fun () -> return (Some (expr, shell_env, hist)))
        )
      | Some search_expr -> (*search mod*)
        let search_expr = 
          match akey.code with 
            | Char i  ->sprintf "%s%c" search_expr (UChar.char_of i)
            | _ -> assert false
        in
          let new_expr = find_hist_expr cur_expr hist search_expr in
          print_search term shell_env hist cur_expr search_expr new_expr >>=
            (fun () -> 
              term_search_mod := Some search_expr;
              return (Some (new_expr, shell_env, hist)))
    )

let multiline_expr term shell_env hist cur_expr = 
  clear_expr term cur_expr >>= 
  (fun () ->
    let expr = insert_newline cur_expr in 
    analyse_and_print term cur_expr >>= 
      (fun () -> return (Some (expr,shell_env, hist)))
  )

let first_line term shell_env (hist, hist_id) cur_expr = 
  (*try to retrieve the current line*)
  LTerm_history.add hist (ctx_to_zed_string cur_expr);
  let hist_id = 0 in
  debug_info (debug_title0 "Creating a Gufo program.");
  try (
  match check_full_expr cur_expr with 
    | Some p ->
(*         let prog = GufoStart.handle_program p "Shell_prog" in *)
        let prog = GufoStart.handle_consolprog p (!fulloprog) in 
        let opt_prog, types = 
        match  Gufo.MCore.is_empty_ofullprog (!fulloprog) with
          | true -> GufoParsedToOpt.parsedToOpt prog
          | false -> GufoParsedToOpt.add_prog_to_optprog !fulloprog prog
        in
        fulloprog := opt_prog;
          let redprog ,shell_env = (GufoEngine.exec opt_prog shell_env) in
          fulloprog := redprog;
        return (Some (create_empty_expr (), shell_env, (hist, hist_id)))
    | None ->
        return (Some (create_empty_expr (), shell_env, (hist, hist_id)))
  ) with 
       | TypeError _msg 
       | SyntaxError _msg ->
             return (Some (create_empty_expr (), shell_env, (hist, hist_id)))
       | InternalError _msg 
       | Sys_error _msg 
       | VarError _msg ->
              return (Some (create_empty_expr (), shell_env, (hist, hist_id)))
       | ParseError (_fname, _line_start, _line_end, _col_start, _col_end, _tok, _reason) ->
              return (Some (create_empty_expr (), shell_env, (hist, hist_id)))


let new_line_normal_mod term shell_env (hist, hist_id) cur_expr = 
  (*try to retrieve the current line*)
  LTerm_history.add hist (ctx_to_zed_string cur_expr);
  let hist_id = 0 in
  debug_info (debug_title0 "Creating a Gufo program.");
  try (
  match check_full_expr cur_expr with 
    | Some p ->
(*         let prog = GufoStart.handle_program p "Shell_prog" in *)
        let prog = GufoStart.handle_consolprog p (!fulloprog) in 
        let opt_prog, types = 
        match  Gufo.MCore.is_empty_ofullprog (!fulloprog) with
          | true -> GufoParsedToOpt.parsedToOpt prog
          | false -> GufoParsedToOpt.add_prog_to_optprog !fulloprog prog
        in
        fulloprog := opt_prog;
        clear_expr term cur_expr >>=
        (fun () -> print_color_expr term cur_expr opt_prog types) >>=
        (fun () -> LTerm.fprints term (eval [B_fg c_unknown; S "\n"])) >>=
        (fun () -> LTerm.flush term) >>=
        (fun () ->
          Zed_edit.goto_eot cur_expr; 
          let tmod = 
            match (!term_rawmod) with 
            | None -> assert false
            | Some tmod -> tmod
          in
          LTerm.leave_raw_mode term tmod) >>=
        (fun () -> 
          let redprog ,shell_env = (GufoEngine.exec opt_prog shell_env) in
          ( LTerm.enter_raw_mode term >>= (fun tmod -> term_rawmod:=Some tmod;
             Lwt.return () )) >>=
          (fun () -> 
          fulloprog := redprog;
          print_res term (Gufo.MCore.moval_to_string redprog.mofp_mainprog.mopg_topcal.loc_val))
          >>=
        (fun () -> return (Some (create_empty_expr (), shell_env, (hist, hist_id)))))
    | None ->
        let expr = insert_newline cur_expr in
        print_expr term expr >>=
        (fun () -> return (Some (create_empty_expr (), shell_env, (hist, hist_id))))
  ) with 
       | TypeError msg 
       | SyntaxError msg ->
              print_err term cur_expr (sprintf "error found :%s\n" msg.loc_val )
              >>=
              (fun () -> return (Some (create_empty_expr (), shell_env, (hist, hist_id))))
       | InternalError msg 
       | Sys_error msg 
       | VarError msg ->
              print_err term cur_expr (sprintf "error found :%s\n" msg )
              >>=
              (fun () -> return (Some (create_empty_expr (), shell_env, (hist, hist_id))))
       | ParseError (fname, line_start, line_end, col_start, col_end , tok,reason) ->
             print_expr term cur_expr >>=
             (fun () -> 
               let err_msg = string_of_ParseError (fname, 
                                                   line_start, 
                                                   line_end, 
                                                   col_start,
                                                   col_end,
                                                   tok, reason) 
               in
               print_err term cur_expr err_msg >>=
               (fun () ->  return (Some (create_empty_expr (), shell_env, (hist, hist_id))))
              )


let new_line_search_mod term shell_env (hist, hist_id) cur_expr search_expr = 
  clear_search_mod term shell_env (hist, hist_id) cur_expr search_expr

let new_line term shell_env (hist, hist_id) cur_expr = 
  match !term_search_mod with
    | None -> new_line_normal_mod term shell_env (hist, hist_id) cur_expr
    | Some search_expr -> 
        new_line_search_mod term shell_env (hist, hist_id) cur_expr search_expr >>=
        (fun () -> return (Some (cur_expr, shell_env, (hist, hist_id))))


let delete term shell_env hist cur_expr =
  match (!term_search_mod) with
    | None -> 
        clear_expr term cur_expr >>=
        (fun () -> 
        let expr = delete_in_expr cur_expr in
        analyse_and_print term expr >>=
        (fun () -> return (Some (expr, shell_env, hist)))
        )
    | Some search_expr ->
        let search_expr = 
          try (Zed_utf8.sub  search_expr 0 (Zed_utf8.length search_expr -1 )) 
          with Zed_utf8.Out_of_bounds  -> search_expr
        in 
        let new_expr = find_hist_expr cur_expr hist search_expr in
        term_search_mod := Some search_expr;
        print_search term shell_env hist cur_expr search_expr new_expr >>=
        (fun () -> return (Some (new_expr, shell_env, hist)))


let mv_left term shell_env hist cur_expr = 
  clear_expr term cur_expr >>=
  (fun () -> 
  let expr =  Zed_edit.prev_char cur_expr; cur_expr in
(*   print_expr term expr >>= *)
  analyse_and_print term expr >>=
  (fun () -> return (Some (expr, shell_env, hist)))
  )

let mv_right term shell_env hist cur_expr = 
  clear_expr term cur_expr >>=
  (fun () -> 
  let expr =  Zed_edit.next_char cur_expr; cur_expr in
  analyse_and_print term expr >>=
  (fun () -> return (Some (expr, shell_env, hist)))
  )

let mv_down term shell_env (hist, hist_i) cur_expr = 
  match (!term_search_mod) with 
    | None ->
      let hist_i = match hist_i with
        | 0 -> 0 
        | i -> i - 1 
      in
      let hist_lst = (LTerm_history.contents hist) in
      let new_expr, hist_i = 
        try (
          match hist_i with 
            | 0 -> create_empty_expr () , hist_i
            | _ -> utf8_to_expr (List.nth hist_lst (hist_i - 1)) , hist_i
        ) with _ -> cur_expr, hist_i + 1
      in
      clear_expr term cur_expr >>=
        (fun () ->
          analyse_and_print term new_expr) >>=
          (fun () -> return (Some (new_expr, shell_env, (hist, hist_i ))))
    | Some search_expr -> 
        let hist_i = 
          match (hist_i - 1) with
            | i when i < 0 -> 0
            | i -> i
        in
        let new_expr = find_hist_expr cur_expr (hist, hist_i) search_expr in
        print_search term shell_env (hist, hist_i) cur_expr search_expr new_expr >>=
      (fun () -> return (Some (new_expr, shell_env, (hist, hist_i ))))

    

let mv_up term shell_env (hist, hist_i) cur_expr = 
  match (!term_search_mod) with 
    | None ->
        let hist_lst = (LTerm_history.contents hist) in
        let new_expr, hist_i = 
          try (
            utf8_to_expr (List.nth hist_lst hist_i ), hist_i + 1
          ) with _ -> 
            (match hist_lst with 
              | [] -> cur_expr, hist_i
              | lst -> utf8_to_expr (List.hd (List.rev (lst))), hist_i
            )
        in
        clear_expr term cur_expr >>=
        (fun () -> analyse_and_print term new_expr) >>=
        (fun () -> return (Some (new_expr, shell_env, (hist, hist_i ))))
    | Some search_expr ->
        let hist_i = hist_i + 1 in 
        let new_expr = find_hist_expr cur_expr (hist, hist_i) search_expr in
        print_search term shell_env (hist, hist_i) cur_expr search_expr new_expr >>=
      (fun () -> return (Some (new_expr, shell_env, (hist, hist_i ))))


(*The completion system: always related to the cur_expr for which the cursor is
at the end.*)
let completion term shell_env hist cur_expr = 

  (*return a new expr witch is cur_expr but with the word at position cur_pos
  replaced by the word better_word.*)
  let improve_expr cur_expr cur_pos cur_word better_word =
    Zed_edit.remove_prev cur_expr (String.length cur_word);
    Zed_edit.insert cur_expr (str_to_zed_rope better_word);
    cur_expr
  in 
  (*we first want to have a 1 line representation of the expression.*)
  let one_line_expr, pos_in_one_line_expr = to_one_line_expr cur_expr in
  let cur_word, cur_pos = get_current_word cur_expr in
  let expr_type, _str_start = 
    GufoCompletion.analyser one_line_expr pos_in_one_line_expr cur_word 
  in
  let posibilities, better_word = 
    GufoCompletion.completion shell_env !fulloprog expr_type cur_word 
  in
  (*we improve the expression with better_word *)
  let cur_expr = improve_expr cur_expr cur_pos cur_word better_word in 
  clear_res_err_and_comple term cur_expr >>=  
    (fun () -> analyse_and_print term cur_expr ) >>=
    (fun () -> 
    print_completion term shell_env hist cur_expr posibilities) >>=
    (fun () -> Lwt.return (Some (cur_expr, shell_env, hist)))


let handle_key_event term shell_env hist cur_expr akey = 
  try
    (match akey.LTerm_key.code with
     | Char uchar when ((UChar.uint_code uchar) = 0x0068  && akey.LTerm_key.control) 
    (* CTRL + H
     * It looks some conventions allows CTRL-H to del char, and some GUI Terms
     * remap backspace to CTRL-H...
     * https://github.com/diml/lambda-term/issues/57
     * *)
        -> delete term shell_env hist cur_expr 
    | Char uchar when ((UChar.uint_code uchar) = 0x0020) && 
          (akey.LTerm_key.control)
          -> (*CTRL + SPACE : enable multiline mode.*)
        (multiline_expr term shell_env hist cur_expr)
    | Char uchar when ((UChar.uint_code uchar) = 0x0072) &&  (*CTRL-R*)
          (akey.LTerm_key.control)
          ->
        (search_hist term shell_env hist cur_expr)
    | Backspace  
    | Delete  -> delete term shell_env hist cur_expr 
    | Char uchar -> print_char term shell_env hist cur_expr akey
    | Enter -> 
        (new_line term shell_env hist cur_expr)
    | Up -> mv_up term shell_env hist cur_expr 
(*     | Up -> do_nothing term shell_env hist cur_expr *)
    | Down  -> mv_down term shell_env hist cur_expr 
(*     | Down  -> do_nothing term shell_env hist cur_expr *)
    | Left  -> mv_left term shell_env hist cur_expr
    | Right -> mv_right term shell_env hist cur_expr
    | Escape -> return None
    | Tab -> completion term shell_env hist cur_expr
    | F1 
    | F2
    | F3
    | F4
    | F5
    | F6
    | F7
    | F8
    | F9
    | F10
    | F11
    | F12 
    | Next_page
    | Prev_page
    | Home
    | End
    | Insert -> do_nothing term shell_env hist cur_expr
  )
  with exc -> do_fail term shell_env hist cur_expr exc


let rec loop term shell_env history tmod cur_expr =
  (if ((is_empty_expr cur_expr) && ((!term_search_mod) == None))
   then print_prefix term  
   else return ()
  )
  >>=
  (fun () -> 
  Lwt.catch (fun () -> (LTerm.read_event term)
    >|= fun event -> Some event
  )
    (function
      | Sys.Break -> return None
      | exn -> Lwt.fail exn)
  >>= function
  | Some Key akey ->
      (** A key has been pressed. *)
          handle_key_event term shell_env history cur_expr akey
          >>= fun res ->
            (match res with 
              | Some (expr,shell_env, hist) -> loop term shell_env hist tmod expr
              | None -> Lwt.fail (ExitTerm (term,tmod))
              )
  | Some Resize _ ->
          loop term shell_env history tmod cur_expr
      (** The terminal has been resized. *)
  | Some _ ->
          loop term shell_env history tmod cur_expr
      (** A mouse button has been pressed. *)
  | None -> 
          loop term shell_env history tmod cur_expr
 )


let main () =
  LTerm_inputrc.load ()
  >>= fun () ->
  Lwt.catch
    (fun () ->
       Lazy.force LTerm.stdout
       >>= fun term ->
       size := LTerm.size term ;
       let tmod = (LTerm.enter_raw_mode term >>=
         (fun tmod -> 
            term_rawmod := Some tmod; return tmod) ) in
       let cur_expr = create_empty_expr () in 
       let display_size = (sprintf "Term size: rows:%d col:%d\n" !size.rows !size.cols ) in
       LTerm.fprints term (eval [S "Welcome, \n";
(*                                  S "Gufo is the Unidentified Flying Ofug.\n";   *)
(*                                  S "Ofug is the Flying Unidentified Gufo.\n";   *)
                                 S "Gufo is an Unidentified Flying Object but it also means 'Owl' in esperanto. \n\n";  
                                 S "This program is GPLV3, created by Pierre Vittet. \n"; 
                                 S "This is a pre-release experimental version.\n\n";
                                 S "To escape GUFO, just press escape.\n";
                                 S display_size; 
                                 S "----------------------------------\n";
        ]) 
       >>=
        (fun () -> 
          let shell_env = Gufo.MCore.get_env (Sys.getcwd ()) in
          (*A dummy first line : it allows to create initial program with
          initial modules (for completion*)
          first_line term shell_env (LTerm_history.create [], 0) cur_expr >>=
          (fun res -> 
            (match res with 
              | Some (expr,shell_env, hist) -> loop term shell_env hist tmod expr
              | None -> Lwt.fail (ExitTerm (term,tmod))
            )
          )
        )
    )
    (function
      | ExitTerm (term,tmod) -> Lwt.return (term,tmod)
      | exn -> Lwt.fail exn)
    >>= fun (term, tmod) -> 
          tmod >>= fun tmod ->LTerm.leave_raw_mode term tmod 

let run () =  Lwt_main.run (main ())
