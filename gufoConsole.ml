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

open GufoParsed
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

exception ExitTerm of (LTerm.t * LTerm.mode Lwt.t)


let term_tmod = ref None

(* COLORS *)
(* foreground color *)

(*
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


(* background color *)
let c_option = blue

(* END COLORS *)

(*EXPR*)
(*expression manipulation: an expression is made from a zed_edit context *)

let count_lines_str str = 
  Zed_utf8.fold 
    (fun achar acc -> 
      match UChar.uint_code achar with
        | 0x000A (* newline *) -> acc + 1
        | _ -> acc
    )
    str 1 

let create_empty_expr () = 
  let zedit = Zed_edit.create ?editable:(Some (fun _ _ -> true)) () in
  let zcursor = Zed_edit.new_cursor zedit in
  Zed_edit.context zedit zcursor

let get_edit_ expr = Zed_edit.edit expr

let get_line_length expr line_num = Zed_rope.length (Zed_edit.get_line (get_edit_ expr) line_num)


let get_cursor_coord expr = Zed_cursor.get_coordinates (Zed_edit.cursor expr) 

let get_cursor_position expr = Zed_edit.line expr,Zed_edit.column expr

let get_lines_count expr = Zed_lines.count (Zed_edit.lines (get_edit_ expr) )

let insert_in_expr expr achar = Zed_edit.insert expr (Zed_rope.make 1 achar); expr

let delete_in_expr expr = Zed_edit.remove_prev expr 1; expr

let insert_newline expr = Zed_edit.newline expr; expr

let count_lines expr = (Zed_lines.count (Zed_edit.lines (get_edit_ expr))) + 1



(* error printing *)

let nb_lines_error = ref 0

let clear_res_err term cur_expr = 
  match (!nb_lines_error) with
    | 0 -> return ()
    | nb_error_line ->
        let expr_cursor_row, expr_cursor_col = get_cursor_position cur_expr in
        let expr_lines_count = get_lines_count cur_expr in
        LTerm.move term (expr_lines_count - expr_cursor_row + 1) (-500) >>=
        (fun () -> nb_lines_error:= 0; LTerm.clear_line term) (*TODO : clear as much line as in nb_lines_error and at the end go up by nb_lines_error instead of 1 *)
        >>=
        (fun () -> LTerm.move term 0 (-500))
        >>=
        (fun () -> LTerm.move term (expr_cursor_row - expr_lines_count - 1 ) expr_cursor_col)

let print_res_err term cur_expr str_err = 
  let nb_error_line = count_lines_str str_err in
  let expr_cursor_row, expr_cursor_col = get_cursor_position cur_expr in
  let expr_lines_count = get_lines_count cur_expr in

  LTerm.fprints term (eval [B_fg c_error; S "\n"]) >>=
  (fun () -> LTerm.move term (-1) (0)) >>=
  (fun () -> nb_lines_error:= nb_error_line;
    LTerm.move term (expr_lines_count - expr_cursor_row + 1) (-500)) >>=
  (fun () -> 
  LTerm.fprints term (eval [B_fg c_error; R (Zed_rope.of_string (str_err))]))
  >>=
  (fun () -> LTerm.move term 0 (-500)) >>=
  (fun () -> LTerm.move term (expr_cursor_row - expr_lines_count - nb_error_line  ) expr_cursor_col )

let clear_expr term expr = 
  clear_res_err term expr >>=
  (fun () -> 
    let nb_lines = count_lines expr in
    let curs_row, curs_col = get_cursor_coord expr in
    let rec del_until nb_to_del = 
      match nb_to_del with
        | 0 -> 
              LTerm.move term 1 0 
        | nb_to_del ->
            LTerm.clear_line term >>= 
              (fun () -> LTerm.move term (-1) 0 )  >>=
              (fun () -> del_until (nb_to_del - 1))
    in
    (*we go on the last line.*)
    LTerm.move term 0 (nb_lines - curs_row) >>=
      (fun () -> del_until nb_lines)
  )


let is_keyword word = 
  match word with 
    | "let" | "in" | "if" | "then" | "else" | "fun" | "struct" -> true 
    | _ -> false

let is_integer word = 
  Str.string_match (Str.regexp "[0-9]+") word 0

let is_float word = 
  Str.string_match (Str.regexp "[0-9]+\.[0-9]+")  word 0 

let is_bool word = 
  match word with 
    | "True" | "False" -> true 
    | _ -> false

let is_cmd word = 
  Str.string_match (Str.regexp "[a-z0-9]+")  word 0 





let print_color_expr term expr optprog types = 
  let print_type_not_found word = 
    [ S word ; B_fg c_unknown]
  in
  let print_type typ word = 
    (match typ with 
      | MOUnique_type (MOComposed_type _ ) -> 
          [ S word ; B_fg c_comp]
      | MOUnique_type (MOBase_type(   MTypeString)) -> 
          [ S word ; B_fg c_string]
      | MOUnique_type (MOBase_type( MTypeBool)) -> 
          [ S word; B_fg c_bool]
      | MOUnique_type (MOBase_type(   MTypeInt)) -> 
          [S word ; B_fg c_int]
      | MOUnique_type (MOBase_type(   MTypeFloat)) -> 
          [ S word ; B_fg c_float]
      | MOUnique_type (MOBase_type(   MTypeCmd)) ->
          [ S word; B_fg c_cmd]
      | MOUnique_type (MOFun_type _) ->
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
          try (
              match StringMap.find_opt var optprog.mofp_mainprog.mopg_topvar2int with
                | None -> 
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
                    match IntSet.for_all 
                      (fun id -> true ) ids
                    with
                      | true -> print_type b_type word
                      | false -> print_type_not_found word
                    )
                | Some id_var -> 
                    let typ = IntMap.find id_var (IntMap.find 0 types) in
                    print_type typ word
            
            
          )
          with | _ -> 
                  [ S word ; B_fg c_unknown ]
          )
      | _ ->
        (match is_keyword word, is_integer word, is_float word, is_bool word, is_cmd word with
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
      | _,true -> 
          (match UChar.uint_code new_char with
            | 0x0022  -> (* quote symbol *)
                [ S (sprintf "%s%c" curword (UChar.char_of new_char) );B_fg c_string] @ acc, "",false,false
            | _ -> 
              acc, (sprintf "%s%s" curword (UTF8.init 1 (fun _ -> new_char) )), true , false
          )
    )
  in
  LTerm.move term 0 (-5000) >>=
  (fun () -> 
  let to_print, last_str,_,_ = 
    Zed_utf8.fold colorize (Zed_rope.to_string (Zed_edit.text (Zed_edit.edit expr))) ([], "",false,false) (*list to print,curword, and a boolean to know if we are in quote*)
  in
  let to_print = 
    if String.length last_str > 0 then ((print_curword last_str) @ to_print) else to_print
  in
  let curs_row, curs_col = get_cursor_coord expr in
  let cur_row_lenght = (get_line_length expr curs_row ) in 
(*   let nb_lines = count_lines expr in *)
  LTerm.fprints term (eval (List.rev to_print))
(*   LTerm.fprints term (eval [S (sprintf "%d"  (curs_row - (get_line_length expr 0 ) ))]) *)
  >>= 
    (*TODO: handle line change*)
  (fun () -> LTerm.move term (0) (curs_col -  cur_row_lenght))
  
  ) 
  

let print_expr term expr = 
  let curs_row, curs_col = Zed_cursor.get_coordinates (Zed_edit.cursor expr)  in
  LTerm.clear_line term >>=
  (fun () -> LTerm.move term 0 (-5000)) >>=
  (fun () -> LTerm.fprints term (eval [B_fg c_unknown; R (Zed_edit.text (Zed_edit.edit expr))])) >>=
  (fun () -> LTerm.move term 0 (-5000)) >>= 
  (fun () -> LTerm.move term 0 (curs_col)) >>= 
  (fun () -> LTerm.show_cursor term)


let print_res term str_res = 
  LTerm.fprints term (eval [B_fg c_result; R (Zed_rope.of_string (str_res^"\n"))])

(*END EXPR*)


let fulloprog = ref empty_ofullprog

let check_full_expr cur_expr = 
  let str_expr = Zed_rope.to_string (Zed_edit.text (Zed_edit.edit cur_expr)) in 
  GufoStart.parse_shell str_expr

let i = ref 0
let analyse_and_print term cur_expr = 
    try (
      match check_full_expr cur_expr with 
      | Some p ->
          (try (
            i := (!i) + 1;
            let prog = GufoStart.handle_program p "Shell_prog" in
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
               | InternalError msg 
               | Sys_error msg 
               | VarError msg 
               | SyntaxError msg ->
            print_expr term cur_expr >>=
            (fun () -> print_res_err term cur_expr msg)
               | Not_found -> 
            print_expr term cur_expr >>=
            (fun () -> print_res_err term cur_expr "Not found")
        )
      | None ->
          print_expr term cur_expr
    )
    with | SyntaxError msg ->
       print_expr term cur_expr >>=
       (fun () -> print_res_err term cur_expr msg)

let do_nothing term cur_expr = Lwt.return (Some cur_expr)

let print_char term cur_expr akey = 
    clear_expr term cur_expr >>=
    (fun () ->
    let expr = 
      match akey.code with 
        | Char i  ->insert_in_expr cur_expr i
        | _ -> assert false
    in
      analyse_and_print term cur_expr >>= 
        (fun () -> return (Some expr))
    )

let multiline_expr term cur_expr = 
  clear_expr term cur_expr >>= 
  (fun () ->
    let expr = insert_newline cur_expr in 
    analyse_and_print term cur_expr >>= 
      (fun () -> return (Some expr))
  )

let new_line term cur_expr = 
  (*try to retrieve the current line*)
  try (
  match check_full_expr cur_expr with 
    | Some p ->
        let prog = GufoStart.handle_program p "Shell_prog" in
        let opt_prog, types = 
        match  Gufo.MCore.is_empty_ofullprog (!fulloprog) with
          | true -> GufoParsedToOpt.parsedToOpt prog
          | false -> GufoParsedToOpt.add_prog_to_optprog !fulloprog prog
        in
        fulloprog := opt_prog;
        print_color_expr term cur_expr opt_prog types >>=
        (fun () -> LTerm.fprints term (eval [B_fg c_unknown; S "\n"])) >>=
        (fun () -> LTerm.flush term) >>=
        (fun () ->
          Zed_edit.goto_eot cur_expr; 
          let tmod = 
            match (!term_tmod) with 
            | None -> assert false
            | Some tmod -> tmod
          in
          LTerm.leave_raw_mode term tmod) >>=
        (fun () -> 
          let moval = (GufoEngine.exec opt_prog) in
          ( LTerm.enter_raw_mode term >>= (fun tmod -> term_tmod:=Some tmod; Lwt.return () ));
          print_res term (Gufo.MCore.moval_to_string moval))
          >>=
        (fun () -> return (Some (create_empty_expr ())))
    | None ->
        let expr = insert_newline cur_expr in
        print_expr term expr >>=
        (fun () -> return (Some (create_empty_expr ())))
  ) with | TypeError msg 
       | InternalError msg 
       | Sys_error msg 
       | VarError msg 
       | SyntaxError msg ->
              print_res_err term cur_expr msg >>=
              (fun () -> return (Some (create_empty_expr ())))

let delete term cur_expr =
  clear_expr term cur_expr >>=
  (fun () -> 
  let expr = delete_in_expr cur_expr in
  analyse_and_print term expr >>=
  (fun () -> return (Some expr))
  )

let mv_left term cur_expr = 
  clear_expr term cur_expr >>=
  (fun () -> 
  let expr =  Zed_edit.prev_char cur_expr; cur_expr in
(*   print_expr term expr >>= *)
  analyse_and_print term expr >>=
  (fun () -> return (Some expr))
  )

let mv_right term cur_expr = 
  clear_expr term cur_expr >>=
  (fun () -> 
  let expr =  Zed_edit.next_char cur_expr; cur_expr in
  analyse_and_print term expr >>=
  (fun () -> return (Some expr))
  )

let mv_down term cur_expr = 
  clear_expr term cur_expr >>=
  (fun () -> 
  print_expr term cur_expr >>=
  (fun () -> return (Some cur_expr))
  )

let mv_up term cur_expr = 
  clear_expr term cur_expr >>=
  (fun () -> 
  print_expr term cur_expr >>=
  (fun () -> return (Some cur_expr))
  )


let handle_key_event term cur_expr akey = 
  match akey.LTerm_key.code with
(*   | Char uchar when (UChar.uint_code uchar) = 0x0068   *)
  (*Ugly hack, I don't know why this key match backspace. 
   * I have posted on lambda-term about the issue:
   * https://github.com/diml/lambda-term/issues/57
   *
   * *)
(*       -> delete term cur_expr  *)
  | Char uchar when ((UChar.uint_code uchar) = 0x0020) && 
        (akey.LTerm_key.control)
        ->
      (multiline_expr term cur_expr)
  | Backspace  
  | Delete  -> delete term cur_expr 
  | Char uchar -> print_char term cur_expr akey
  | Enter -> 
      (new_line term cur_expr)
(*   | Up -> mv_up term cur_expr *)
  | Up -> do_nothing term cur_expr
(*   | Down  -> mv_down term cur_expr *)
  | Down  -> do_nothing term cur_expr
  | Left  -> mv_left term cur_expr
  | Right -> mv_right term cur_expr
  | Escape -> return None
  | Tab 
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
  | Insert -> do_nothing term cur_expr


let rec loop term history tmod cur_expr =
  Lwt.catch (fun () -> (LTerm.read_event term)
    >|= fun event -> Some event
  )
    (function
      | Sys.Break -> return None
      | exn -> Lwt.fail exn)
  >>= function
  | Some Key akey ->
      (** A key has been pressed. *)
          handle_key_event term cur_expr akey
          >>= fun res ->
            (match res with 
              | Some expr -> loop term history tmod expr
              | None -> Lwt.fail (ExitTerm (term,tmod))
              )
  | Some Resize _ ->
          loop term history tmod cur_expr
      (** The terminal has been resized. *)
  | Some _ ->
          loop term history tmod cur_expr
      (** A mouse button has been pressed. *)
  | None -> 
          loop term history tmod cur_expr
 

let main () =
  LTerm_inputrc.load ()
  >>= fun () ->
  Lwt.catch
    (fun () ->
       Lazy.force LTerm.stdout
       >>= fun term ->
       let tmod = (LTerm.enter_raw_mode term >>=
         (fun tmod -> term_tmod := Some tmod; return tmod) ) in
       let cur_expr = create_empty_expr () in 
       LTerm.fprints term (eval [S "Welcome, \n";
(*                                  S "Gufo is the Unidentified Flying Ofug.\n";  *)
                                 S "Gufo is an Unidentified Flying Object but it also means 'Owl' in esperanto. \n\n"; 
                                 S "This program is GPLV3, created by Pierre Vittet. \n"; 
                                 S "This is a pre-release experimental version.\n\n";
                                 S "To escape GUFO, just press escape.\n";
                                 S "----------------------------------\n";
        ]) 
       >>=
        (fun () -> loop term (LTerm_history.create []) tmod cur_expr)
    )
    (function
      | ExitTerm (term,tmod) -> Lwt.return (term,tmod)
      | exn -> Lwt.fail exn)
    >>= fun (term, tmod) -> 
          tmod >>= fun tmod ->LTerm.leave_raw_mode term tmod 

let run () =  Lwt_main.run (main ())
