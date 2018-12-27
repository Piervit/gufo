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

open Sedlexing
open Sedlex_menhir
open Printf
open GufoParsed
open Gufo_lexer
open Gufo_lexing
open GufoEngine
open GenUtils





(*
let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
*)



(*
let parse_with_error lexbuf from_shell =
  try
    (match from_shell with
      | true -> Gufo_parser.shell Gufo_lexer.read lexbuf
      | false -> Gufo_parser.prog Gufo_lexer.read lexbuf
    )
  with
  | SyntaxError msg ->
     raise (SyntaxError "bouh") 
(*     raise (SyntaxError (sprintf "%s: %s\n" (print_position lexbuf) msg)) *)
  | Gufo_parser.Error ->
     raise (SyntaxError "bouh") 
(*     raise (SyntaxError (sprintf "%s: syntax error\n" (print_position lexbuf))) *)

*)


(*
let rec parse_and_print lexbuf from_shell =
  match parse_with_error lexbuf from_shell with
  | Some prog ->

      debugPrint "Dumping program types\n";
      (if !GufoConfig.debug 
      then (StringMap.iter (fun str el -> dump_mtype el) prog.mpg_types)
      else ());
      debugPrint "Dumping program variables\n";
      (if !GufoConfig.debug 
      then (List.iter dump_var prog.mpg_topvar)
      else ());

      Some prog
  | None -> 
      None

*)



let parse_shell content =
  let lexbuf = create_lexbuf (Sedlexing.Utf8.from_stream (Stream.of_string content)) in
  let prog = sedlex_with_menhir Gufo_lexer.read Gufo_parser.shell lexbuf in
  prog



let module_to_filename modulename =
    String.concat "." [String.uncapitalize_ascii modulename; "ma"]

let filename_to_module filename = 
    String.sub (String.capitalize_ascii filename) 0 ((String.length filename) - 3)
   
let parse_file filename =
        let inx = open_in filename in
        let lexbuf = create_lexbuf ~file:filename (Sedlexing.Utf8.from_channel inx) in
(*         lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename }; *)
        let prog = sedlex_with_menhir Gufo_lexer.read Gufo_parser.prog lexbuf in
(*         let prog = parse_and_print lexbuf false in  *)
        close_in inx;
        prog




let handle_program p curMod = 
  let toparse= GufoParsedToOpt.search_modules p in
    StringMap.iter (fun k v -> debugPrint (sprintf "iter: %s: %d \n" k v )) toparse; 
  let toparse= StringMap.remove curMod toparse in
  let rec parse_others_modules mapmodule parsed depmodules toparse = 
    StringMap.iter (fun k v -> debugPrint (sprintf "%s: %d \n" k v )) toparse; 
    let (mapmodule,progmodules,depmodules, newtoparse) = 
      StringMap.fold 
      (fun newmodule intmod (alreadyparsed, parsed,depmodules, newmodtoparse) -> 
        let filename = module_to_filename newmodule in
        (match GufoModules.is_system_module filename with
          | true -> 
              let alreadyparsed = StringMap.add newmodule intmod alreadyparsed in
              let parsed = IntMap.add intmod (MSystemMod filename) parsed in
              let depmodules = IntMap.add intmod IntSet.empty depmodules in
              (alreadyparsed, parsed , depmodules, newmodtoparse)
          | false -> 
            let newmoduleprog = parse_file filename in
            match newmoduleprog with
            | None -> printf "No prog\n"; assert false
            | Some prog -> let depmodules_str = GufoParsedToOpt.search_modules prog in
              let alreadyparsed = StringMap.add newmodule intmod alreadyparsed in
              let parsed = IntMap.add intmod (MUserMod prog) parsed in
              let depset = StringMap.fold 
                (fun str_ i iset -> IntSet.add i iset) depmodules_str IntSet.empty in
              let depmodules = IntMap.add intmod depset depmodules in
              (*keep every new not already parsed*)
              let newtoparse = StringMap.filter (fun modname _modint -> 
                not (StringMap.mem modname alreadyparsed)) depmodules_str in
              let newmodtoparse = StringMap.union (fun i a _ -> Some a) 
                                  newmodtoparse newtoparse 
              in
              (alreadyparsed, parsed, depmodules, newmodtoparse)
        )
      )
      toparse (mapmodule, parsed, depmodules, StringMap.empty) 
    in 
    match StringMap.is_empty newtoparse with
      | true -> (mapmodule, progmodules,depmodules )
      | false ->  parse_others_modules mapmodule progmodules depmodules newtoparse
  in
  let (mapmodule, progmodules, depmodules) = 
    parse_others_modules 
      (StringMap.empty)
      (IntMap.empty)
      IntMap.empty 
      toparse
  in
  let progmap_debug = 
    StringMap.fold 
      (fun str i map -> IntMap.add i str map)
      mapmodule IntMap.empty
  in 
      {
        mfp_mainprog = p;
        mfp_progmodules = progmodules;
        mfp_module_dep = depmodules; 
        mfp_progmap = StringMap.add curMod 0 mapmodule;
        mfp_progmap_debug = IntMap.add 0 curMod progmap_debug;
        
      }



 



