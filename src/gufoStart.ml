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
open GufoStartComp
open Gufo.MCore


let parse_shell content = GufoStartComp.parse_shell content


let add_mainprog_to_progmap curModName progmap = 
      StringMap.add curModName 0 progmap

let add_mainprog_to_progmap_debug curModName progmap_dbg = 
      IntMap.add 0 curModName progmap_dbg
   
(**
 * parse_file : filename -> parsedResult
 *
 *)
let parse_file filename = GufoStartComp.parse_file filename

let handle_program p curMod = 
  debug_info (debug_title1 "Start converting a gufo program to a full gufo program (including external modules)");
  let toparse= GufoParsedToOpt.search_modules p in
    (match GufoConfig.get_debug_level () with
      | GufoConfig.DBG_FULL -> 
          debug_print (debug_title2 "Linked modules");
          (match (StringMap.is_empty toparse) with
            | true -> debug_print "-- no external module used --\n"
            | false ->  StringMap.iter (fun k v -> debug_print (sprintf "%s: %d \n" k v )) toparse)
      | _ -> ()
    ); 
  let toparse = StringMap.remove curMod toparse in
  let rec parse_others_modules mapmodule parsed depmodules toparse = 
    let (mapmodule,progmodules,depmodules, newtoparse) = 
      StringMap.fold 
      (fun newmodule intmod (alreadyparsed, parsed,depmodules, newmodtoparse) -> 
        let filename = GufoModuleUtils.module_to_filename newmodule in
        debug_print (sprintf "gufoStart %s \n" filename) ;
        (match GufoModules.is_system_module filename with
          | true -> 
              let alreadyparsed = StringMap.add newmodule intmod alreadyparsed in
              let parsed = IntMap.add intmod (MSystemMod filename) parsed in
              let depmodules = IntMap.add intmod IntSet.empty depmodules in
              (alreadyparsed, parsed , depmodules, newmodtoparse)
          | false -> 
            let newmoduleprog =
              (match Sys.file_exists filename with
                | true ->  parse_file filename 
                | false ->  None 
              )
            in
            match newmoduleprog with
            | None -> eprintf "No program found\n"; assert false
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
      debug_info ("Gufo program converted to a full gufo program.");
  let prog = 
      {
        mfp_mainprog = p;
        mfp_progmodules = progmodules;
        mfp_module_dep = depmodules; 
        mfp_progmap = add_mainprog_to_progmap curMod mapmodule;
        mfp_progmap_debug = add_mainprog_to_progmap_debug curMod progmap_debug;
        
      }
  in dump_fullprog prog; prog

let handle_consolprog p previous_fulloptiprog = 
  debug_info (debug_title1 "Start converting a gufo console program using the info already acquired from existing program.");
  let prog = 
    match is_empty_ofullprog previous_fulloptiprog with 
      | true -> 
        (*If the previous_fulloptiprog is empty, we are on the start of our
          interactive program, and we have to init system modules. *)
        {
          mfp_mainprog = p;
          mfp_progmodules = GufoModules.get_system_modules; 
          mfp_module_dep =  GufoModules.get_system_modules_dep;
          mfp_progmap = add_mainprog_to_progmap "Shell Prog" 
                          (GufoModules.get_system_modules_progmap) ;
          mfp_progmap_debug = add_mainprog_to_progmap_debug "Shell Prog" 
                                (GufoModules.get_system_modules_progmap_debug) ;
        }
      | false -> 
        {
          mfp_mainprog = p;
          mfp_progmodules =  IntMap.empty;
          mfp_module_dep = previous_fulloptiprog.mofp_module_dep; 
          mfp_progmap = previous_fulloptiprog.mofp_progmap;
          mfp_progmap_debug = previous_fulloptiprog.mofp_progmap_debug;
        }
  in dump_fullprog prog ; prog




