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

(* Standard system module handling. *)

open GenUtils
open Gufo.MCore
open GufoParsed
open Format


exception GufoInvalidModule of string
exception GufoModuleInvalidArgument

(*The different existing systems. *)
let system_modules = ["Base"; "List"; "Cmd"; "String"; "Set";"Map"; "Int"; "Float"; "Opt"]



let is_system_module filename =
  List.exists
    (fun lstel -> (String.compare  lstel filename) == 0) 
    (List.map GufoModuleUtils.module_to_filename system_modules)

let parse_system_module filename = 
  match filename with
    | "base.gf" -> GufoModBase.mosysmodule
    | "list.gf" -> GufoModList.mosysmodule
    | "cmd.gf" -> GufoModCmd.mosysmodule
    | "string.gf" -> GufoModString.mosysmodule
    | "set.gf" -> GufoModSet.mosysmodule
    | "map.gf" -> GufoModMap.mosysmodule
    | "int.gf" -> GufoModInt.mosysmodule
    | "float.gf" -> GufoModFloat.mosysmodule
    | "opt.gf" -> GufoModOpt.mosysmodule
    | _ -> raise (GufoInvalidModule ("Cannot parse "^ filename))


let get_intname_from_modulestr modstr fulloprog = 
  try
    StringMap.find modstr fulloprog.mofp_progmap 
  with Not_found ->
    raise (GufoInvalidModule ("Cannot find intname from " ^ modstr))

let get_module_prog_from_modulestr modstr fulloprog = 
  let iname = get_intname_from_modulestr modstr fulloprog in
  try
    IntMap.find iname fulloprog.mofp_progmodules 
  with Not_found ->
    raise (GufoInvalidModule ("Cannot get module from "^ modstr))


let get_oref_from_sysmodule ref fulloprog = 
  match ref.mrv_module with
    | None -> raise GufoModuleInvalidArgument 
    | Some modul -> 
        let sysmod = parse_system_module modul in
          match ref.mrv_varname with 
            | [varname] ->
              let intname = StringMap.find varname sysmod.mosm_typstr2int in
              (get_intname_from_modulestr modul fulloprog), intname
            | _ -> assert false (*TODO...*)

let sysmodctype_to_ctype sysmodtyp = 
  {
    moct_name= sysmodtyp.mosmt_intname;
    moct_fields= List.fold_left 
                (fun map sysmodfd -> 
                  IntMap.add sysmodfd.mosmf_intname
                  {
                    motf_name = sysmodfd.mosmf_intname;
                    motf_type = sysmodfd.mosmf_type;
                    motf_debugname = sysmodfd.mosmf_name;
                  }
                  map
                ) 
                IntMap.empty sysmodtyp.mosmt_fields ;
    moct_internal_val = sysmodtyp.mosmt_internal_val; 
    moct_debugname = sysmodtyp.mosmt_name;
  }

let get_types_map modu = 
  IntMap.fold
    (fun k el map_toptypes  ->
      IntMap.add el.mosmv_intname el.mosmv_type map_toptypes
    ) 
  modu.mosm_topvar
  IntMap.empty

let write_type stream typ = 
  let stream = Printf.sprintf "%s\n type %s:" stream typ.mosmt_name in
  let stream = 
    List.fold_left 
      (fun stream field -> 
        Printf.sprintf "%s\n %s : %s" stream field.mosmf_name (type_to_string field.mosmf_type)
      ) 
      stream typ.mosmt_fields 
  in
  stream

  
  (*This part should be rewritten. It is only basic draft, never has been called. *)
(*let generate_stdlib_doc () = 
  List.iter
    (fun modul -> 
      let mosysmodule = parse_system_module modul in
      let file_content = "" in
      let file_content = GufoDocWriter.write_main_title file_content ("Module " ^ mosysmodule.mosm_name) in
      let file_content = 
      IntMap.fold
        (fun intname typ file_content -> 
          let file_content =  write_type file_content typ in
          file_content 
          (*TODO: more to add *)
        )
        mosysmodule.mosm_types file_content
      in 
(*         file_content *)
      ()
      (*TODO*)
    )
    system_modules
*)

let get_system_modules = 
  let _ , sysmap = 
    List.fold_left
      (fun (i,map) el -> (i+1, IntMap.add i (MSystemMod (el)) map) )
      (1, IntMap.empty) (List.map GufoModuleUtils.module_to_filename system_modules)
  in 
    sysmap

let get_system_modules_dep = 
   let _ , sysmap = 
    List.fold_left
      (fun (i,map) _el -> (i+1, IntMap.add i IntSet.empty map) )
      (1, IntMap.empty) system_modules
  in 
    sysmap

 let get_system_modules_progmap = 
   let _ , sysmap = 
    List.fold_left
      (fun (i,map) el -> (i+1, StringMap.add el i map) )
      (1, StringMap.empty) system_modules
  in 
    sysmap

 let get_system_modules_progmap_debug = 
   let _ , sysmap = 
    List.fold_left
      (fun (i,map) el -> (i+1, IntMap.add i el map) )
      (1, IntMap.empty) system_modules
  in 
    sysmap


