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

(* System module handling. *)

open GenUtils
open Gufo.MCore
open GufoParsed
open Format

let is_system_module filename =
  match filename with
    | "list.ma" -> true
    | "cmd.ma" -> true
    | "string.ma" -> true
    | "set.ma" -> true
    | _ -> false

let parse_system_module filename = 
  match filename with
    | "list.ma" -> GufoListMod.mosysmodule
    | "cmd.ma" -> GufoCmdMod.mosysmodule
    | "string.ma" -> GufoStringMod.mosysmodule
    | "set.ma" -> GufoSetMod.mosysmodule
    | _ -> assert false


let get_intname_from_modulestr modstr fulloprog = 
  StringMap.find modstr fulloprog.mofp_progmap 

let get_module_prog_from_modulestr modstr fulloprog = 
  let iname = get_intname_from_modulestr modstr fulloprog in
  IntMap.find iname fulloprog.mofp_progmodules 

let get_oref_from_sysmodule ref fulloprog = 
  match ref.mrv_module with
    | None -> assert false
    | Some modul -> 
        let sysmod = parse_system_module modul in
          match ref.mrv_varname with 
            | [varname] ->
              let intname = StringMap.find varname sysmod.mosm_typstr2int in
              Some (get_intname_from_modulestr modul fulloprog), intname
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


