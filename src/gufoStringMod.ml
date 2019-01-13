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

(*The String system module.*)

open Gufo.MCore
open GenUtils
open GufoParsed

let types = IntMap.empty

let asList args scope =  
  match args with 
    |  [MOSimple_val (MOBase_val MOTypeStringVal (str))] ->
        let res = String.split_on_char '\n' str in
        let res =
        List.map (fun str -> MOSimple_val (MOBase_val (MOTypeStringVal((String.trim str))))) res
        in MOSimple_val (MOList_val res)
    | _ -> assert false 

let topvars = 
  [
    {
      (*split a string into a list *)
      mosmv_name = "asList";
      mosmv_description = "Split each line of the given string into a list.";
      mosmv_intname = 1;
      mosmv_type = 
        MOUnique_type (MOFun_type
        ([
         MOUnique_type (MOBase_type (MTypeString)); ], 
         
         MOUnique_type (MOList_type (MOUnique_type (MOBase_type (MTypeString)))))
        )
        ;
      mosmv_action= asList;
    };
  ]

let mosysmodule =
{
  mosm_types = types;
  mosm_typstr2int= StringMap.empty;
  mosm_typstrfield2int= StringMap.empty;
  mosm_typstrfield2inttype = StringMap.empty;
  mosm_typfield2inttype = IntMap.empty;
  mosm_topvar= List.fold_left (fun acc vars -> IntMap.add vars.mosmv_intname vars acc) IntMap.empty topvars;
  mosm_varstr2int= List.fold_left (fun acc vars -> StringMap.add vars.mosmv_name vars.mosmv_intname acc) StringMap.empty topvars;
}
