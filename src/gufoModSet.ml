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


(*the system module for List. *)

open Gufo.MCore
open GenUtils
open GufoParsed

let settypes = IntMap.empty

let is_in args scope =  
  match args with 
    |  [el_val ;MOSimple_val (MOSet_val(mset)); ] ->
        MOSimple_val (MOBase_val (MOTypeBoolVal (MSet.mem (core_to_simple_mtype el_val) mset)))

    | _ -> assert false 


let topvars = 
  [
    {
      mosmv_name = "is_in";
      mosmv_description = "Return true if the element is in the set. Else, return false.";
      mosmv_intname = 1;
      mosmv_type = 
        MOUnique_type (MOFun_type
        ([ MOUnique_type (MOAll_type (-20)) ; 
          MOUnique_type (MOSet_type( MOUnique_type (MOAll_type (-20) )))
        ], MOUnique_type (MOBase_type(MTypeBool)))
        )
        ;
      mosmv_action= is_in;
    };
  ]

let mosysmodule =
{
  mosm_name= "Set";
  mosm_types = settypes;
  mosm_typstr2int= StringMap.empty;
  mosm_typstrfield2int= StringMap.empty;
  mosm_typstrfield2inttype = StringMap.empty;
  mosm_typfield2inttype = IntMap.empty;
  mosm_topvar= List.fold_left (fun acc vars -> IntMap.add vars.mosmv_intname vars acc) IntMap.empty topvars;
  mosm_varstr2int= List.fold_left (fun acc vars -> StringMap.add vars.mosmv_name vars.mosmv_intname acc) StringMap.empty topvars;
}
