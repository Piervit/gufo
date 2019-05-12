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


(*the system module for Opt. *)

open Gufo.MCore
open GenUtils

let modtypes = IntMap.empty

let get args scope = 
  match args with
    |  [MOSimple_val (MOSome_val i ); defval ] -> i
    |  [MOSimple_val (MONone_val); defval ] -> defval
    | _ -> assert false 

(*TODO
let fail args scope = 
  match args with
    |  [MOSimple_val (MOSome_val i ); deffun ] -> i
    |  [MOSimple_val (MONone_val); deffun ] -> deffun 
    | _ -> assert false 
*)

let topvars = 
  [
    {
      mosmv_name = "get";
      mosmv_description = "get: optval -> defval -> val
        If optval is set to 'some i', return i, else return defval.";
      mosmv_intname = 1;
      mosmv_type = 
        MOUnique_type (MOFun_type
        ([ MOUnique_type (MOOption_type (MOUnique_type (MOAll_type 1))) ; (MOUnique_type (MOAll_type 1)) ]
        , MOUnique_type (MOAll_type 1))
        )
        ;
      mosmv_action= get;
    };
  ]

let mosysmodule =
{
  mosm_name= "Mod";
  mosm_types = modtypes;
  mosm_typstr2int= StringMap.empty;
  mosm_typstrfield2int= StringMap.empty;
  mosm_typstrfield2inttype = StringMap.empty;
  mosm_typfield2inttype = IntMap.empty;
  mosm_topvar= List.fold_left (fun acc vars -> IntMap.add vars.mosmv_intname vars acc) IntMap.empty topvars;
  mosm_varstr2int= List.fold_left (fun acc vars -> StringMap.add vars.mosmv_name vars.mosmv_intname acc) StringMap.empty topvars;
}
