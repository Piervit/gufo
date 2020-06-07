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


(*the system module for Int. *)

open Gufo.MCore
open GenUtils
open GufoParsedHelper
open GufoLocHelper

let floattypes = IntMap.empty

let toString args scope = 
  match (lst_val_only args) with
    | [MOSimple_val (MOBase_val (MOTypeFloatVal i))] ->
       box_loc (MOSimple_val (MOBase_val 
               (MOTypeStringVal ((string_of_float_loc i)))))
    | _ -> assert false

(*return a type option, some i, if the string is parsable to the floateger i, else return none.*)
let fromString args scope = 
  match (lst_val_only args) with
    |  [MOSimple_val (MOBase_val (MOTypeStringVal i)) ] ->
        (try
          box_loc (MOSimple_val (MOSome_val 
            (box_loc
            (MOSimple_val (MOBase_val (MOTypeFloatVal (
              box_loc
                (float_of_string (List.hd (String.split_on_char '\n' i.loc_val)))
          )))))
            ))
        with _ ->
          box_loc (MOSimple_val (MONone_val))
        )
    | _ -> assert false 

let topvars = 
  [
    {
      mosmv_name = "toString";
      mosmv_description = "Return the float argument as a string.";
      mosmv_intname = 1;
      mosmv_type = 
        MOFun_type
        ([ MOBase_type (MTypeFloat) ;]
        , MOBase_type (MTypeString))
        
        ;
      mosmv_action= toString;
    };
    {
      mosmv_name = "fromString";
      mosmv_description = "Inspect the first line of the string and try to
return it as an floateger: return None, it is not parsable as float, else return
'(Some i)' with i the parsed floateger.";
      mosmv_intname = 2;
      mosmv_type = 
        MOFun_type
        ([ MOBase_type (MTypeString) ;]
        , MOOption_type (MOBase_type (MTypeFloat)))
        
        ;
      mosmv_action= fromString;
    };
  ]

let mosysmodule =
{
  mosm_name= "Float";
  mosm_types = floattypes;
  mosm_typstr2int= StringMap.empty;
  mosm_typstrfield2int= StringMap.empty;
  mosm_typstrfield2inttype = StringMap.empty;
  mosm_typfield2inttype = IntMap.empty;
  mosm_topvar= List.fold_left (fun acc vars -> IntMap.add vars.mosmv_intname vars acc) IntMap.empty topvars;
  mosm_varstr2int= List.fold_left (fun acc vars -> StringMap.add vars.mosmv_name vars.mosmv_intname acc) StringMap.empty topvars;
}
