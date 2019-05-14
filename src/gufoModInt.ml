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

let inttypes = IntMap.empty

let toString args scope = 
  match args with
    |  [MOSimple_val (MOBase_val (MOTypeIntVal i)) ] ->
       MOSimple_val (MOBase_val (MOTypeStringVal (string_of_int i)))
    | _ -> assert false 

(*return a type option, some i, if the string is parsable to the integer i, else return none.*)
let fromString args scope = 
  match args with
    |  [MOSimple_val (MOBase_val (MOTypeStringVal i)) ] ->
        (try
          MOSimple_val (MOSome_val 
            (MOSimple_val (MOBase_val (MOTypeIntVal 
              (int_of_string (List.hd (String.split_on_char '\n' i)))))))
        with _ ->
          MOSimple_val (MONone_val)
        )
    | _ -> assert false 

let topvars = 
  [
    {
      mosmv_name = "toString";
      mosmv_description = "Return the int argument as a string.";
      mosmv_intname = 1;
      mosmv_type = 
        MOFun_type
        ([ MOBase_type (MTypeInt) ;]
        , MOBase_type (MTypeString))
        
        ;
      mosmv_action= toString;
    };
    {
      mosmv_name = "fromString";
      mosmv_description = "Inspect the first line of the string and try to
return it as an integer: return None, it is not parsable as int, else return
'(Some i)' with i the parsed integer.";
      mosmv_intname = 2;
      mosmv_type = 
        MOFun_type
        ([ MOBase_type (MTypeString) ;]
        , MOOption_type (MOBase_type (MTypeInt)))
        
        ;
      mosmv_action= fromString;
    };
  ]

let mosysmodule =
{
  mosm_name= "Int";
  mosm_types = inttypes;
  mosm_typstr2int= StringMap.empty;
  mosm_typstrfield2int= StringMap.empty;
  mosm_typstrfield2inttype = StringMap.empty;
  mosm_typfield2inttype = IntMap.empty;
  mosm_topvar= List.fold_left (fun acc vars -> IntMap.add vars.mosmv_intname vars acc) IntMap.empty topvars;
  mosm_varstr2int= List.fold_left (fun acc vars -> StringMap.add vars.mosmv_name vars.mosmv_intname acc) StringMap.empty topvars;
}
