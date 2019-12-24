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

let maptypes = IntMap.empty

let is_in args scope =  
  match args with 
    |  [MOSimple_val (MOMap_val(mmap)); key_val ] ->
        MOSimple_val (MOBase_val (MOTypeBoolVal (MMap.mem (core_to_simple_val key_val) mmap)))
    | _ -> assert false 

let mwith args scope = 
  match args with
    |  [MOSimple_val (MOMap_val(mmap)); MOSimple_val (MOMap_val(mmap2)) ] ->
      MOSimple_val (MOMap_val (MMap.union (fun _k _a b -> (Some b)) mmap mmap2))
    | _ -> assert false 

let add args scope = 
  match args with
    |  [MOSimple_val (MOMap_val(mmap)); kval; vval ] ->
        MOSimple_val (MOMap_val (MMap.add (core_to_simple_val kval) vval mmap))
    | _ -> assert false 

let rm args scope = 
  match args with
    |  [MOSimple_val (MOMap_val(mmap)); kval ] ->
        MOSimple_val (MOMap_val (MMap.remove (core_to_simple_val kval) mmap))
    | _ -> assert false 

let topvars = 
  [
    {
      mosmv_name = "is_in";
      mosmv_description = "Return true if the element is in the map. Else, return false.";
      mosmv_intname = 1;
      mosmv_type = 
        MOFun_type
        ([ MOMap_type( MOAll_type (-20), MOAll_type (-21) );
           MOAll_type (-20) 
        ], MOBase_type(MTypeBool))
        
        ;
      mosmv_action= is_in;
    };
    {
      mosmv_name = "union";
      mosmv_description = "Do the union of two maps. In case key already
                           exists, the value of the second map is used.";
      mosmv_intname = 2;
      mosmv_type = 
        MOFun_type
        ([ MOMap_type( MOAll_type (-20), MOAll_type (-21) );
           MOMap_type( MOAll_type (-20), MOAll_type (-21) )
        ], 
           MOMap_type( MOAll_type (-20), MOAll_type (-21) ))
        ;
      mosmv_action= mwith;
    };
    {
      mosmv_name = "add";
      mosmv_description = "Add a (key, value) to a map.";
      mosmv_intname = 3;
      mosmv_type = 
        MOFun_type
        ([ 
          MOMap_type( MOAll_type (-20), MOAll_type (-21) );
          MOAll_type (-20); 
          MOAll_type (-21);
        ], 
          MOMap_type( MOAll_type (-20), MOAll_type (-21) ))
        ;
      mosmv_action= add;
    };
    {
      mosmv_name = "rm";
      mosmv_description = "remove a key and its associated value from the map.";
      mosmv_intname = 4;
      mosmv_type = 
        MOFun_type
        ([ 
          MOMap_type( MOAll_type (-20), MOAll_type (-21) );
          MOAll_type (-20); 
        ], 
          MOMap_type( MOAll_type (-20), MOAll_type (-21) ))
        ;
      mosmv_action= rm;
    };
  ]

let mosysmodule =
{
  mosm_name= "Map";
  mosm_types = maptypes;
  mosm_typstr2int= StringMap.empty;
  mosm_typstrfield2int= StringMap.empty;
  mosm_typstrfield2inttype = StringMap.empty;
  mosm_typfield2inttype = IntMap.empty;
  mosm_topvar= List.fold_left (fun acc vars -> IntMap.add vars.mosmv_intname vars acc) IntMap.empty topvars;
  mosm_varstr2int= List.fold_left (fun acc vars -> StringMap.add vars.mosmv_name vars.mosmv_intname acc) StringMap.empty topvars;
}
