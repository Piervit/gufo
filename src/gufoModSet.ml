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

let cardinal args scope =  
  match args with 
    |  [MOSimple_val (MOSet_val(mset)); ] ->
        MOSimple_val (MOBase_val (MOTypeIntVal (MSet.cardinal mset)))

    | _ -> assert false 

let is_in args scope =  
  match args with 
    |  [MOSimple_val (MOSet_val(mset)); el_val ] ->
        MOSimple_val (MOBase_val (MOTypeBoolVal (MSet.mem (core_to_simple_val el_val) mset)))

    | _ -> assert false 

let union args scope = 
  match args with 
    |  [MOSimple_val (MOSet_val(mset)); MOSimple_val (MOSet_val(mset2))] ->
        MOSimple_val (MOSet_val (MSet.union  mset mset2))
    | _ -> assert false 

let add args scope = 
  match args with
    |  [MOSimple_val (MOSet_val(mset));el_val ] ->
        MOSimple_val (MOSet_val (MSet.add (core_to_simple_val el_val) mset))
    | _ -> assert false 

let rm args scope = 
  match args with
    |  [MOSimple_val (MOSet_val(mset));el_val ] ->
        MOSimple_val (MOSet_val (MSet.remove (core_to_simple_val el_val) mset))
    | _ -> assert false 


let topvars = 
  [
    {
      mosmv_name = "cardinal";
      mosmv_description = "Return the number of elements of the set as an integer.";
      mosmv_intname = 1;
      mosmv_type = 
        MOFun_type
        ([ MOSet_type( MOAll_type (-20) );
        ], MOBase_type(MTypeInt))
        
        ;
      mosmv_action= cardinal;
    };
    {
      mosmv_name = "is_in";
      mosmv_description = "Return true if the element is in the set. Else, return false.";
      mosmv_intname = 2;
      mosmv_type = 
        MOFun_type
        ([ MOSet_type( MOAll_type (-20) );
           MOAll_type (-20) 
        ], MOBase_type(MTypeBool))
        
        ;
      mosmv_action= is_in;
    };
    {
      mosmv_name = "union";
      mosmv_description = "Do the union of two sets. ";
      mosmv_intname = 3;
      mosmv_type = 
        MOFun_type
        ([ MOSet_type( MOAll_type (-20) );
           MOSet_type( MOAll_type (-20) );
        ], 
           MOSet_type( MOAll_type (-20) ))
        ;
      mosmv_action= union;
    };
    {
      mosmv_name = "add";
      mosmv_description = "Add a specific value to a set.";
      mosmv_intname = 4;
      mosmv_type = 
        MOFun_type
        ([ MOSet_type( MOAll_type (-20) );
           MOAll_type (-20) ;
        ], 
           MOSet_type( MOAll_type (-20) ))
        ;
      mosmv_action= add;
    };
    {
      mosmv_name = "rm";
      mosmv_description = "Remove a specific value from a set.";
      mosmv_intname = 5;
      mosmv_type = 
        MOFun_type
        ([ MOSet_type( MOAll_type (-20) );
           MOAll_type (-20) ;
        ], 
           MOSet_type( MOAll_type (-20) ))
        ;
      mosmv_action= rm;
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
