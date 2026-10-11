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

let listtypes = IntMap.empty

(*The code of iter and filter is directly in GufoEngine, because modules cannot
directly call GufoEngine to avoid circular dependancies.*)
let iter args scope = assert false 
let filter args scope = assert false
let map args scope = assert false
let fold_left args scope = assert false

let length args scope =
  match args with
    | [MOSimple_val (MOList_val mtvlist);] ->
      MOSimple_val (MOBase_val (MOTypeIntVal (List.length mtvlist)))
    | _ -> assert false






let topvars = 
  [
    {
      mosmv_name = "iter";
      mosmv_description = "Iterate over the elements of the list.";
      mosmv_intname = 2;
      mosmv_type = 
        MOFun_type
        ([ MOFun_type([MOAll_type 1], MOUnit_type) ; 
          MOList_type( MOAll_type 1 )
        ], MOUnit_type)
        
        ;
      mosmv_action= iter;
    };
    {
      mosmv_name = "length";
      mosmv_description = "Return the size of the given list.";
      mosmv_intname = 1;
      mosmv_type = 
        MOFun_type
        ([ MOList_type( MOAll_type 1 )], MOBase_type (MTypeInt))
        
        ;
      mosmv_action= length;
    };

    {
      mosmv_name = "filter";
      mosmv_description = "Return a new list with only the element of the list
                          respecting the predicate.";
      mosmv_intname = 3;
      mosmv_type = 
        MOFun_type
        ([MOFun_type ([MOAll_type 1], MOBase_type (MTypeBool));
          MOList_type( MOAll_type 1 );
         ], 
         MOList_type( MOAll_type 1 ))
        ;
      mosmv_action= filter;
    };
    {
      mosmv_name = "map";
      mosmv_description = "Apply the given function to elements of the list.";
      mosmv_intname = 4;
      mosmv_type = 
        MOFun_type
        ([MOFun_type ([MOAll_type 1], MOAll_type 2 );
          MOList_type( MOAll_type 1 );
         ], 
         MOList_type( MOAll_type 2 ))
        ;
      mosmv_action= map;
    };
    {
      mosmv_name = "fold_left";
      mosmv_description = "";
      mosmv_intname = 4;
      mosmv_type = 
        MOFun_type
        ([ 
           MOFun_type ([MOAll_type 2; MOAll_type 1], MOAll_type 2 );
           MOAll_type 2 ;
           MOList_type( MOAll_type 1 );
         ], 
         MOAll_type 2 )
        ;
      mosmv_action= fold_left;
    };

  ]

let mosysmodule =
{
  mosm_name= "List";
  mosm_types = listtypes;
  mosm_typstr2int= StringMap.empty;
  mosm_typstrfield2int= StringMap.empty;
  mosm_typstrfield2inttype = StringMap.empty;
  mosm_typfield2inttype = IntMap.empty;
  mosm_topvar= List.fold_left (fun acc vars -> IntMap.add vars.mosmv_intname vars acc) IntMap.empty topvars;
  mosm_varstr2int= List.fold_left (fun acc vars -> StringMap.add vars.mosmv_name vars.mosmv_intname acc) StringMap.empty topvars;
}
