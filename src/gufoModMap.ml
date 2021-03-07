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
open GufoParsedHelper
open GufoLocHelper

let maptypes = IntMap.empty

let cardinal args scope =  
  match (lst_val_only args) with 
    |  [MOSimple_val (MOMap_val(mmap)) ] ->
        box_loc (MOSimple_val (MOBase_val (MOTypeIntVal (box_loc (MMap.cardinal mmap.loc_val)))))
    | _ -> assert false 


let is_in args scope = 
  match (args) with
    |  [mmap; kval] ->
      (match mmap.loc_val with
        | MOSimple_val (MOMap_val(mmap)) -> 
        box_loc(MOSimple_val (MOBase_val (MOTypeBoolVal 
          (box_loc(
            MMap.mem ((core_to_simple_val kval)) mmap.loc_val)))
        ))
        | _ -> assert false
      )
    | _ -> assert false 

let mwith args scope = 
  match (lst_val_only args) with
    |  [MOSimple_val (MOMap_val(mmap)); MOSimple_val (MOMap_val(mmap2)) ] ->
      box_loc (MOSimple_val 
                (MOMap_val (box_loc (MMap.union (fun _k _a b -> (Some b)) 
                  mmap.loc_val 
                  mmap2.loc_val))))
    | _ -> assert false 

let get args scope = 
  match (args) with
    |  [mmap; kval ] ->
      (match mmap.loc_val with
        | MOSimple_val (MOMap_val(mmap)) -> 
          (match (MMap.find_opt ((core_to_simple_val kval)) mmap.loc_val) with
            | None -> box_loc (MOSimple_val (MONone_val))
            | Some v -> box_loc (MOSimple_val (MOSome_val v))
          )
        | _ -> assert false
      )
    | _ -> assert false 


let add args scope = 
  match (args) with
    |  [mmap;kval; vval ] ->
      (match mmap.loc_val with
        | MOSimple_val (MOMap_val(mmap)) -> 
          box_loc (MOSimple_val 
                    (MOMap_val (box_loc (MMap.add (core_to_simple_val kval) vval mmap.loc_val))))
          
        | _ -> assert false
      )
    | _ -> assert false

let rm args scope = 
  match (args) with
    |  [mmap;kval ] ->
      (match mmap.loc_val with
        | MOSimple_val (MOMap_val(mmap)) -> 
          box_loc (MOSimple_val (MOMap_val (box_loc 
                    (MMap.remove (core_to_simple_val kval) mmap.loc_val))))
          
        | _ -> assert false
      )
    | _ -> assert false


let topvars = 
  [
    {
      mosmv_name = "cardinal";
      mosmv_description = "Return the number of elements in the map.";
      mosmv_intname = 1;
      mosmv_type = 
        motypeCoreFunToMoType
          (MOCFun_type
          ([ MOCMap_type( MOCAll_type (-20), MOCAll_type (-21) );
          ], MOCBase_type(MTypeInt))
          )
          "Map.cardinal"
        ;
      mosmv_action= cardinal;
    };

    {
      mosmv_name = "is_in";
      mosmv_description = "Return true if the element is in the map. Else, return false.";
      mosmv_intname = 2;
      mosmv_type = 
        motypeCoreFunToMoType
          (MOCFun_type
          ([ MOCMap_type( MOCAll_type (-20), MOCAll_type (-21) );
             MOCAll_type (-20) 
          ], MOCBase_type(MTypeBool))
          )
          "Map.is_in"
        ;
      mosmv_action= is_in;
    };
    {
      mosmv_name = "get";
      mosmv_description = "Get the element of the map pointed by the key.";
      mosmv_intname = 3;
      mosmv_type = 
        motypeCoreFunToMoType
          (MOCFun_type
          ([ MOCMap_type( MOCAll_type (-20), MOCAll_type (-21) );
             MOCAll_type (-20) 
          ], 
             MOCOption_type (MOCAll_type (-21)))
          )
          "Map.get"
        ;
      mosmv_action= get;
    };
    {
      mosmv_name = "union";
      mosmv_description = "Do the union of two maps. In case key already
                           exists, the value of the second map is used.";
      mosmv_intname = 4;
      mosmv_type = 
        motypeCoreFunToMoType
          (MOCFun_type
          ([ MOCMap_type( MOCAll_type (-20), MOCAll_type (-21) );
             MOCMap_type( MOCAll_type (-20), MOCAll_type (-21) )
          ], 
             MOCMap_type( MOCAll_type (-20), MOCAll_type (-21) ))
          )
        "Map.union"
        ;
      mosmv_action= mwith;
    };
    {
      mosmv_name = "add";
      mosmv_description = "Add a (key, value) to a map.";
      mosmv_intname = 5;
      mosmv_type = 
        motypeCoreFunToMoType
          (MOCFun_type
          ([ 
            MOCMap_type( MOCAll_type (-20), MOCAll_type (-21) );
            MOCAll_type (-20); 
            MOCAll_type (-21);
          ], 
            MOCMap_type( MOCAll_type (-20), MOCAll_type (-21) ))
          )
          "Map.add"
        ;
      mosmv_action= add;
    };
    {
      mosmv_name = "rm";
      mosmv_description = "remove a key and its associated value from the map.";
      mosmv_intname = 6;
      mosmv_type = 
        motypeCoreFunToMoType
          (MOCFun_type
          ([ 
            MOCMap_type( MOCAll_type (-20), MOCAll_type (-21) );
            MOCAll_type (-20); 
          ], 
            MOCMap_type( MOCAll_type (-20), MOCAll_type (-21) ))
          )
          "Map.rm"
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
