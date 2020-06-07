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
open GufoLocHelper

let types = IntMap.empty

let split args scope =  
  match (lst_val_only args) with 
    |  [MOSimple_val (MOBase_val MOTypeStringVal (str))] ->
        let res = String.split_on_char '\n' str.loc_val in
        let res =
        List.map (fun str -> box_loc (MOSimple_val 
                    (MOBase_val (MOTypeStringVal(box_loc(String.trim str)))))) res
        in box_loc (MOSimple_val (MOList_val res))
    | _ -> assert false 


let contains args scope =  
  match (args) with 
    |  [str; substr] ->
      (match str.loc_val, substr.loc_val with
        | MOSimple_val (MOBase_val MOTypeStringVal (str)),
          MOSimple_val (MOBase_val MOTypeStringVal sstr) 
          when (String.equal sstr.loc_val "")
          -> 
            (*We always contains the empty string*)
            box_loc (MOSimple_val (MOBase_val (MOTypeBoolVal (box_loc true) )))

        | MOSimple_val (MOBase_val MOTypeStringVal (str)),
          MOSimple_val (MOBase_val MOTypeStringVal (substr))
          ->
            (
            let substr_first_char = String.get substr.loc_val 0 in
            (*the list of every position where the first char of substr appears.*)
            let possible_starts = 
              let rec get_possible_starts from_pos possible_starts =
                match from_pos with 
                  | i when i < 0 || i >= (String.length str.loc_val) -> 
                      possible_starts
                  | _ -> 
                    (
                      match (String.index_from_opt str.loc_val from_pos substr_first_char) with
                        | None -> possible_starts
                        | Some new_pos -> get_possible_starts (new_pos + 1) (new_pos::possible_starts)
                    )
              in
                List.rev (get_possible_starts 0 [])
            in
            (*for each possible start, we check the following chars.*)
            let substr_last_index = String.length substr.loc_val  in
            let str_last_index = String.length str.loc_val in
              
            let rec rec_contains pos_in_str pos_in_substr = 
              (match pos_in_substr - substr_last_index, pos_in_str - str_last_index with 
                | 0,_ -> true 
                | _,0 -> false
                | _,_ -> 
                  let sub_current_char = String.get substr.loc_val pos_in_substr in
                  let str_current_char = String.get str.loc_val pos_in_str in
                  (match (int_of_char sub_current_char) - (int_of_char str_current_char) with
                    | 0 -> rec_contains (pos_in_str + 1) (pos_in_substr + 1)
                    | _ -> false
                  )    
              )
            in
              box_loc (MOSimple_val (MOBase_val (MOTypeBoolVal (box_loc(
                List.exists 
                  (fun start_pos -> 
                    rec_contains (start_pos + 1) 1 
                  )
                  possible_starts
                ))))
                )
            )
    | _ -> assert false 
    )
  | _ -> assert false



(*
let split args scope =  
  match args with 
    |  [MOSimple_val (MOBase_val MOTypeStringVal (sep));
        MOSimple_val (MOBase_val MOTypeStringVal (str))] ->
        let res = 
          match sep.length with
            | 1 -> String.split_on_char '\n' str 
            | n -> (*TODO*)
        in
        let res =
        List.map (fun str -> MOSimple_val (MOBase_val (MOTypeStringVal((String.trim str))))) res
        in MOSimple_val (MOList_val res)
    | _ -> assert false 

*)
let topvars = 
  [
    {
      (*split a string into a list *)
      mosmv_name = "split";
      mosmv_description = "Split each line of the given string into a list.";
      mosmv_intname = 1;
      mosmv_type = 
        MOFun_type
        ([
         MOBase_type (MTypeString); ], 
         MOList_type (MOBase_type (MTypeString)))
        
        ;
      mosmv_action= split;
    };
    {
      (*split a string into a list *)
      mosmv_name = "contains";
      mosmv_description = "contains str substr: Return true if substr is contained within str. Else return false.";
      mosmv_intname = 2;
      mosmv_type = 
        MOFun_type
        ([
         MOBase_type (MTypeString); 
         MOBase_type (MTypeString); 
         ], 
         MOBase_type (MTypeBool))
        
        ;
      mosmv_action= contains;
    };
  ]

let mosysmodule =
{
  mosm_name= "String";
  mosm_types = types;
  mosm_typstr2int= StringMap.empty;
  mosm_typstrfield2int= StringMap.empty;
  mosm_typstrfield2inttype = StringMap.empty;
  mosm_typfield2inttype = IntMap.empty;
  mosm_topvar= List.fold_left (fun acc vars -> IntMap.add vars.mosmv_intname vars acc) IntMap.empty topvars;
  mosm_varstr2int= List.fold_left (fun acc vars -> StringMap.add vars.mosmv_name vars.mosmv_intname acc) StringMap.empty topvars;
}
