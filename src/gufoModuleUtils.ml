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

(*Utils for modules.*)

open Gufo.MCore
open GenUtils

let gen_mosm_types sysmodtypes = 
  List.fold_left
    (fun map typ ->
    IntMap.add typ.mosmt_intname typ map
    )
  IntMap.empty
  sysmodtypes


let gen_mosm_typstr2int sysmodtypes = 
  List.fold_left
    (fun map typ -> 
      StringMap.add typ.mosmt_name typ.mosmt_intname map
    )
    StringMap.empty
    sysmodtypes


let gen_mosm_typstrfield2int sysmodtypes =
  List.fold_left
    (fun map typ ->
      List.fold_left 
        (fun map fd -> 
          StringMap.add fd.mosmf_name fd.mosmf_intname map
        )
      map
      typ.mosmt_fields
    )
    StringMap.empty
    sysmodtypes


let gen_mosm_typestrfield2inttype sysmodtypes = 
  List.fold_left
    (fun map typ ->
      List.fold_left 
        (fun map fd -> 
          StringMap.add fd.mosmf_name typ.mosmt_intname map
        )
      map
      typ.mosmt_fields
    )
    StringMap.empty
    sysmodtypes



let gen_mosm_typfield2inttype sysmodtypes = 
  List.fold_left
    (fun map typ ->
      List.fold_left 
        (fun map fd -> 
          IntMap.add fd.mosmf_intname typ.mosmt_intname map
        )
      map
      typ.mosmt_fields
    )
    IntMap.empty
    sysmodtypes

let module_to_filename modulename =
    String.concat "." [String.uncapitalize_ascii modulename; "gf"]

let filename_to_module filename = 
    String.sub (String.capitalize_ascii filename) 0 ((String.length filename) - 3)
 
let getModuleNameFromPath path =
  let last_file = List.hd (List.rev (String.split_on_char '/' path)) in
  filename_to_module last_file

let getNbArgsFromCoreFunction sysmodVar =
  match sysmodVar.mosmv_type with
    | MOFun_type (args, ret) ->
        List.length args
    | _ -> 0
