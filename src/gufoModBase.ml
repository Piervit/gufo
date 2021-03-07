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

(*Base system function *)

open Gufo.MCore
open GenUtils

open Sedlexing
open Sedlex_menhir
open Printf
open Gufo_lexer
open Gufo_lexing

open GufoStartComp
open GufoLocHelper


let modtypes = IntMap.empty



let loadProg args scope =  assert false 


let topvars = 
  [
    {
      mosmv_name = "load";
      mosmv_description = "Load the given file as a as a gufo module.";
      mosmv_intname = 1;
      mosmv_type = 
        motypeCoreFunToMoType
          (MOCFun_type
          ([ (MOCBase_type (MTypeString) ) ;]
          , (MOCBase_type MTypeBool )))
        "Base.load"
        ;
      mosmv_action= loadProg;
    };
  ]

let mosysmodule =
{
  mosm_name= "Base";
  mosm_types = modtypes;
  mosm_typstr2int= StringMap.empty;
  mosm_typstrfield2int= StringMap.empty;
  mosm_typstrfield2inttype = StringMap.empty;
  mosm_typfield2inttype = IntMap.empty;
  mosm_topvar= 
    List.fold_left (fun acc vars -> IntMap.add vars.mosmv_intname vars acc) IntMap.empty topvars;
  mosm_varstr2int= List.fold_left (fun acc vars -> StringMap.add vars.mosmv_name vars.mosmv_intname acc) StringMap.empty topvars;
}
