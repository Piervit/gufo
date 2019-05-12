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

(*the system module for Command. *)
 

open Gufo.MCore
open GenUtils
open GufoModuleUtils
open GufoParsed

let listtypes = IntMap.empty

let types = 
  [
  {
    mosmt_name= "cmd";
    mosmt_intname= 1;
    mosmt_fields= 
      [
        {
          mosmf_name = "res";
          mosmf_intname= 2;
          mosmf_type= MOUnique_type (MOOption_type (MOUnique_type (MOBase_type (MTypeInt))));
        };
      {
        mosmf_name = "print";
          mosmf_intname= 3;
          mosmf_type= MOUnique_type ((MOBase_type (MTypeString)));
        };
      {
        mosmf_name = "print_err";
          mosmf_intname= 4;
          mosmf_type= MOUnique_type ((MOBase_type (MTypeString)));
        }

      ];
    mosmt_internal_val = StringMap.empty; 
  };
  ]


let mosysmodule =
{
  mosm_name= "Cmd";
  mosm_types = gen_mosm_types types;
  mosm_typstr2int = gen_mosm_typstr2int types;
  mosm_typstrfield2int= gen_mosm_typstrfield2int types ;
  mosm_typstrfield2inttype= gen_mosm_typestrfield2inttype types;
  mosm_typfield2inttype= gen_mosm_typfield2inttype types;
  mosm_topvar=  IntMap.empty ;
  mosm_varstr2int=  StringMap.empty ;
}
