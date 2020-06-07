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

open GufoParsedHelper
open GufoParsed

let string_of_float_loc floc = 
  {floc with loc_val = string_of_float floc.loc_val}

let string_of_int_loc iloc = 
  {iloc with loc_val = string_of_int iloc.loc_val}


let box_loc v = box_with_dummy_pos v

let lst_val_only loc_lst = List.map (fun a -> a.loc_val) loc_lst
