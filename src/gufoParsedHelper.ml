(*
  This file is part of Gufo but was initially copied from
    https://github.com/colis-anr/morbig (file CSTHelpers.ml).
    Morbig is also a GPL V3 Program, so it should be compatible.


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
open GufoParsed
open Lexing

(* Helpers about positions *)

let on_located f x = f x.loc_val

let with_pos p v =
  {
    loc_val      = v;
    loc_pos      = p;
  }

let dummy_lexing_position = {
    pos_fname = "";
    pos_lnum  = -1;
    pos_bol   = -1;
    pos_cnum  = -1;
  }

let dummy_position = {
    ppos_start = dummy_lexing_position;
    ppos_end = dummy_lexing_position;
  }

let with_poss p1 p2 v =
  with_pos { ppos_start = p1; ppos_end = p2 } v

let start_of_position p = p.ppos_start

let end_of_position p = p.ppos_end

let filename_of_position p =
  p.ppos_start.pos_fname

let line p =
  p.pos_lnum

let column p =
  p.pos_cnum - p.pos_bol

let characters p1 p2 =
  (column p1, p2.pos_cnum - p1.pos_bol) (* intentionally [p1.pos_bol] *)

let emacs_position filename linenumber cs =
  Printf.sprintf "%sine %d%s"
    (if filename = "" then
       "L"
     else
       Printf.sprintf "File \"%s\", l" filename
    )
    linenumber
    (match cs with
     | [] -> ""
     | [c] -> Printf.sprintf ", character %d" c
     | c1 :: c2 :: _ -> Printf.sprintf ", characters %d-%d" c1 c2)

let string_of_lexing_position p =
  emacs_position p.pos_fname (line p) [column p]

let string_of_position p =
  let filename = filename_of_position p in
  let l = line p.ppos_start in
  let c1, c2 = characters p.ppos_start p.ppos_end in
    if filename = "" then
      Printf.sprintf "Line %d, characters %d-%d" l c1 c2
    else
      Printf.sprintf "File \"%s\", line %d, characters %d-%d" filename l c1 c2

let compare_positions p1 p2 =
  compare p1.ppos_start.pos_cnum p2.ppos_start.pos_cnum

let merge_positions p1 p2 =
  { ppos_start = p1.ppos_start; ppos_end = p2.ppos_end }


let box_with_dummy_pos a =
  {
    loc_val = a; 
    loc_pos = dummy_position; 
  }
