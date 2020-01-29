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


open GufoConfig
type position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}

(*We don't use the pos_fname to compare pos because we expect every position to
come from the same filename and we don't want to lose time comparing string .*)
let comparePos pos1 pos2 = 
  match (compare pos1.pos_lnum pos2.pos_lnum) with
    | 0 ->
        compare pos1.pos_bol pos2.pos_bol
    | i -> i 


module IntSet = Set.Make(struct type t = int let compare = compare end)
module StringSet = Set.Make(struct type t = string let compare = compare end)
module IntMap = Map.Make(struct type t = int let compare = compare end)
module PositionMap = Map.Make(struct type t = position let compare = comparePos end)
module StringMap = Map.Make(String)


let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)
 

let debug_info s = 
  match GufoConfig.get_debug_level () with
    | DBG_FULL  
    | DBG_INFO -> Printf.printf "%s\n" s
    | DBG_NO_DEBUG -> ()



let debug_print s = 
  match GufoConfig.get_debug_level () with
    | DBG_FULL -> Printf.printf "%s\n" s
    | DBG_INFO 
    | DBG_NO_DEBUG -> ()


let debug_title0 s =
  Printf.sprintf "\n%s\n=========================================\n" s

let debug_title1 s =
  Printf.sprintf "\n%s\n=======================\n" s

let debug_title2 s =
  Printf.sprintf "\n%s\n============\n" s

let debug_title3 s =
  Printf.sprintf "\n%s\n======\n" s

let debug_title4 s =
  Printf.sprintf "\n%s\n===\n" s




(*LIST UTILITY*)

(*return tuple (a,b) with a from list la and b element from lb at same position
than a in la. (a,b) are the first combination matching predicate f.
Raise [Invalid_argument] if lists haven't same length.
*)
let rec list_find2 f la lb =
  (*check lists have same lenght*)
  match (List.length la) - (List.length lb) with 
    | 0 -> 
        (*check lists are not empty *)
        (match  la, lb with
          | [],[] -> raise Not_found
          | _ -> 
              let (a,la_minus) = (List.hd la, List.tl la) in
              let (b,lb_minus) = (List.hd lb, List.tl lb) in
              (match (f a b) with
          | true -> (a,b)
          | false -> list_find2 f la_minus lb_minus
          ))
    | _ ->  raise (Invalid_argument "list_find2: List cannot be of different lenght")

(*compar is the comparison fonction between simple element of a and b*)
let list_compare compar la lb =
    try 
      let (adiff, bdiff) = list_find2 (fun a b -> compar a b != 0) la lb 
      in compar adiff bdiff
    with 
    | Invalid_argument _ -> (List.length la) - (List.length lb) 
    | Not_found -> 0 

let list_starts lst n = 
   let rec lst_starts_ new_lst lst n =
    match n with
      | 0 -> List.rev new_lst
      | i -> lst_starts_ ((List.hd lst)::new_lst) (List.tl lst)  (n - 1)
  in
lst_starts_ [] lst n  
  

(* return lst without the n first elements. 
 * This function has slow performance.
 * *)
let list_ends lst n = 
  let rec lst_ends_ lst n =
    match n with
      | 0 -> lst
      | i -> lst_ends_ (List.tl lst) (n - 1)
  in
lst_ends_ lst n  

let list_append_at_end lst el = 
  List.rev (el::(List.rev lst))


(*  
 * This function has slow performance.
 * *)
let list_split_at_idx lst n = 
  let rec list_split_at_idx read_lst next_lst n =
    match n with
      | 0 -> (List.rev read_lst,  next_lst)
      | i -> list_split_at_idx (List.cons (List.hd next_lst) read_lst ) (List.tl next_lst) (n - 1)
  in
  list_split_at_idx [] lst n  



(*String utility*)

let rm_first_char str =
  String.sub str 1 ((String.length str)-1)


let start_with_uppercase str = 
  let first_char_code = Char.code (String.get str 0) in
  (first_char_code >= 65) && (first_char_code < 91)

let is_lowercase achar = 
  let char_code = Char.code achar in
  (char_code >= 97) && (char_code < 123)

let split_in_two str pos =
  ((String.sub str 0 pos),
   (String.sub str (pos+ 1) ((String.length str) - (pos+1))))


let compare_first_chars str1 str2 = 
  try 
    (String.compare str1 (String.sub str2 0 (String.length str1) ) ) = 0
  with _ -> false

(*compare_last_chars str1 str2
  return true if str2 finish as str1 (but can be longer).
  return false if str2 is smaller than str1 or if id doesn't finish by it.
  *)
let compare_last_chars str1 str2 = 
  try 
    (String.compare str1 (String.sub str2 ((String.length str2) - (String.length str1)) (String.length str1) ) ) = 0
  with _ -> false



(*Lets say that we are in the dir cwd and we ask for a path "str_path"
(absolute or relative), return the absolute path asked. *)
let get_abs_path cwd str_path =
  let trimmed_str_path = (String.trim str_path) in
  match String.get trimmed_str_path 0 with
    | '/' -> (*absolute path*)
      trimmed_str_path
    | _ -> (*relative path*)
        cwd ^ trimmed_str_path

(*
  let up_char = Char.uppercase_ascii first_char in
  Char.equal first_char up_char
*)
