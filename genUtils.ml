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
module IntSet = Set.Make(struct type t = int let compare = compare end)
module IntMap = Map.Make(struct type t = int let compare = compare end)
module StringMap = Map.Make(String)


let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)


let debugPrint s = 
  match !debug with
    | true -> Printf.printf "%s" s
    | false -> ()

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
  let first_char = String.get str 0 in
  let up_char = Char.uppercase_ascii first_char in
  Char.equal first_char up_char
