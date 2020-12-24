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

open GufoParsed

(*
  fold_over_obinding_val : apply_fun -> accumulator -> expr -> accumulator
  Recursively apply apply_fun to every mobinding found within expr and add
  result in accumulator.

*)
val fold_over_obinding_val : ('a -> Gufo.MCore.mobinding -> 'a) -> 'a -> Gufo.MCore.motype_val located -> 'a

(*
  fold_over_obinding_and_ofun_val: apply_fun -> accumulator -> expr ->
  accumulator Recursively apply apply_fun to every mobinding and mofun_val found
  within expr and add result in accumulator.
*)
val fold_over_obinding_and_ofun_val : 
  ('a -> Gufo.MCore.mobinding -> 'a) -> 
  ('a -> Gufo.MCore.mofun_val  -> 'a) -> 
   'a -> Gufo.MCore.motype_val located -> 'a

(*
  fold_over_oref_val: apply_fun -> accumulator -> expr ->
  accumulator Recursively apply apply_fun to every moref_val found
  within expr and add result in accumulator.
*)
val fold_over_oref_val : ('a -> Gufo.MCore.moref_val -> 'a) -> 
                          'a -> Gufo.MCore.motype_val located -> 'a

(* transform_ref_in_funcall transform_fun -> val -> newval
   transform every moref_val within val into mofun_val (using the
   transformation function transform_fun).
   A ref can have internal links to other references: the childs of a reference
   are transformed by transform_fun before the given reference.
 *)
(* val transform_ref_in_funcall : (Gufo.MCore.moref_val -> Gufo.MCore.mofun_val) -> Gufo.MCore.motype_val -> Gufo.MCore.motype_val *)

(*for a funarg type, return the type unfolded list of arguments. *)
val unstack_args : Gufo.MCore.mofunarg -> int list

(*for a funarg type, return the type unfolded list of arguments. *)
val unstack_args_with_pos : Gufo.MCore.mofunarg -> (int * int list) list

(*Simple utility fonction to get type of element in case of multi level list.*)
val get_type_at_deep : Gufo.MCore.motype -> int -> Gufo.MCore.motype


val find_type_in_prog : Gufo.MCore.fullprogopt -> 
                        GenUtils.IntMap.key option -> 
                        GenUtils.IntMap.key -> 
                        Gufo.MCore.mocomposed_type

(*field has type (int option * int) *)
(* return the composed type which own the field *)
val get_ownertype_from_field : Gufo.MCore.fullprogopt -> 
                               Gufo.MCore.moprogram -> 
                               GenUtils.IntMap.key option * GenUtils.IntMap.key -> 
                               Gufo.MCore.motype


val get_type_field_from_field : Gufo.MCore.fullprogopt -> 
                                Gufo.MCore.moprogram -> 
                                GenUtils.IntMap.key option * GenUtils.IntMap.key -> 
                                Gufo.MCore.motype_field


val get_type_from_field : Gufo.MCore.fullprogopt -> 
                          Gufo.MCore.moprogram -> 
                          GenUtils.IntMap.key option * GenUtils.IntMap.key -> 
                          Gufo.MCore.motype


val get_type_from_ref : Gufo.MCore.fullprogopt -> 
                        Gufo.MCore.moprogram -> 
                        Gufo.MCore.motype GenUtils.IntMap.t GenUtils.IntMap.t -> 
                        Gufo.MCore.moref_val -> 
                        Gufo.MCore.motype


