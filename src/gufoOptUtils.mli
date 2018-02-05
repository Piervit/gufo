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

val fold_over_obinding_val : ('a -> Gufo.MCore.mobinding -> 'a) -> 'a -> Gufo.MCore.motype_val -> 'a

val fold_over_obinding_and_ofun_val : ('a -> Gufo.MCore.mobinding -> 'a) -> 
                                      ('a ->  int GenUtils.StringMap.t * Gufo.MCore.mofunarg list *  Gufo.MCore.motype_val -> 'a) -> 
                                       'a -> 
                                        Gufo.MCore.motype_val -> 'a

(*for a funarg type, return the type unfolded list of arguments. *)
val unstack_args : Gufo.MCore.mofunarg -> int list

(*for a funarg type, return the type unfolded list of arguments. *)
val unstack_args_with_pos : Gufo.MCore.mofunarg -> (int * int list) list

(*Simple utility fonction to get type of element in case of multi level list.*)
val get_type_at_deep : Gufo.MCore.motype_or -> int -> Gufo.MCore.motype_or


val find_type_in_prog : Gufo.MCore.fullprogopt -> 
                        GenUtils.IntMap.key option -> 
                        GenUtils.IntMap.key -> 
                        Gufo.MCore.mocomposed_type

(*field has type (int option * int) *)
(* return the composed type which own the field *)
val get_ownertype_from_field : Gufo.MCore.fullprogopt -> 
                               Gufo.MCore.moprogram -> 
                               GenUtils.IntMap.key option * GenUtils.IntMap.key -> 
                               Gufo.MCore.motype_or


val get_type_field_from_field : Gufo.MCore.fullprogopt -> 
                                Gufo.MCore.moprogram -> 
                                GenUtils.IntMap.key option * GenUtils.IntMap.key -> 
                                Gufo.MCore.motype_field


val get_type_from_field : Gufo.MCore.fullprogopt -> 
                          Gufo.MCore.moprogram -> 
                          GenUtils.IntMap.key option * GenUtils.IntMap.key -> 
                          Gufo.MCore.motype_or


val get_type_from_ref : Gufo.MCore.fullprogopt -> 
                        Gufo.MCore.moprogram -> 
                        Gufo.MCore.motype_or GenUtils.IntMap.t GenUtils.IntMap.t -> 
                        Gufo.MCore.moref_val -> 
                        Gufo.MCore.motype_or


