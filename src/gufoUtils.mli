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

(*Utility functions for the low level representation. *)

val fold_over_cmd_val_from_cmdseq :('a -> GufoParsed.mcmd_val -> 'a) -> 'a -> GufoParsed.mcmd_seq -> 'a

val fold_over_cmd_val : ('a -> GufoParsed.mcmd_val -> 'a) -> 'a -> GufoParsed.mtype_val -> 'a

val fold_over_composed_type_val :('a -> GufoParsed.mcomposed_type_val -> 'a) -> 'a -> GufoParsed.mtype_val -> 'a

val fold_over_mref_val : ('a -> GufoParsed.mref_val -> 'a) -> 'a -> GufoParsed.mtype_val -> 'a

val fold_over_mref_type :('a -> GufoParsed.mref_val -> 'a) -> 'a -> GufoParsed.mtype -> 'a

val fold_over_binding_val : ('a -> GufoParsed.mbinding -> 'a) -> 'a -> GufoParsed.mtype_val -> 'a

(* The set of every Gufo keyword. *)
val gufoKeywords: GenUtils.StringSet.t

(* is_keyword word -> return true if work is a gufoKeywords*)
val is_keyword: string -> bool
