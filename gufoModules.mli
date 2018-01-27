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

(* System module handling. *)

val is_system_module : string -> bool

val parse_system_module : string -> Gufo.MCore.mosysmodule

val get_intname_from_modulestr : string -> Gufo.MCore.fullprogopt -> int

val get_module_prog_from_modulestr : string -> Gufo.MCore.fullprogopt -> Gufo.MCore.momodultype

val get_oref_from_sysmodule : GufoParsed.mref_val -> Gufo.MCore.fullprogopt -> int option * int

val sysmodctype_to_ctype : Gufo.MCore.mosysmoduletype -> Gufo.MCore.mocomposed_type

val get_types_map : Gufo.MCore.mosysmodule -> Gufo.MCore.motype_or GenUtils.IntMap.t

