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

(* Standard system module handling. *)

open GenUtils
(**
 *  This exception is throw if a caller of a gufoModule function try to use an
 *  invalid module.
 *)

exception GufoInvalidModule of string
(**
 * This exception is thrown if a function from gufoModules is called with an
 * invalid argument.
 * *)
exception GufoModuleInvalidArgument

(** 
 * is_system_module : module -> is_module
 *
 * Return true if the given string (such as "list" correspond to a standard
 * system module.
 *  Else, return false.
 * *)
val is_system_module : string -> bool

(**
 * parse_system_module: module -> sysmodule
 *
 * Return the mosysmodule corresponding to the given module (such as "list").
 * If the given string do not correspond to a given module, throw a
 * GufoInvalidModule exception.
 *)
val parse_system_module : string -> Gufo.MCore.mosysmodule

(**
 * get_intname_from_modulestr : module -> prog -> intname
 *
 * From a module (as string such as "List"), return the intname: the integer
 * identifier of the module in the given program. This can both be used from
 * standard library module and user-defined modules.
 *
 * If 'module' is not a module available in 'prog', throw GufoInvalidModule
 * exception.
 *)
val get_intname_from_modulestr : string -> Gufo.MCore.fullprogopt -> int

(**
 *get_module_prog_from_modulestr : module -> prog -> module
 *
 * From a 'module' (as string such as "List"), return the internal representation
 * of the module. This can both be used from standard library module and
 * user-defined modules.
 *
 * If 'module' is not a module available in 'prog', throw GufoInvalidModule
 * exception.
 *
 *)
val get_module_prog_from_modulestr : string -> Gufo.MCore.fullprogopt -> Gufo.MCore.momodultype


(**
 *get_oref_from_sysmodule : ref -> prog -> (i_module, i_var)
 *
 * From a reference 'ref', representing a reference in an explicit module,
 * return the module id ('i_module') and the id of the referenced var ('i_var').
 * 
 * if 'ref' do not has an explicit module, the function throw a
 * GufoModuleInvalidArgument exception.
 *
 *
 *)
val get_oref_from_sysmodule : GufoParsed.mref_val -> Gufo.MCore.fullprogopt -> int * int

(**
 * sysmodctype_to_ctype : sysmoduletype -> ctype 
 *
 * From a standard library type, return a "normal" composed type.
 *
 *)
val sysmodctype_to_ctype : Gufo.MCore.mosysmoduletype -> Gufo.MCore.mocomposed_type

(**
 * get_types_map : module -> typeMap
 *
 * Return every types defined in a standard library modules as a map of type.
 *)
val get_types_map : Gufo.MCore.mosysmodule -> Gufo.MCore.motype GenUtils.IntMap.t

val get_system_modules : GufoParsed.mmodultype IntMap.t 
val get_system_modules_dep : IntSet.t IntMap.t 
val get_system_modules_progmap : int StringMap.t
val get_system_modules_progmap_debug : string IntMap.t
