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

(*The transformer from low level representation to high level representation
 * and the type checker.*)


val search_modules : GufoParsed.mprogram -> int GenUtils.StringMap.t

(*From a low level fullprog to a highlevel fillprog with a map of the type. *)
val parsedToOpt : GufoParsed.fullprog -> 
                  Gufo.MCore.fullprogopt * Gufo.MCore.motype GenUtils.IntMap.t GenUtils.IntMap.t

(*
 * add_prog_to_optprog fulloptiprog fullprog 
 *
 * fulloptiprog is the provious verified oprog.
   *fullprog is the new prog to include in fulloptiprog.
    This function is expected to be called in the context of a console program :
    it means that we make transformation choice considering this context, for
    exemple we remove the topcal of the old mainprog.

   * *)
val add_prog_to_optprog: Gufo.MCore.fullprogopt -> 
                         GufoParsed.fullprog -> 
                         Gufo.MCore.fullprogopt * Gufo.MCore.motype GenUtils.IntMap.t GenUtils.IntMap.t

val add_module_to_optprog : string -> Gufo.MCore.fullprogopt -> GufoParsed.mprogram -> Gufo.MCore.fullprogopt * Gufo.MCore.motype GenUtils.IntMap.t GenUtils.IntMap.t

(*This function should not be used if you are not sure about what you are doing.
  This is only needed from GufoEngine in a case when we have to trick partial
  interpretation for core system (we need to generate fresh index...).
*)
val get_fresh_int : unit -> int
