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


(* This is the interpretation engine of Gufo. *)
(* An execution is the reduction of a large expressions to a simple expression:
 * basic type or composed type.
 * It does not check for types or errors as it assume the type checker has
 * done the job. This makes it efficient.
 *
 * *)

(* This function allow to compare two gufo values. *)
val val_compare: Gufo.MCore.motype_val -> Gufo.MCore.motype_val -> int

(* This function sadly needs to be exposed because it is used by system
 * modules. 
 * This should normally not be used.
 * *)
val apply_fun : bool -> Gufo.MCore.topvar_val GenUtils.IntMap.t -> Gufo.MCore.motype_val -> Gufo.MCore.motype_val list -> Gufo.MCore.motype_val

(* The execution of a Gufo program.
 *
 * The fullprogopt returned is a reduced prog after "execution".
 * *)
val exec: Gufo.MCore.fullprogopt -> Gufo.MCore.shell_env -> Gufo.MCore.fullprogopt * Gufo.MCore.shell_env
