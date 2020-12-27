(*
  This file is part of Gufo but was initially copied from
    https://github.com/colis-anr/morbig (file CSTHelpers.mli).
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

(** {2 Helpers about positions} *)

open GufoParsed 

val on_located : ('a -> 'b) -> 'a located -> 'b

val with_pos : pars_position -> 'a -> 'a located
val with_poss : Lexing.position -> Lexing.position -> 'a -> 'a located

val dummy_lexing_position : Lexing.position
val dummy_position : pars_position

val start_of_position : pars_position -> Lexing.position
val end_of_position : pars_position -> Lexing.position
val filename_of_position : pars_position -> string

val line : Lexing.position -> int
val column : Lexing.position -> int

val characters : Lexing.position -> Lexing.position -> int * int

val emacs_position : string -> int -> int list -> string

val string_of_lexing_position : Lexing.position -> string
val string_of_position : pars_position -> string

(*raise a type error exception*)
val raise_typeError : string -> GufoParsed.pars_position -> 'a

val compare_positions : pars_position -> pars_position -> int

val box_with_dummy_pos : 't -> 't located 
