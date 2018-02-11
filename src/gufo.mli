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

open GenUtils
open GufoParsed
open GufoUtils

(*The simple parsed representation is located in gufo_parsed.ml while in this
 * file, we have the "optimised representation".
 *
 * In the optimised representation, the variable are refered though an integer
 * instead than though a string. We sometimes just keep the string as a debug
 * value.
 * *)


module MCore :
sig

  type motype_field = {
    motf_name : int;
    motf_type: motype_or;
    motf_debugname : string;
  }
  
  and mocomposed_type = {
    moct_name: int;
    moct_fields: motype_field IntMap.t; (*ordered according to their name *)
    moct_internal_val : mtype_val StringMap.t; 
    moct_debugname: string;
  }
 
  and mobase_type = GufoParsed.mbase_type

  
  and motype =
   | MOComposed_type of mocomposed_type
   | MOBase_type of mobase_type
   | MOTuple_type of motype_or list
   | MOList_type of motype_or
   | MOOption_type of motype_or
   | MOSet_type of motype_or
   | MOMap_type of motype_or * motype_or
   | MOFun_type of motype_or list * motype_or (*arguments type, ret type *)
   | MOAll_type of int(*ocaml 'a , the int is only an identifier*)
   | MOUnit_type
   | MORef_type of int option * int * int * motype_or list
 (*
   (module * id_ref * deep *args ) 
   THIS TYPE IS A TRICKY ONE, because it is
   not always used on the same way: in PART 1 and PART 2 of gufoParsedToOpt it
   is a ref to a composed type which is only used on first step of type
   definition. At the end of PART 2, we do not used it any more. In PART 3
   (type checking) we use it to refer the type of another var, for exemple:
      let $a = $b 

  then $a will have type (MORef_type (None, id($b), 0)) until we are able to replace
  this by the effective type of $b.
  In PART 1 and 2, the third element (deep) is not used (always set to 0). In
  part 3, this is used to refer to a deepness in list, for exemple: 

  let $a = $b[1][2]
  $a will have type (MORef_type (None, id_$b, 2))

  Still, in PART 3, we don't use MORef_type with field, because if we have the field name, we can resolve the type more precisely:
  let $a = $b.toto 

  We can deduce the type of $a, from the know type of the field toto. 

  args is only used in PART 3 and allows to know if the MORef_type
  corresponding to a function call is used with argument (and of which type).

  Normally at the end of type-checking (end of PART 3), MORef_type are no
  more present. If there is still, it shows the impossibility to deduce a
  type and should lead to an error.
   
   *)
   | MOTupel_type of int option * int *int * motype_or list * int list
   (*
    * (*module * id_ref * deep * args * pos_in_tuble *)
    * appears in PART 3 of type checking in a similar way than MORef_type.
    * This means that the type is a reference of an element of the tuple (at
    * pos pos_in_tuble)*)

 and motype_or = 
   | MOUnique_type of motype 
   | MOOr_type of motype list * int

   (* The SimpleCore module is internal stuff to manage Map and Set
    * I tried to have this as a functor but as the idea was that any value of
    * Gufo could be a key value, I got a recursivity problem.
    *
    * This module is the solution I got.
    *
    * PS: as of 25/01/2018: Set and Map have not been tested.
    *
    * *)
  module SimpleCore : 
   sig
     type mitype_val

   end

  module MMap : 
    sig
      type key = SimpleCore.mitype_val
      type (+'a) t
      val empty: 'a t
      val is_empty: 'a t -> bool
      val mem: key -> 'a t -> bool
      val add: key -> 'a -> 'a t -> 'a t
      val singleton: key -> 'a -> 'a t
      val remove: key -> 'a t -> 'a t
      val merge: (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
      val union: (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
      val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
      val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
      val iter: (key -> 'a -> unit) -> 'a t -> unit
      val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      val for_all: (key -> 'a -> bool) -> 'a t -> bool
      val exists: (key -> 'a -> bool) -> 'a t -> bool
      val filter: (key -> 'a -> bool) -> 'a t -> 'a t
      val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
      val cardinal: 'a t -> int
      val bindings: 'a t -> (key * 'a) list
      val min_binding: 'a t -> (key * 'a)
      val min_binding_opt: 'a t -> (key * 'a) option
      val max_binding: 'a t -> (key * 'a)
      val max_binding_opt: 'a t -> (key * 'a) option
      val choose: 'a t -> (key * 'a)
      val choose_opt: 'a t -> (key * 'a) option
      val split: key -> 'a t -> 'a t * 'a option * 'a t
      val find: key -> 'a t -> 'a
      val find_opt: key -> 'a t -> 'a option
      val find_first: (key -> bool) -> 'a t -> key * 'a
      val find_first_opt: (key -> bool) -> 'a t -> (key * 'a) option
      val find_last: (key -> bool) -> 'a t -> key * 'a
      val find_last_opt: (key -> bool) -> 'a t -> (key * 'a) option
      val map: ('a -> 'b) -> 'a t -> 'b t
      val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
    end

  module MSet :
    sig
      type elt = SimpleCore.mitype_val
      type t
      val empty: t
      val is_empty: t -> bool
      val mem: elt -> t -> bool
      val add: elt -> t -> t
      val singleton: elt -> t
      val remove: elt -> t -> t
      val union: t -> t -> t
      val inter: t -> t -> t
      val diff: t -> t -> t
      val compare: t -> t -> int
      val equal: t -> t -> bool
      val subset: t -> t -> bool
      val iter: (elt -> unit) -> t -> unit
      val map: (elt -> elt) -> t -> t
      val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
      val for_all: (elt -> bool) -> t -> bool
      val exists: (elt -> bool) -> t -> bool
      val filter: (elt -> bool) -> t -> t
      val partition: (elt -> bool) -> t -> t * t
      val cardinal: t -> int
      val elements: t -> elt list
      val min_elt: t -> elt
      val min_elt_opt: t -> elt option
      val max_elt: t -> elt
      val max_elt_opt: t -> elt option
      val choose: t -> elt
      val choose_opt: t -> elt option
      val split: elt -> t -> t * bool * t
      val find: elt -> t -> elt
      val find_opt: elt -> t -> elt option
      val find_first: (elt -> bool) -> t -> elt
      val find_first_opt: (elt -> bool) -> t -> elt option
      val find_last: (elt -> bool) -> t -> elt
      val find_last_opt: (elt -> bool) -> t -> elt option
      val of_list: elt list -> t
    end

  type movar = {
    mova_name: int list; (*we have a list when the input is a tuple, for
    exemple in the case "let a,b= 5" then mva_name is ("a","b"). *)
    mova_value: motype_val;
  }

  and moref_val = {
    morv_module : int option; (* None if defined in current module.*)
    morv_varname : int * (int option * int) list; (*varname * (fieldmoduleid * fieldsid *)
    morv_index : motype_val list option;
    morv_debugname : string;
  }
 
  and mcSetEl = motype_val

  and mocmd_redir =  
    | MORedirOStdOut
    | MORedirOStdErr
    | MORedirOFile of mostringOrRef_val (*path*)
    | MORedirOFileAppend of mostringOrRef_val (*path*)
    | MORedirEStdOut
    | MORedirEStdErr
    | MORedirEFile of mostringOrRef_val (*path*)
    | MORedirEFileAppend of mostringOrRef_val (*path*)
    | MORedirIStdIn
    | MORedirIFile of mostringOrRef_val (*path*)

  and mocmd_output = 
    | MOCMDOStdOut
    | MOCMDOStdErr
    | MOCMDOFile of mostringOrRef_val (*path*)
    | MOCMDOFileAppend of mostringOrRef_val (*path*)

  and mocmd_outputerr = 
    | MOCMDEStdOut
    | MOCMDEStdErr
    | MOCMDEFile of mostringOrRef_val (*path*)
    | MOCMDEFileAppend of mostringOrRef_val (*path*)

  and mocmd_input = 
    | MOCMDIStdIn
    | MOCMDIFile of mostringOrRef_val (*path*)

  and mostringOrRef_val =
    | MOSORString of string
    | MOSORExpr of motype_val


  and mocmd_val = {
    mocm_cmd : string;
    mocm_args : mostringOrRef_val list;
    mocm_res : int option; (*if res is None, the command has not already been executed*)
    mocm_output : mocmd_output; 
    mocm_outputerr : mocmd_outputerr; 
    mocm_input_src : mocmd_input; 
    mocm_input : string option; 
    mocm_print: string option;
    mocm_print_error: string option;
    mocm_print_std: string option;
  }

  and mocmd_seq = 
    | MOSimpleCmd of mocmd_val
    | MOForkedCmd of mocmd_seq (*symbol & *)
    | MOAndCmd of mocmd_seq * mocmd_seq (*&& : The right side of && will only be evaluated if the exit status of the left side is zero.*)
    | MOOrCmd of mocmd_seq * mocmd_seq (*|| : The right side of || will only be evaluated if the exit status of the left side is non-zero.*)
    | MOSequenceCmd of mocmd_seq * mocmd_seq (*; *)
    | MOPipedCmd of mocmd_seq * mocmd_seq (*; *)
 
  and mobase_type_val = 
    | MOTypeStringVal of string
    | MOTypeBoolVal of bool
    | MOTypeIntVal of int
    | MOTypeFloatVal of float
    | MOTypeCmdVal of mocmd_seq
(*     | MTypeFileVal of mfile_val *)
  
 
  and mocomposed_type_val = {
    mocv_module_def : int option ; (*the module in which the type is defined.*)
    mocv_fields: motype_val IntMap.t; (*The key are are name of the field, the values are the value of the field.*)
    mocv_resolved_type : int option *int ; (*module, id of the type*)
  }
  
 
  and mosimple_type_val = 
    | MOBase_val of mobase_type_val 
    | MOTuple_val of motype_val list
    | MOList_val of motype_val list
    | MONone_val 
    | MOSome_val of motype_val
    | MOSet_val of MSet.t
    | MOMap_val of motype_val MMap.t
    | MOFun_val of int StringMap.t * mofunarg list * motype_val (*args name map (for debug + color)* args id * body_expr *)
    | MOEmpty_val
  
  and mofunarg = 
    | MOBaseArg of int
    | MOTupleArg of mofunarg list
  
  and motype_val = 
    | MOSimple_val of mosimple_type_val
    | MOComposed_val of mocomposed_type_val
    | MORef_val of moref_val * motype_val list (*module, varname args*)
    | MOBasicFunBody_val of mo_expr_operation * motype_val * motype_val
    | MOBind_val of mobinding
    | MOIf_val of motype_val * motype_val * motype_val
    | MOComp_val of mocomp_op * motype_val * motype_val (* comp_op * left_expr * right_expr *)
    | MOBody_val of motype_val list

  and mocomp_op = GufoParsed.mcomp_op

  and mobinding = {
    mobd_name : (int * int list) list; (*for each element of the tuple,
    give its unique id and code its position in the tuple with int list. 
    for exemple, if we just have "let $a = 4 in $a", mbd_name will be [x, []]
    with x its associated integer id.
    If we have "let $a -- ($b -- $c) -- $d in ..." the mbd_name will be
      [xa, [1]; xb, [2;1]; xc, [2;2]; xd, [3]]
    *)
    mobd_debugnames : string IntMap.t; (*this has been added for debug and possible colorization*)
    mobd_value: motype_val;
    mobd_body: motype_val ;
  }

  and mo_expr_operation = GufoParsed.m_expr_operation
 
  (** A scope contains types and variables*)
  and moscope = {
    mosc_father : moscope option; (* None when toplevel*)
    mosc_vars : movar list;
    mosc_type : motype list; (* always MComposed_type except for toplevel*) 
  }
  
  and momodule = {
    momo_name : int;
    momo_topvar : movar list
  }
  
  and moshell_state = GufoParsed.mshell_state
  
  and mosysmodulemvar = {
    mosmv_name: string;
    mosmv_intname: int;
    mosmv_type: motype_or; 
    mosmv_action: (motype_val list -> topvar_val IntMap.t -> motype_val); (* function argument -> scope -> function res *)
  }

  and mosysmodulefield = {
    mosmf_name : string;
    mosmf_intname: int;
    mosmf_type: motype_or;
  }

  and mosysmoduletype = {
    mosmt_name: string;
    mosmt_intname: int;
    mosmt_fields : mosysmodulefield list;
    mosmt_internal_val: mtype_val StringMap.t;
  }

  and mosysmodule = {
    mosm_types : mosysmoduletype IntMap.t;
    mosm_typstr2int: int StringMap.t;
    mosm_typstrfield2int: int StringMap.t; (*from the field string to the int field*)
    mosm_typstrfield2inttype: int StringMap.t; (*from the field string to the int type*)
    mosm_typfield2inttype: int IntMap.t; (*from the field id to the int type*)
    mosm_topvar: mosysmodulemvar IntMap.t;
    mosm_varstr2int: int StringMap.t;
  }

  and  momodultype = 
    | MOUserMod of moprogram
    | MOSystemMod of mosysmodule

  (*A full program representation. Full means with modules.
   * A full program is composed of a main program and of modules programmes.
   *
   * *)
  and fullprogopt= {
    mofp_mainprog : moprogram ;
    mofp_progmodules : momodultype IntMap.t ;
    mofp_module_dep : IntSet.t IntMap.t; (*For a module i, give the set of module used as dependancy*)
    mofp_progmap : int StringMap.t; (*For a module name, give the module in*)
    mofp_progmap_debug : string IntMap.t; (*For debug purpose, allow to get the module name back.*)
  }


  (*A program representation. I can be a main program or a modules. *)
  and moprogram = {
    mopg_name : int;
    mopg_types : mocomposed_type IntMap.t; (*from the struct id, get the type*)
    mopg_field_to_type: int IntMap.t; (* from a field id, get the type.*)
    mopg_topvar: topvar_val IntMap.t;
    mopg_topvar_bind_vars: IntSet.t StringMap.t IntMap.t; (*used to give indication of the type of a var in a binding (or in a fun):
      For a topvar, for a var name, I want a set of integer var id.
      *)
    mopg_topcal : motype_val;
    mopg_topcal_bind_vars: IntSet.t StringMap.t; (*used to give indication of the type of a var in a binding (or in a fun)*)
    mopg_topvar2int: int StringMap.t ;
    mopg_topvar_debugname: string IntMap.t ;
    mopg_ctype2int: int StringMap.t; (*composed type name to int*)
    mopg_field2int: int StringMap.t;
  }

  and topvar_val = 
    | MOTop_val of motype_val 
    | MOTupEl_val of int * int list (*reference of the tupel, position in the tuple. This is a list for multi dimensional tuple*)

  and moprocess = GufoParsed.mprocess

  (*the shell environment is independant from the program and is only provided
   * at execution time.*)
  type shell_env={
    mose_curdir : string;
    mose_envvar : string StringMap.t;
  }



  type t = mosimple_type_val


  open SimpleCore

  (**Transformation from SimpleCore to Core **)

  val simple_to_core_val: mitype_val -> motype_val

  val core_to_simple_mtype: motype_val -> mitype_val

  (**END Transformation from SimpleCore to Core **)

  (*functions for program *)
  
  val empty_oprog : moprogram
  val empty_ofullprog: fullprogopt
  val is_empty_ofullprog : fullprogopt -> bool

  (*END functions for progam *)

  (*EXPR *)

  val empty_expr : motype_val

  (*END EXPR *)

  (*functions for shell_env *)

  (*From a path, generate a shell environment (without specific environment
   * variables. *)
  val get_env : string -> shell_env

  (*set_var cur_env var value : return a new env which is the cur_env with the
   * environment variable 'var' set to 'value'.*)
  val set_var: shell_env -> string -> string -> shell_env
  
  (*END functions for shell_env*)

  (**PRINTER **)
  val type_to_string: motype_or -> string

  val moval_to_string: motype_val -> string

  (**END PRINTER **)
end
