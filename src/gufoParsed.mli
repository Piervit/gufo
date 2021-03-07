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

(*The low level representation *)

open GenUtils


(*TYPES *)

type lexing_position = Stdlib.Lexing.position 

type pars_position = {
  ppos_start : lexing_position;
  ppos_end : lexing_position;
}

and 'a located = {
    loc_val : 'a ;
    loc_pos : pars_position;
}

exception SyntaxError of string located
exception TypeError of string located
exception ExecutionError of string
exception VarError of string
exception InternalError of string
exception UncatchedError of string
exception RuntimeError of string



type mvar = {
  mva_name: var_decl located; 
  mva_value: mtype_val located;
}

and var_decl = 
  | MBaseDecl of string located
  | MTupDecl of var_decl list


and mcmd_redir =  (*only for parsing, then we use mcmd_* *)
  | MRedirOStdOut
  | MRedirOStdErr
  | MRedirOFile of mstringOrRef_val (*path*)
  | MRedirOFileAppend of mstringOrRef_val (*path*)
  | MRedirEStdOut
  | MRedirEStdErr
  | MRedirEFile of mstringOrRef_val (*path*)
  | MRedirEFileAppend of mstringOrRef_val (*path*)
  | MRedirIStdIn
  | MRedirIFile of mstringOrRef_val (*path*)

and mcmd_output = 
  | MCMDOStdOut
  | MCMDOStdErr
  | MCMDOFile of mstringOrRef_val (*path*)
  | MCMDOFileAppend of mstringOrRef_val (*path*)

and mcmd_outputerr = 
  | MCMDEStdOut
  | MCMDEStdErr
  | MCMDEFile of mstringOrRef_val (*path*)
  | MCMDEFileAppend of mstringOrRef_val (*path*)

and mcmd_input = 
  | MCMDIStdIn
  | MCMDIFile of mstringOrRef_val (*path*)

and mstringOrRef_val =
  | SORString of string located
  | SORExpr of mtype_val located

and mcmd_val = {
  mcm_cmd : string located;
  mcm_args : mstringOrRef_val list;
  mcm_output : mcmd_output; 
  mcm_outputerr : mcmd_outputerr; 
  mcm_input_src : mcmd_input; 
}

and mcmd_seq = 
  | SimpleCmd of mcmd_val located
  | ForkedCmd of mcmd_seq located (*symbol & *)
  | AndCmd of mcmd_seq located * mcmd_seq located (*&& : The right side of && will only be evaluated if the exit status of the left side is zero.*)
  | OrCmd of mcmd_seq located * mcmd_seq located (*|| : The right side of || will only be evaluated if the exit status of the left side is non-zero.*)
  | SequenceCmd of mcmd_seq located * mcmd_seq located (*; *)
  | PipedCmd of mcmd_seq located * mcmd_seq located (*; *)

and mbase_type = 
  | MTypeString
  | MTypeBool
  | MTypeInt
  | MTypeFloat
  | MTypeCmd

and mbase_type_val = 
  | MTypeStringVal of string located
  | MTypeBoolVal of bool located
  | MTypeIntVal of int located
  | MTypeFloatVal of float located
  | MTypeCmdVal of mcmd_seq located

and mtype_field = {
  mtf_name : string located;
  mtf_type: msimple_type located;
  mtf_extend: string option;
}

and mtype_field_val = {
  mtfv_name : string located;
  mtfv_val: mtype_val located;
}

and mcomposed_type = {
  mct_name: string;
  mct_fields: mtype_field list;
  mct_internal_val: (string * mtype_val) list; (*(fonction_name * val (*should be of type MSimple_val (MFun_val) *)*)
}

and mcomposed_type_val = {
  mcv_module_def : string located option;
  mcv_fields: mtype_field_val located list;
}

and mref_val = {
  mrv_module : string option; (* None if curmodule *)
  mrv_varname : string list located; (*varname * fields ($toto.f1.f2 ) *)
  mrv_index: mtype_val located list option; (*in case we access a list element of the
                                    reference, such as in $a[$b][$c].
                                    $a will have mrv_index as Some [mref($b); 
                                                                    mref(c)]
                                    *)
}

(*
(*environment variable *)
and menvref_val = {
  mrv_varname : string ;
}
*)

and msimple_type =
  | MBase_type of mbase_type
  | MTuple_type of (msimple_type located) list
  | MList_type of msimple_type located
  | MOption_type of msimple_type located
  | MSet_type of msimple_type located
  | MMap_type of msimple_type located * msimple_type located
  | MFun_type of msimple_type located list * msimple_type located (*arguments type, ret type *)
  | MRef_type of mref_val
  | MAll_type of string(*ocaml 'a , the int is only an identifier*)
  | MUnit 



and msimple_type_val = 
  | MBase_val of mbase_type_val 
  | MTuple_val of mtype_val located list located
  | MList_val of mtype_val located list located
  | MEmpty_val
  | MNone_val 
  | MSome_val of mtype_val located
  | MSet_val of  mtype_val located list located
  | MMap_val of (mtype_val located * mtype_val located) list located(*(key * value list ) Type info will come next. *)
  | MFun_val of mfunarg list located * mtype_val located (* args name * body_expr *)

and mfunarg = 
  | MBaseArg of string located
  | MTupleArg of mfunarg list located

and mtype =
  | MComposed_type of mcomposed_type
  | MSimple_type of msimple_type located

and mtype_val = 
  | MComposed_val of mcomposed_type_val
  | MSimple_val of msimple_type_val
  | MRef_val of mref_val located * mtype_val located list (*module, varname args*)
  | MEnvRef_val of string located (*environment variable*)
  | MBasicFunBody_val of m_expr_operation located * mtype_val located list
  | MBind_val of mbinding
  | MIf_val of mtype_val located * mtype_val located * mtype_val located
  | MComp_val of mcomp_op located * mtype_val located * mtype_val located (* comp_op * left_expr * right_expr *)
  | MBody_val of mtype_val located list (*this is used to express complex body using unit
                          * mtype for side effect.
                          * For exemple like "printf "aa";; true" 
                          * *)

and mcomp_op = 
  | Egal      (* == *)
  | NotEqual (* != *)
  | LessThan  (* < *)
  | LessOrEq  (* <= *)
  | GreaterThan (* > *)
  | GreaterOrEq (* >= *)

and mbinding = {
  mbd_name : var_decl; 
  mbd_value: mtype_val located;
  mbd_body: mtype_val located;
}

and m_expr_operation =
  | MConcatenation
  | MAddition 
  | MAdditionFloat
  | MSoustraction
  | MSoustractionFloat
  | MMultiplication
  | MMultiplicationFLoat
  | MDivision
  | MDivisionFloat
  | MModulo
  | MModuloFloat
  | MWithList
  | MWithSet
  | MWithMap
  | MWithoutSet
  | MWithoutMap
  | MHasSet
  | MHasMap
(*   | IntAbsolute  *)


and mprogram = {
  mpg_types : mtype StringMap.t;
  mpg_topvar: mvar list;
  mpg_topcal : mtype_val located;

}

and mmodultype = 
  | MUserMod of mprogram
  | MSystemMod of string (*module name, unparsed*)

and fullprog= {
  mfp_mainprog : mprogram ;
  mfp_progmodules : mmodultype IntMap.t ; 
  mfp_module_dep : IntSet.t IntMap.t; (*For a module i, give the set of module used as dependancy*)
  mfp_progmap : int StringMap.t;  (*For each module name, return its integer name.*)
  mfp_progmap_debug : string IntMap.t; 
}


and mprocess = {
  mps : string list; (*TODO*)
}


(* END TYPES *)
(* FUNCTIONS *)

val basetype_to_str : mbase_type -> string

val ref_to_string : mref_val -> string

val dump_mtype : mtype -> unit

val dump_var :mvar -> unit

val dump_fullprog : fullprog -> unit

(* END FUNCTIONS *)
