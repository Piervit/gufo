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

exception SyntaxError of string
exception TypeError of string
exception ExecutionError of string
exception VarError of string
exception InternalError of string
exception UncatchedError of string
exception RuntimeError of string


(*TYPES *)

type mvar = {
  mva_name: var_decl ; 
  mva_value: mtype_val;
}

and var_decl = 
  | MBaseDecl of string 
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
  | SORString of string
  | SORExpr of mtype_val

and mcmd_val = {
  mcm_cmd : string;
  mcm_args : mstringOrRef_val list;
  mcm_output : mcmd_output; 
  mcm_outputerr : mcmd_outputerr; 
  mcm_input_src : mcmd_input; 
}

and mcmd_seq = 
  | SimpleCmd of mcmd_val
  | ForkedCmd of mcmd_seq (*symbol & *)
  | AndCmd of mcmd_seq * mcmd_seq (*&& : The right side of && will only be evaluated if the exit status of the left side is zero.*)
  | OrCmd of mcmd_seq * mcmd_seq (*|| : The right side of || will only be evaluated if the exit status of the left side is non-zero.*)
  | SequenceCmd of mcmd_seq * mcmd_seq (*; *)
  | PipedCmd of mcmd_seq * mcmd_seq (*; *)

and mfile_val = {
  mfv_path:string;
}

and mbase_type = 
  | MTypeString
  | MTypeBool
  | MTypeInt
  | MTypeFloat
  | MTypeCmd

and mbase_type_val = 
  | MTypeStringVal of string
  | MTypeBoolVal of bool
  | MTypeIntVal of int
  | MTypeFloatVal of float
  | MTypeCmdVal of mcmd_seq
(*      | MTypeFileVal of mfile_val *) 

and mtype_field = {
  mtf_name : string;
  mtf_type: msimple_type;
  mtf_extend: string option;
}

and mtype_field_val = {
  mtfv_name : string;
  mtfv_val: mtype_val;
}

and mcomposed_type = {
  mct_name: string;
  mct_fields: mtype_field list;
  mct_internal_val: (string * mtype_val) list; (*(fonction_name * val (*should be of type MSimple_val (MFun_val) *)*)
}

and mcomposed_type_val = {
  mcv_module_def : string option;
  mcv_fields: mtype_field_val list;
}

and mref_val = {
  mrv_module : string option; (* None if curmodule *)
  mrv_varname : string list; (*varname * fields ($toto.f1.f2 ) *)
  mrv_index: mtype_val list option; (*in case we access a list element of the
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
  | MTuple_type of msimple_type list
  | MList_type of msimple_type
  | MOption_type of msimple_type
  | MSet_type of msimple_type
  | MMap_type of msimple_type * msimple_type
  | MFun_type of msimple_type list * msimple_type (*arguments type, ret type *)
  | MRef_type of mref_val
  | MAll_type of string(*ocaml 'a , the int is only an identifier*)
  | MUnit 



and msimple_type_val = 
  | MBase_val of mbase_type_val 
  | MTuple_val of mtype_val list
  | MList_val of mtype_val list
  | MEmpty_val
  | MNone_val 
  | MSome_val of mtype_val
  | MSet_val of  mtype_val list
  | MMap_val of (mtype_val * mtype_val) list (*(key * value list ) Type info will come next. *)
  | MFun_val of mfunarg list * mtype_val (* args name * body_expr *)

and mfunarg = 
  | MBaseArg of string
  | MTupleArg of mfunarg list

and mtype =
  | MComposed_type of mcomposed_type
  | MSimple_type of msimple_type

and mtype_val = 
  | MComposed_val of mcomposed_type_val
  | MSimple_val of msimple_type_val
  | MRef_val of mref_val * mtype_val list (*module, varname args*)
  | MEnvRef_val of string (*environment variable*)
  | MBasicFunBody_val of m_expr_operation * mtype_val list
  | MBind_val of mbinding
  | MIf_val of mtype_val * mtype_val * mtype_val
  | MComp_val of mcomp_op * mtype_val * mtype_val (* comp_op * left_expr * right_expr *)
  | MBody_val of mtype_val list (*this is used to express complex body using unit
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
  mbd_value: mtype_val;
  mbd_body: mtype_val ;
}

and m_expr_operation =
  | MAddition 
  | MSoustraction
  | MMultiplication
  | MDivision
  | MModulo
  | MWith
  | MWithout
  | MHas (*$a has? $b ->return true if $b if in the set or map $a.*)
(*   | IntAbsolute  *)

(** A scope contains types and variables*)
and mscope = {
  msc_father : mscope option; (* None when toplevel*)
  msc_vars : mvar list;
  msc_type : mtype list; (* always MComposed_type except for toplevel*) 
}

and mshell_state = {
  mst_curdir : string; 
}

and mprogram = {
  mpg_types : mtype StringMap.t;
  mpg_topvar: mvar list;
  mpg_topcal : mtype_val ;

}



and mmodultype = 
  | MUserMod of mprogram
  | MSystemMod of string (*module name, unparsed*)

and fullprog= {
  mfp_mainprog : mprogram ;
  mfp_progmodules : mmodultype IntMap.t ;
  mfp_module_dep : IntSet.t IntMap.t; (*For a module i, give the set of module used as dependancy*)
  mfp_progmap : int StringMap.t; 
  mfp_progmap_debug : string IntMap.t; 
}


and mprocess = {
  mps : string list; (*TODO*)
}

and mshell = {
  mpr_scopes : mscope list;
  mpr_process : mprocess list; (* this is a shell, it can be running process*)
  mpr_shell_state : mshell_state; 
}


(* END TYPES *)
(* FUNCTIONS *)

val basetype_to_str : mbase_type -> string

val ref_to_string : mref_val -> string

val dump_mtype : mtype -> unit

val dump_var :mvar -> unit

val mval_to_cmd: mtype_val -> mcmd_seq

(* END FUNCTIONS *)
