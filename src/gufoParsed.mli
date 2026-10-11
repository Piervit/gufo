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

(* =====================================================================
 * GUFO ABSTRACT SYNTAX TREE (AST) TYPES
 * =====================================================================
 *
 * This section defines all the types used to represent Gufo programs.
 * The hierarchy is organized in three layers:
 *
 *   1. TYPE DESCRIPTORS (static)   - What a variable IS at compile time
 *   2. RUNTIME VALUES (dynamic)    - What a variable CONTAINS at runtime
 *   3. SHARED STRUCTURES           - Cross-cutting descriptors used by both
 *
 * HIERARCHY:
 *
 *   1. TYPE DESCRIPTORS (static typing)
 *      mtype                    - Top-level type (simple or composed)
 *       - mcomposed_type          - User-defined struct types
 *       - msimple_type            - Type constructors
 *            - mbase_type             - Primitives (string, bool, int, float, cmd)
 *            - MTuple_type            - Tuple types
 *            - MList_type             - List types
 *            - MOption_type           - Option types
 *            - MSet_type              - Set types
 *            - MMap_type              - Map types
 *            - MFun_type              - Function types
 *            - MRef_type              - Reference types (uses mref_val)
 *
 *   2. RUNTIME VALUES (dynamic values and expressions)
 *      mtype_val                - Top-level value
 *       - mcomposed_type_val        - Struct instances with field values
 *       - msimple_type_val          - Simple values
 *            - mbase_type_val           - Primitives (string, bool, int, float, cmd)
 *            - MTuple_val               - Tuple values
 *            - MList_val                - List values
 *            - MSome_val / MNone_val    - Option values
 *            - MSet_val                 - Set values
 *            - MMap_val                 - Map values
 *            - MFun_val                 - Function closures
 *       - Expression values (control flow and operations):
 *            - MRef_val                 - Variable references (uses mref_val)
 *            - MEnvRef_val              - Environment variable references
 *            - MBasicFunBody_val        - Basic operations (+, -, etc.)
 *            - MBind_val                - Let/bind expressions
 *            - MIf_val                  - Conditional expressions
 *            - MComp_val                - Comparison expressions
 *            - MBody_val                - Sequence of expressions
 *
 * =====================================================================
 *)

type mvar = {
  mva_name: var_decl ; 
  mva_value: mtype_val;
}

and var_decl = 
(* Variable declaration types. 
 * MBaseDecl: simple variable name (e.g., $x)
 * MTupDecl: tuple destructuring (e.g., $a -- $b) *)
  | MBaseDecl of string 
  | MTupDecl of var_decl list


and mcmd_redir =  
(* Command output redirection types for parsing. These are converted to mcmd_output during parsing. *)
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
(* Represents a Unix command with its arguments and I/O redirections. *)
  mcm_cmd : string;          (* The command name (e.g., "ls", "grep") *)
  mcm_args : mstringOrRef_val list;  (* List of arguments (can be strings or expressions) *)
  mcm_output : mcmd_output;   (* Where standard output goes *)
  mcm_outputerr : mcmd_outputerr;  (* Where standard error goes *)
  mcm_input_src : mcmd_input;    (* Where input comes from *)
}

and mcmd_seq = 
(* Command sequencing and control operators. *)
  | SimpleCmd of mcmd_val          (* Single command *)
  | ForkedCmd of mcmd_seq          (* & - run command in background *)
  | AndCmd of mcmd_seq * mcmd_seq   (* && - conditional execution (right runs only if left succeeds) *)
  | OrCmd of mcmd_seq * mcmd_seq    (* || - conditional execution (right runs only if left fails) *)
  | SequenceCmd of mcmd_seq * mcmd_seq  (* ; - sequential execution *)
  | PipedCmd of mcmd_seq * mcmd_seq    (* | - pipe output of left to input of right *)

and mfile_val = {
  mfv_path:string;
}

and mbase_type = 
(* Primitive/basic types for type annotations (static types). *)
  | MTypeString    (* String type *)
  | MTypeBool      (* Boolean type (true/false) *)
  | MTypeInt       (* Integer type *)
  | MTypeFloat     (* Floating-point number type *)
  | MTypeCmd       (* Command type - represents Unix command execution *)

and mbase_type_val = 
(* Primitive values - the actual runtime data for basic types. 
 * Each constructor holds the concrete value of that type. *)
  | MTypeStringVal of string    (* String value: "hello" *)
  | MTypeBoolVal of bool        (* Boolean value: true, false *)
  | MTypeIntVal of int          (* Integer value: 42 *)
  | MTypeFloatVal of float       (* Float value: 3.14 *)
  | MTypeCmdVal of mcmd_seq      (* Command value: represents a Unix command pipeline *)

and mtype_field = {
(* Describes a field in a user-defined composite type (struct). *)
  mtf_name : string;       (* Field name *)
  mtf_type: msimple_type;  (* Type of this field *)
  mtf_extend: string option;  (* Optional parent type name for inheritance *)
}

and mtype_field_val = {
(* Runtime value of a struct field - pairs field name with its actual value. *)
  mtfv_name : string;    (* Field name *)
  mtfv_val: mtype_val;   (* Actual value of this field *)
}

and mcomposed_type = {
(* Describes a user-defined composite type (struct/type). 
 * Structs are like records with named, typed fields. *)
  mct_name: string;                    (* Type name *)
  mct_fields: mtype_field list;        (* List of field definitions *)
  mct_internal_val: (string * mtype_val) list;  (* Internal function implementations for this type *)
}

and mcomposed_type_val = {
(* Runtime representation of a struct instance. 
 * Contains the actual values for each field of the struct. *)
  mcv_module_def : string option;  (* Module where this struct type was defined *)
  mcv_fields: mtype_field_val list;   (* List of field names and their actual values *)
}

and mref_val = {
(* Reference to a variable, possibly qualified with module, fields, and indices. 
 * This is used to represent variable access in the AST. *)
  mrv_module : string option;        (* None = current module, Some name = other module *)
  mrv_varname : string list;         (* Variable name path: ["var"] or ["mod"; "var"] for module.var *)
  mrv_index: mtype_val list option;  (* List index expressions: Some [i; j] for $var[i][j] *)
}

(*
(* Environment variable reference - simply holds the variable name as a string. *)
and menvref_val = {
  mrv_varname : string;  (* Environment variable name (e.g., "PATH", "HOME") *)
}
*)

and msimple_type =
(* Type descriptors - static type annotations without actual values.
 * These describe the shape/type of a value, not the value itself. 
 *)
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
(* Runtime values for simple types. These hold actual data. These are the
concrete values that exist at runtime.*)
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
(* Function argument declaration types for pattern matching. 
 * MBaseArg: simple named argument (e.g., $x)
 * MTupleArg: tuple destructuring pattern (e.g., $a -- $b) *)
  | MBaseArg of string
  | MTupleArg of mfunarg list

and mtype =
(* Top-level type descriptor.
 * Either a composed type (struct) or a simple type (primitive, list, etc.). 
 * This is the main type for static type annotations. *)
  | MComposed_type of mcomposed_type
  | MSimple_type of msimple_type

and mtype_val = 
  | MComposed_val of mcomposed_type_val
(* Top-level runtime value.
 * This is the main type for representing Gufo values at runtime.
 * It can be a composed value (struct), simple value (primitive), reference,
 * environment variable, function body, binding, conditional, comparison, or sequence. 
 *)
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
(*and mpos = 
  {
    mps_line : int;
    mps_col : int;
  }

and mtype_val_with_pos =
  mtype_val * pos
*)
and mcomp_op = 
(* Comparison operators for conditional expressions and filtering. *)
  | Egal      (* == *)
  | NotEqual (* != *)
  | LessThan  (* < *)
  | LessOrEq  (* <= *)
  | GreaterThan (* > *)
  | GreaterOrEq (* >= *)

and mbinding = 
(* Represents a let-binding (variable declaration with initialization).
 * mbd_name: the variable being declared, mbd_value: its initial value, mbd_body: expression in scope 
 *)
{
  mbd_name : var_decl;
  mbd_value: mtype_val;
  mbd_body: mtype_val ;
}

and m_expr_operation =
(* Basic operations that can be performed on values.
 * Arithmetic: MAddition, MAdditionFloat, MSoustraction, MSoustractionFloat, etc.
 * String: MConcatenation\n * Collection: MWithList, MWithSet, MWithMap, MWithoutSet, MWithoutMap
 * Membership: MHasSet, MHasMap 
 *)
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

(** A scope contains types and variables*)
and mscope = {
(* Represents a lexical scope for type and variable visibility.
 * msc_father: parent scope (None for top-level), msc_vars: variables in this scope,
 * msc_type: types defined in this scope (always MComposed_type except toplevel) 
 *)
  msc_father : mscope option; (* None when toplevel*)
  msc_vars : mvar list;
  msc_type : mtype list; (* always MComposed_type except for toplevel*) 
}

and mshell_state = {
(* Represents the shell execution state.
 * Currently only tracks the current working directory. *)
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
  mfp_progmap : int StringMap.t;  (*For each module name, return its integer name.*)
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

val dump_fullprog : fullprog -> unit

(* END FUNCTIONS *)
