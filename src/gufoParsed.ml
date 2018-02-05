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


open Format
exception SyntaxError of string
exception TypeError of string
exception VarError of string
exception ExecutionError of string
exception InternalError of string
exception UncatchedError of string
exception RuntimeError of string


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
  | MBasicFunBody_val of m_expr_operation * mtype_val list
  | MBind_val of mbinding
  | MIf_val of mtype_val * mtype_val * mtype_val
  | MComp_val of mcomp_op * mtype_val * mtype_val (* comp_op * left_expr * right_expr *)
  | MBody_val of mtype_val list (*this is used to express complex body using unit
                          * mtype for side effect.
                          * For exemple like "printf "aa";; true" 
                          * *)


and mcomp_op = 
  | Egal      (* == 
                Can be applied to every type    
              *)
  | NotEqual (* != 
                Can be applied to every type    
                *)
  | LessThan  (* < 
                Can be applied to numeric type (int, float)
              *)
  | LessOrEq  (* <= 
                Can be applied to numeric type (int, float)
              *)
  | GreaterThan (* > 
                Can be applied to numeric type (int, float)
                *)
  | GreaterOrEq (* >= 
                Can be applied to numeric type (int, float)
                *)

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

let basetype_to_str mybasetype = 
  match mybasetype with
    | MTypeString -> "string"
    | MTypeBool -> "bool"
    | MTypeInt -> "int"
    | MTypeFloat -> "float"
    | MTypeCmd -> "cmd"

let dump_base_type mybasetype = 
  match mybasetype with
    | MTypeString ->
      print_string "string"
    | MTypeBool ->
      print_string "bool"
    | MTypeInt ->
      print_string "int"
    | MTypeFloat ->
      print_string "float"
    | MTypeCmd ->
      print_string "cmd"

let ref_to_string var =
  match var.mrv_module  with
    | None -> String.concat "." var.mrv_varname
    | Some modul -> String.concat "." (modul :: var.mrv_varname)

let mval_to_cmd sv = 
  match sv with 
    | MSimple_val (MBase_val (MTypeCmdVal cmdseq))
      -> cmdseq
    | _ -> assert false


let rec dump_mtype_short mytype = 
      match mytype with
        | MBase_type mybasetype -> 
            dump_base_type mybasetype;
        | MTuple_type types -> 
            print_string "tuple [ "; 
            List.iter (fun ty -> print_space (); 
                       dump_mtype_short ty) types;
            print_string " ] "; 
        | MList_type mybasetype -> 
            print_string "list "; dump_mtype_short mybasetype
        | MOption_type mybasetype -> 
            print_string "option "; dump_mtype_short mybasetype
        | MSet_type mybasetype -> 
            print_string "set "; dump_mtype_short mybasetype
        | MMap_type (keytype, maptype) -> 
            print_string "map["; dump_mtype_short keytype; print_string "] "; 
            dump_mtype_short maptype
        | MFun_type (args, ret_type) -> 
            print_string "fun ("; List.iter (fun arg -> dump_mtype_short arg; print_string " -> ") args ; 
            dump_mtype_short ret_type; 
            print_string ")"
        | MRef_type ref_val -> print_string (ref_to_string ref_val)
        | MAll_type i -> print_string "allType "; print_string i
        | MUnit -> print_string "unit"

let dump_mtype_field mytypefield =
    open_hovbox 2;
    print_string mytypefield.mtf_name;
    print_string ": ";
    dump_mtype_short mytypefield.mtf_type;
    (match mytypefield.mtf_extend with 
      | None -> print_space ();
      | Some s -> (
        print_string " (extend ";
        print_string s;
        print_string " ) ")
    );
    print_string " ) ";
    close_box ()

let rec dump_mtype mytype = 
    open_hovbox 2;
      (match mytype with
        | MComposed_type mycomptype -> 
            print_string mycomptype.mct_name;
            print_string ": ";
            print_newline();
            open_hovbox 2;
            List.iter (fun fd -> dump_mtype_field fd; print_newline()) mycomptype.mct_fields;
            close_box ()
        | MSimple_type MBase_type mybasetype -> 
          dump_base_type mybasetype;
          print_newline()
        | MSimple_type MList_type mybasetype -> 
          print_string "list"; dump_mtype_short mybasetype ;
          print_newline()
        | MSimple_type MOption_type mybasetype -> 
          print_string "option"; dump_mtype_short mybasetype;
          print_newline()
        | MSimple_type MSet_type mybasetype -> 
          print_string "set"; dump_mtype_short mybasetype;
          print_newline()
        | MSimple_type MMap_type (keytype, maptype) -> 
            print_string "map["; dump_mtype_short keytype; print_string"] "; dump_mtype_short maptype;
          print_newline()
        | MSimple_type MFun_type (args, ret_type) -> 
          (print_string "fun ("; 
          List.iter (fun typ -> dump_mtype_short typ ; print_string " -> ") args; dump_mtype_short ret_type );
          print_string ")";
          print_newline();
        | MSimple_type MUnit ->
          print_string "()"
        | MSimple_type (MTuple_type typlst) ->
            print_string "(";
            dump_mtype (MSimple_type (List.hd typlst));
            List.iter (fun typ -> print_string ", "; dump_mtype (MSimple_type typ)) (List.tl typlst) ;
            print_string ")"
        | MSimple_type (MRef_type rval) -> 
            print_string (ref_to_string rval)
        | MSimple_type (MAll_type str) -> 
            print_string str

        );
    close_box ()


let rec print_stringOrRef s = 
  match s with
    | SORString s -> print_string s 
    | SORExpr s -> dump_var_val s

and dump_cmd_val cmdval = 
  let dump_cmd cmd = 
    print_string cmd.mcm_cmd; 
    print_space ();
    List.iter (fun a -> 
        match a with 
          | SORString a -> print_string a; print_space ()
          | SORExpr a -> dump_var_val a; print_space ())
      cmd.mcm_args;
    (match cmd.mcm_output with 
      | MCMDOStdErr -> print_string ">& "
      | MCMDOFile f -> print_string "> "; print_stringOrRef f
      | MCMDOFileAppend f -> print_string ">> "; print_stringOrRef f
      | _ -> ()
    );
    (match cmd.mcm_outputerr with 
      | MCMDEStdOut -> print_string "2>&1 "
      | MCMDEFile f -> print_string "2> "; print_stringOrRef f
      | MCMDEFileAppend f -> print_string "2>> "; print_stringOrRef f
      | _ -> ()
    );
    (match cmd.mcm_input_src with 
      | MCMDIFile f -> print_string "< " ;print_stringOrRef f
      | _ -> ()
    )
    
  in
  let dump_2op symbol operand1 operand2 =
    print_string "(" ; dump_cmd_val operand1; print_string symbol; dump_cmd_val operand2; print_string ")"
  in
  match cmdval with
    | SimpleCmd cmdval -> dump_cmd cmdval
    | ForkedCmd cmdval -> dump_cmd_val cmdval; print_string " & "
    | AndCmd (cmdval1, cmdval2)-> dump_2op " && " cmdval1 cmdval2
    | OrCmd (cmdval1, cmdval2)-> dump_2op " || " cmdval1 cmdval2
    | SequenceCmd (cmdval1, cmdval2)-> dump_2op " ; " cmdval1 cmdval2
    | PipedCmd (cmdval1, cmdval2)-> dump_2op " | " cmdval1 cmdval2
  and dump_arg arg = 
    match arg with
      | MBaseArg arg -> print_string arg; print_space ()
      | MTupleArg arglst -> 
          print_string "(";
          List.iter (fun arg -> dump_arg arg; print_string " -- ") arglst;
          print_string ")"
  and dump_var_val varval = 
    let print_call op args = 
      print_string "( ";
              print_string op;
              List.iter (fun arg -> print_string " "; dump_var_val arg) args;
              print_string " ) ";
    in
    (match varval with

     |  MSimple_val svar ->
        (match svar with 
         |  MEmpty_val ->
           print_string "()"

         |  MNone_val ->
           print_string "none"; print_space () 
         |  MSome_val v->
           print_string "some"; dump_var_val v; print_space () 
         | MBase_val MTypeStringVal v ->
         print_string v; print_space () 
         | MBase_val MTypeBoolVal true ->
         print_string "True"; print_space () 
         | MBase_val MTypeBoolVal false ->
         print_string "False"; print_space () 
         | MBase_val MTypeIntVal v ->
         print_int v ; print_space () 
         | MBase_val MTypeFloatVal v -> 
         print_float v ; print_space () 
         | MBase_val MTypeCmdVal v ->
         dump_cmd_val v 
         | MTuple_val lvals ->
           print_string "tuple [ ";
           List.iter dump_var_val lvals;
           print_string "] "
         | MList_val lvals ->
           print_string "list [ ";
           List.iter (fun arg -> dump_var_val arg; print_string ", ") lvals;
           print_string "] "
         | MSet_val vals ->
           print_string "set [ ";
           List.iter (fun arg -> dump_var_val arg; print_string ", ") vals;
           print_string "] ";
         | MMap_val vals ->
           print_string "map[ ";
           List.iter (fun (key,aval) -> dump_var_val key ;print_string ": ";dump_var_val aval; print_string ", ") vals;
           print_string "] ";

         | MFun_val (fargs, fexpr) ->
           print_string "fun ";
           List.iter dump_arg fargs;
           print_string "= ";
           dump_var_val fexpr
        )
     |  MComposed_val cvar ->
        (match cvar.mcv_module_def with
          | None -> print_space ()
          | Some s -> print_string s;
                      print_string "."
        );
        print_string "{";
        List.iter (fun fd -> 
          print_string fd.mtfv_name;
          print_string " = ";
          dump_var_val fd.mtfv_val;
          print_string " , "
          ) cvar.mcv_fields ;
        print_string "}"
     |  MRef_val (var, args) ->
         (match args with
          | [] ->
            print_string " ref "; print_string (ref_to_string var)
          | args -> print_call (ref_to_string var) args
          )
     | MBind_val bd ->
         print_string "let ";
         dump_var_decl bd.mbd_name;
         print_string " = ";
         dump_var_val bd.mbd_value;
         print_string " in ";
         dump_var_val bd.mbd_body
     | MBasicFunBody_val (fop, args) ->
         (match fop with
            | MAddition -> 
                print_call "+" args
            | MSoustraction ->
                print_call "-" args
            | MMultiplication ->
                print_call "*" args
            | MDivision ->
                print_call "/" args
            | MModulo ->
                print_call " mod " args
            | MWith ->
                print_call " with " args
            | MWithout ->
                print_call " wout " args
         )
      | MIf_val (cond, thn, els ) ->
          print_string "if {";
          dump_var_val cond;
          print_string "} then{ ";
          dump_var_val thn;
          print_string "}else{ ";
          dump_var_val els;
          print_string "} "
      | MComp_val (comp_op, left, right) ->
          let print_comp op = 
            dump_var_val left;
            print_string op;
            dump_var_val right
          in
          (match comp_op with 
            | Egal ->
                print_comp " == " 
            | NotEqual ->
                print_comp " != " 
            | LessThan ->
                print_comp " < " 
            | LessOrEq ->
                print_comp " <= " 
            | GreaterThan ->
                print_comp " > " 
            | GreaterOrEq ->
                print_comp " >= " 
          )
      | MBody_val bodylst -> 
          List.iter (fun bd -> dump_var_val bd; print_string " ;; ") bodylst
    )
and dump_var_decl decl = 
  match decl with 
    | MBaseDecl str -> print_string str
    | MTupDecl tup -> print_string "("; 
                      List.iter 
                        (fun decl -> dump_var_decl decl;
                                     print_string " -- ") 
                        tup; 
                      print_string ")"

and dump_var var =
  open_hovbox 2;
  dump_var_decl var.mva_name;
  print_string " = ";
  print_space ();
  dump_var_val var.mva_value;
  close_box ();
  print_newline ()
