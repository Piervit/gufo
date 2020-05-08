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

type lexing_position = Stdlib.Lexing.position = {
  pos_fname : string ;
  pos_lnum  : int ;
  pos_bol   : int ;
  pos_cnum  : int ;
}

type pars_position = {
  ppos_start : lexing_position;
  ppos_end : lexing_position;
}


and 'a located = {
    loc_val : 'a ;
    loc_pos : pars_position;
}

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
  | SequenceCmd of mcmd_seq located * mcmd_seq located(*; *)
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
  mtf_name : string;
  mtf_type: msimple_type;
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
  | MTuple_val of mtype_val located list
  | MList_val of mtype_val located list
  | MEmpty_val
  | MNone_val 
  | MSome_val of mtype_val located
  | MSet_val of  mtype_val located list
  | MMap_val of (mtype_val located * mtype_val located) list (*(key * value list ) Type info will come next. *)
  | MFun_val of mfunarg list * mtype_val located (* args name * body_expr *)

and mfunarg = 
  | MBaseArg of string located
  | MTupleArg of mfunarg list located

and mtype =
  | MComposed_type of mcomposed_type
  | MSimple_type of msimple_type

and mtype_val = 
  | MComposed_val of mcomposed_type_val
  | MSimple_val of msimple_type_val
  | MRef_val of mref_val located * mtype_val located list (*module, varname args*)
  | MEnvRef_val of string located (*environment variable*)
  | MBasicFunBody_val of m_expr_operation * mtype_val located list
  | MBind_val of mbinding
  | MIf_val of mtype_val located * mtype_val located * mtype_val located
  | MComp_val of mcomp_op * mtype_val located * mtype_val located (* comp_op * left_expr * right_expr *)
  | MBody_val of mtype_val located list (*this is used to express complex body using unit
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
  mfp_progmap : int StringMap.t; 
  mfp_progmap_debug : string IntMap.t; 
}


and mprocess = {
  mps : string list; (*TODO*)
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
    | None -> String.concat "." var.mrv_varname.loc_val
    | Some modul -> String.concat "." (modul :: var.mrv_varname.loc_val)


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


let print_loc locpos = 
  let str_pos = 
    Printf.sprintf "Starting at line %d char %d until line %d char %d" 
                    locpos.ppos_start.pos_lnum 
                    (locpos.ppos_start.pos_cnum - locpos.ppos_start.pos_bol)
                    locpos.ppos_end.pos_lnum
                    (locpos.ppos_end.pos_cnum - locpos.ppos_end.pos_bol)
  in 
  print_string str_pos


let rec print_stringOrRef s = 
  match s with
    | SORString ls -> print_string ls.loc_val 
    | SORExpr s -> dump_mtype_val s.loc_val

and dump_cmd_val cmdval = 
  let dump_cmd cmd = 
    print_string cmd.mcm_cmd.loc_val; 
    print_space ();
    List.iter (fun a -> 
        match a with 
          | SORString a -> print_string a.loc_val;  print_space ()
          | SORExpr a -> dump_mtype_val a.loc_val; print_space ())
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
    | SimpleCmd cmdval -> dump_cmd cmdval.loc_val ; print_space () ; print_loc cmdval.loc_pos; print_space ()
    | ForkedCmd cmdval -> dump_cmd_val cmdval.loc_val; print_string " & "
    | AndCmd (cmdval1, cmdval2)-> dump_2op " && " cmdval1.loc_val cmdval2.loc_val
    | OrCmd (cmdval1, cmdval2)-> dump_2op " || " cmdval1.loc_val cmdval2.loc_val
    | SequenceCmd (cmdval1, cmdval2)-> dump_2op " ; " cmdval1.loc_val cmdval2.loc_val
    | PipedCmd (cmdval1, cmdval2)-> dump_2op " | " cmdval1.loc_val cmdval2.loc_val
  and dump_arg arg = 
    match arg with
      | MBaseArg arg -> print_string arg.loc_val; print_space ()
      | MTupleArg arglst -> 
          print_string "(";
          List.iter (fun arg -> dump_arg arg; print_string " -- ") arglst.loc_val;
          print_string ")"
  and dump_mtype_val varval = 
    let print_call op args = 
      print_string "( ";
              print_string op;
              List.iter (fun arg -> print_string " "; dump_mtype_val arg.loc_val) args;
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
           print_string "some"; dump_mtype_val v.loc_val; print_space () 
         | MBase_val MTypeStringVal v ->
         print_string v.loc_val; print_space () 
         | MBase_val MTypeBoolVal b ->
            (match b.loc_val with
              | true -> print_string "True"; print_space () 
              | false -> print_string "False"; print_space () 
            )
         | MBase_val MTypeIntVal v ->
         print_int v.loc_val ; print_space () 
         | MBase_val MTypeFloatVal v -> 
         print_float v.loc_val ; print_space () 
         | MBase_val MTypeCmdVal v ->
         dump_cmd_val v.loc_val 
         | MTuple_val lvals ->
           print_string "tuple [ ";
           List.iter (fun v -> dump_mtype_val v.loc_val) lvals;
           print_string "] "
         | MList_val lvals ->
           print_string "list [ ";
           List.iter (fun arg -> dump_mtype_val arg.loc_val; print_string ", ") lvals;
           print_string "] "
         | MSet_val vals ->
           print_string "set [ ";
           List.iter (fun arg -> dump_mtype_val arg.loc_val; print_string ", ") vals;
           print_string "] ";
         | MMap_val vals ->
           print_string "map[ ";
           List.iter (fun (key,aval) -> dump_mtype_val key.loc_val ;print_string ": ";dump_mtype_val aval.loc_val; print_string ", ") vals;
           print_string "] ";

         | MFun_val (fargs, fexpr) ->
           print_string "fun ";
           List.iter dump_arg fargs;
           print_string "= ";
           dump_mtype_val fexpr.loc_val
        )
     |  MComposed_val cvar ->
        (match cvar.mcv_module_def with
          | None -> print_space ()
          | Some s -> print_string s.loc_val;
                      print_string "."
        );
        print_string "{";
        List.iter (fun fd -> 
          print_string fd.loc_val.mtfv_name.loc_val;
          print_string " = ";
          dump_mtype_val fd.loc_val.mtfv_val.loc_val;
          print_string " , "
          ) cvar.mcv_fields;
        print_string "}"
     |  MRef_val (var, args) ->
         (match args with
          | [] ->
            print_string " ref "; print_string (ref_to_string var.loc_val)
          | args -> print_call (ref_to_string var.loc_val) args
          )
     |  MEnvRef_val (var) ->
            print_string " ref "; print_string var.loc_val
     | MBind_val bd ->
         print_string "let ";
         dump_var_decl bd.mbd_name;
         print_string " = ";
         dump_mtype_val bd.mbd_value.loc_val;
         print_string " in ";
         dump_mtype_val bd.mbd_body.loc_val
     | MBasicFunBody_val (fop, args) ->
         (match fop with
            | MConcatenation -> 
                print_call "^" args
            | MAddition -> 
                print_call "+" args
            | MAdditionFloat -> 
                print_call "+." args
            | MSoustraction ->
                print_call "-" args
            | MSoustractionFloat ->
                print_call "-." args
            | MMultiplication ->
                print_call "*" args
            | MMultiplicationFLoat ->
                print_call "*." args
            | MDivision ->
                print_call "%" args
            | MDivisionFloat ->
                print_call "%." args
            | MModulo ->
                print_call " mod " args
            | MModuloFloat ->
                print_call " mod. " args
            | MWithList ->
                print_call " With " args
            | MWithSet ->
                print_call " SWith " args
            | MWithMap ->
                print_call " MWith " args
            | MWithoutSet ->
                print_call " SWout " args
            | MWithoutMap ->
                print_call " MWout " args
            | MHasSet ->
                print_call " SHas? " args
            | MHasMap ->
                print_call " MMHas? " args
         )
      | MIf_val (cond, thn, els ) ->
          print_string "if {";
          dump_mtype_val cond.loc_val;
          print_string "} then{ ";
          dump_mtype_val thn.loc_val;
          print_string "}else{ ";
          dump_mtype_val els.loc_val;
          print_string "} "
      | MComp_val (comp_op, left, right) ->
          let print_comp op = 
            dump_mtype_val left.loc_val;
            print_string op;
            dump_mtype_val right.loc_val
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
          List.iter (fun bd -> dump_mtype_val bd.loc_val; print_string " ;; ") bodylst
    )
and dump_var_decl decl = 
  match decl with 
    | MBaseDecl str -> print_string str.loc_val
    | MTupDecl tup -> print_string "("; 
                      List.iter 
                        (fun decl -> dump_var_decl decl;
                                     print_string " -- ") 
                        tup; 
                      print_string ")"

and dump_var var =
  open_hovbox 2;
  dump_var_decl var.mva_name.loc_val;
  print_string " = ";
  print_space ();
  dump_mtype_val var.mva_value.loc_val;
  close_box ();
  print_newline ()


let dump_fullprog prog = 

      debug_info ("-- mainprog --");
      
      debug_info ("---- topcall ----");
      dump_mtype_val prog.mfp_mainprog.mpg_topcal.loc_val;
      

      debug_info ("-- mfp_progmodules --");
      IntMap.iter
        (fun i modu -> debug_info (sprintf "%d %s \n" i 
                        (match modu with 
                          | MSystemMod strmod -> strmod
                          | _ -> "user mod"
                        )
                      )
        )
        prog.mfp_progmodules;
      debug_info ("-- mfp_module_dep--");
      IntMap.iter
        (fun i _ -> debug_info (sprintf "%d \n" i )
        )
        prog.mfp_module_dep;
      debug_info ("-- mfp_progmap --");
      StringMap.iter
        (fun modname i -> debug_info (sprintf "%s : %d \n" modname i )
        )
        prog.mfp_progmap;

      debug_info ("-- mfp_progmap_debug --");
      IntMap.iter
        (fun i modname -> debug_info (sprintf "%d : %s \n" i modname )
        )
        prog.mfp_progmap_debug




