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


module MCore =
struct
  open Format

  type motype_field = {
    motf_name : int;
    motf_type: motype_or;
    motf_debugname : string;
  }
  
  and mocomposed_type = {
    moct_name: int;
    moct_fields: motype_field IntMap.t; 
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
   | MOFun_type of motype_or list * motype_or 
   | MOAll_type of int
   | MOUnit_type
   | MORef_type of int option * int * int * motype_or list
   | MOTupel_type of int option * int *int * motype_or list * int list
   
 and motype_or = 
   | MOUnique_type of motype 
   | MOOr_type of motype list * int

  module SimpleCore = 
   struct 

      type mivar = {
        miva_name: int list; (*we have a list when the input is a tuple, for
        exemple in the case "let a,b= 5" then mva_name is ("a","b"). *)
        miva_value: mitype_val;
      }

      and mcSetEl = mitype_val

      and micmd_redir =  
        | MIRedirOStdOut
        | MIRedirOStdErr
        | MIRedirOFile of mistringOrRef_val (*path*)
        | MIRedirOFileAppend of mistringOrRef_val (*path*)
        | MIRedirEStdOut
        | MIRedirEStdErr
        | MIRedirEFile of mistringOrRef_val (*path*)
        | MIRedirEFileAppend of mistringOrRef_val (*path*)
        | MIRedirIStdIn
        | MIRedirIFile of mistringOrRef_val (*path*)

      and micmd_output = 
        | MICMDOStdOut
        | MICMDOStdErr
        | MICMDOFile of mistringOrRef_val (*path*)
        | MICMDOFileAppend of mistringOrRef_val (*path*)

      and micmd_outputerr = 
        | MICMDEStdOut
        | MICMDEStdErr
        | MICMDEFile of mistringOrRef_val (*path*)
        | MICMDEFileAppend of mistringOrRef_val (*path*)

      and micmd_input = 
        | MICMDIStdIn
        | MICMDIFile of mistringOrRef_val (*path*)

      and mistringOrRef_val =
        | MISORString of string
        | MISORExpr of mitype_val



      and micmd_val = {
        micm_cmd : string;
        micm_args : mistringOrRef_val list;
        micm_res : int option; 
        micm_output : micmd_output; 
        micm_outputerr : micmd_outputerr; 
        micm_input_src : micmd_input; 
        micm_input : string option; 
        micm_print: string option;
        micm_print_error: string option;
        micm_print_std: string option;
      }

      and micmd_seq = 
        | MISimpleCmd of micmd_val
        | MIForkedCmd of micmd_seq 
        | MIAndCmd of micmd_seq * micmd_seq 
        | MIOrCmd of micmd_seq * micmd_seq 
        | MISequenceCmd of micmd_seq * micmd_seq 
        | MIPipedCmd of micmd_seq * micmd_seq 
 
      and mibase_type_val = 
        | MITypeStringVal of string
        | MITypeBoolVal of bool
        | MITypeIntVal of int
        | MITypeFloatVal of float
        | MITypeCmdVal of micmd_seq
      
     
      and micomposed_type_val = {
        micv_module_def : int option; 
        micv_fields: mitype_val IntMap.t; 
        micv_resolved_type : int option *int ; 
      }
      
      and miref_val = {
        mirv_module : int option; 
        mirv_varname : int * (int option * int) list; 
        mirv_index: mitype_val list option;
        mirv_debugname: string;
      }
      
      and misimple_type_val = 
        | MIBase_val of mibase_type_val 
        | MITuple_val of mitype_val list
        | MIList_val of mitype_val list
        | MINone_val 
        | MISome_val of mitype_val
        | MIFun_val of int StringMap.t * mifunarg list * mitype_val 
        | MIEmpty_val
      
      and mifunarg = 
        | MIBaseArg of int
        | MITupleArg of mifunarg list
      
      and mitype_val = 
        | MISimple_val of misimple_type_val
        | MIComposed_val of micomposed_type_val
        | MIRef_val of miref_val * mitype_val list 
        | MIBasicFunBody_val of mi_expr_operation * mitype_val * mitype_val
        | MIBind_val of mibinding
        | MIIf_val of mitype_val * mitype_val * mitype_val
        | MIComp_val of micomp_op * mitype_val * mitype_val 
        | MIBody_val of mitype_val list

      and micomp_op = GufoParsed.mcomp_op

      and mibinding = {
        mibd_name : (int * int list) list;
        mibd_debugnames: string IntMap.t;
        mibd_value: mitype_val;
        mibd_body: mitype_val ;
      }

      and mi_expr_operation = GufoParsed.m_expr_operation
 
      and mimodule = {
        mimo_name : int;
        mimo_topvar : mivar list
      }
      
      and mishell_state = GufoParsed.mshell_state
      
      and misysmodulemvar = {
        mismv_name: string;
        mismv_intname: int;
        mismv_type: motype list * motype; 
        mismv_action: (mitype_val list -> mitype_val IntMap.t -> mitype_val); 
      }
      and misysmodule = {
        mism_types : motype list;
        mism_topvar: misysmodulemvar IntMap.t;
      }



      and  mimodultype = 
        | MIUserMod of miprogram
        | MISystemMod of misysmodule


      and fullprogopt= {
        mifp_mainprog : miprogram ;
        mifp_progmodules : mimodultype IntMap.t ;
        mifp_progmap : int StringMap.t;
      }

      and miprogram = {
        mipg_types : motype IntMap.t; 
        mipg_field_to_type: int IntMap.t; 
        mipg_topvar: mitype_val IntMap.t;
        mipg_modules: mimodule IntMap.t;
        mipg_topcal : mitype_val ;
        mipg_var2int: int StringMap.t ;
        mipg_ctype2int: int StringMap.t; 
        mipg_field2int: int StringMap.t;
        mipg_module2int: int StringMap.t;
      }


      and miprocess = GufoParsed.mprocess
 
      type t = mitype_val

      let rec mlist_compare ta tb = 
        let rec el_compare lsta lstb = 
          match lsta, lstb with
            | a::nlsta, b::nlstb ->
                (match type_compare a b with
                  | 0 -> el_compare nlsta nlstb
                  | i -> i
                )
            | [], [] -> 0
            | _ -> assert false 
        in
        match (List.length ta) - (List.length tb) with
          | 0 -> el_compare ta tb
          | i -> i 

      and mref_compare a b = 
        let rec comp_fields fa fb = 
          match fa, fb with
            | [], [] -> 0
            | (None, fa)::lsta, (Some _ , fb)::lstb -> 1
            | (Some _, fa)::lsta, (None , fb)::lstb -> -1
            | (None , fa)::lsta, (None , fb)::lstb -> 
                (match fa - fb with 
                 | 0 -> comp_fields lsta lstb
                 | i -> i 
                )
            | (Some moda, fa)::lsta, (Some modb, fb)::lstb -> 
                (match moda - modb, fa - fb with 
                  | 0,0 -> comp_fields lsta lstb
                  | 0,i -> i 
                  | y,i -> y 
                )
            | _ -> assert false
        in
        let comp_varname vnamea vnameb = 
          match vnamea, vnameb with
            | (i, lst), (ip, lstp) -> 
                (match i - ip  with
                  | 0 -> 
                      (match (List.length lst) - (List.length lstp) with
                        | 0 -> comp_fields lst lstp
                        | i -> i 
                      )
                  | i -> i 
                )
        in
        match a.mirv_module, b.mirv_module with
          | None, Some _ -> 1 
          | Some _, None -> -1
          | None, None -> 0
          | Some i, Some ip -> 
              (match i - ip with
                | 0 -> comp_varname a.mirv_varname b.mirv_varname
                | i -> i
              )
    

      and cmd_compare cmda cmdb = 
        let cmp_res resa resb = 
          match resa, resb with
            | Some _, None -> 1
            | None, Some _ -> -1
            | Some a, Some b when a != b -> compare a b
            | _,_ -> 0
        in
        let cmp_print printa printb = 
          match printa, printb with
            | Some _, None -> 1
            | None, Some _ -> -1
            | Some printa , Some printb when ((String.compare printa printb != 0)) -> String.compare printa printb
            | _ -> 0 
        in
          match String.compare cmda.micm_cmd cmdb.micm_cmd with
            | 0  -> 
                (match list_compare 
                  (fun arga argb -> 
                    match arga, argb with 
                      | MISORString a, MISORExpr b -> 1 
                      | MISORExpr a, MISORString b -> -1
                      | MISORString a, MISORString b -> 
                        String.compare a b
                      | MISORExpr a, MISORExpr b -> type_compare a b
                  ) cmda.micm_args cmdb.micm_args 
                with
                  | 0 ->
                      (match cmp_res cmda.micm_res cmdb.micm_res with
                        | 0 ->
                            (match cmp_print cmda.micm_print_error cmdb.micm_print_error with
                            | 0 ->
                              cmp_print cmda.micm_print_std cmdb.micm_print_std
                            | i -> i  
                            )
                        | i -> i 
                      )
                  | i -> i
                )
            | i -> i
      
      and cmd_seq_compare cmdseqa cmdseqb =
        match cmdseqa, cmdseqb with
          | MISimpleCmd cmda, MISimpleCmd cmdb ->
              cmd_compare cmda cmdb
          | MIForkedCmd cmda, MIForkedCmd cmdb ->
              cmd_seq_compare cmda cmdb
          | _, MISimpleCmd _ -> 1
          | MISimpleCmd _, _ -> -1 
          | _, MIForkedCmd _ -> 1
          | MIForkedCmd _, _ -> -1 
          | MIAndCmd (seqa1, seqa2), MIAndCmd (seqb1, seqb2) 
          | MIOrCmd (seqa1, seqa2), MIOrCmd (seqb1, seqb2) 
          | MISequenceCmd(seqa1, seqa2), MISequenceCmd(seqb1, seqb2) 
          | MIPipedCmd (seqa1, seqa2), MIPipedCmd (seqb1, seqb2) ->
              (match (cmd_seq_compare seqa1 seqb1, cmd_seq_compare seqa2 seqb2) with
                | (0, 0) -> 0 
                | (0, i) -> i
                | (i, _) -> i
              )
          | _, MIAndCmd _ -> 1
          | MIAndCmd _, _ -> -1 
          | _, MIOrCmd _ -> 1
          | MIOrCmd _, _ -> -1 
          | _, MISequenceCmd _ -> 1
          | MISequenceCmd _, _ -> -1 
      
      and option_compare oa ob = 
        match oa, ob with
          | None, None -> 0
          | Some a, Some b -> type_compare a b
          | Some _, None -> 1
          | _, Some _-> -1
      
      and set_compare sa sb = set_compare sa sb
      and map_compare sa sb = assert false

      and funarg_compare a b =
        match a, b with
          | MIBaseArg a, MIBaseArg b -> a -b 
          | MITupleArg a, MITupleArg b -> funargs_compare a b
          | _,_ -> assert false


      and funargs_compare alst blst = 
          let rec stop_at_diff alst blst =
            match alst, blst with
              | [],[] -> 0
              | ela::lsta, elb::lstb -> 
                  (match funarg_compare ela elb with
                    | 0 -> stop_at_diff lsta lstb
                    | i -> i )
              | _,_ -> assert false
          in 
          match (List.length alst) - (List.length blst) with
            | 0 -> stop_at_diff alst blst
            | i -> i 

      and fun_compare (aargs, abody) (bargs, bbody)  = 
        match funargs_compare aargs bargs with
          | 0 -> type_compare abody bbody (*THIS SHOULD BE THOUGH AND IMPROVED*)
          | i -> i 

      and composedType_compare sa sb = assert false

      and compare_basicfun (opa, arga1, arga2) (opb, argb1, argb2) = 
        let comp_args  = 
                (match type_compare arga1 argb1 with 
                | 0 -> type_compare arga2 argb2
                | i -> i
                )
        in
        match opa, opb with 
          | MAddition, MAddition 
          | MSoustraction, MSoustraction
          | MMultiplication, MMultiplication
          | MDivision, MDivision
          | MModulo, MModulo
          | MWith, MWith ->  comp_args 
          | MWithout, MWithout ->  comp_args 
          | MAddition, _ -> 1
          | _, MAddition -> -1
          | MSoustraction, _ -> 1
          | _, MSoustraction -> -1
          | MDivision, _ -> 1
          | _, MDivision -> -1
          | MModulo, _ -> 1
          | _, MModulo -> -1
          | MWith,_ -> 1
          | _,MWith -> -1
          | MWithout, _ -> 1 
          | _ , _ -> -1

      and compare_binding bda bdb = 
        match (List.length bda.mibd_name) - (List.length bdb.mibd_name) with
          | 0 ->
              list_compare
                (fun (ida, posa) (idb, posb) ->
                  match ida - idb with
                    | 0 -> 
                        (list_compare (fun posa posb -> posa -posb) posa posb)
                    | i -> i
                )
                bda.mibd_name bdb.mibd_name
          | i -> i 

      and compare_if (cond1,thn1, els1) (cond2, thn2, els2) =
        match type_compare cond1 cond2 with
          | 0 -> 
              (match type_compare thn1 thn2 with
                | 0 -> type_compare els1 els2 
                | i -> i 
              )
          | i -> i 

      and compare_comp (op1, left1, right1) (op2, left2, right2) = 
        match op1, op2 with 
          | Egal, Egal
          | NotEqual, NotEqual
          | LessThan, LessThan
          | LessOrEq, LessOrEq
          | GreaterThan, GreaterThan
          | GreaterOrEq, GreaterOrEq ->
              (match type_compare left1 left2 with
                | 0 -> type_compare right1 right2  
                | i -> i
              )
          | Egal, _ -> 1
          | _, Egal -> -1
          | NotEqual, _ -> 1
          | _ , NotEqual -> -1
          | LessThan, _ -> 1
          | _, LessThan -> -1
          | GreaterThan, _ -> 1
          | _ , GreaterThan -> -1
          | GreaterOrEq, _ -> 1
          | _, _ -> -1 

        and compare_body bdlst1 bdlst2 = 
          match bdlst1, bdlst2 with
            | [],[] -> 0
            | ela::lsta, elb::lstb -> 
                (match type_compare ela elb with
                | 0 -> compare_body lsta lstb 
                | i -> i )
            | _, _ -> assert false

      and simple_type_compare a b = 
        match a, b with
          | MIBase_val aaa, MIBase_val bbb -> 
          (*TODO definir pour chaque type de base une fonction de comparaison*)
              (match aaa, bbb with
                | MITypeStringVal aaaa, MITypeStringVal bbbb ->
                      String.compare aaaa bbbb
                | MITypeBoolVal true, MITypeBoolVal false ->
                      1
                | MITypeBoolVal false, MITypeBoolVal true ->
                      -1
                | MITypeBoolVal _, MITypeBoolVal _ ->
                      0
                | MITypeIntVal aaaa, MITypeIntVal bbbb ->
                      compare aaaa bbbb
                | MITypeFloatVal aaaa, MITypeFloatVal bbbb ->
                    if aaaa = bbbb then 0 
                    else 
                      if aaaa > bbbb then 1
                      else -1
                | MITypeCmdVal aaaa, MITypeCmdVal bbbb ->
                      cmd_seq_compare aaaa bbbb
          | _ , _ -> raise (TypeError "Bad type comparison")
              )
          | MITuple_val aaa, MITuple_val bbb -> 
              mlist_compare aaa bbb
          | MIList_val aaa, MIList_val bbb -> 
              mlist_compare aaa bbb
          | MIFun_val (_,aaaargs, aaabody), MIFun_val (_,bbbargs, bbbbody) -> 
              fun_compare (aaaargs, aaabody) (bbbargs, bbbbody)
          | MIEmpty_val, MIEmpty_val -> 0
          | MIEmpty_val, _ -> 1 
          | _, MIEmpty_val -> -1
          | _ , _ -> raise (TypeError "Bad type comparison")
      
      and type_compare a b =
          match a, b with 
          | MISimple_val  aa , MISimple_val bb ->
              simple_type_compare aa bb
          | MIComposed_val aa , MIComposed_val bb ->
              composedType_compare aa bb
          | MIRef_val (refa,argsa), MIRef_val (refb,argsb) ->
              mref_compare refa refb 
          | MIBasicFunBody_val (op, arg1, arg2), MIBasicFunBody_val (opp, arg1p, arg2p) ->
              compare_basicfun (op,arg1, arg2) (opp, arg1p, arg2p)
          | MIBind_val bd, MIBind_val bdp ->
              compare_binding bd bdp
          | MIIf_val (if1,thn1,els1), MIIf_val (if2, thn2, els2) ->
              compare_if (if1, thn1, els1) (if2, thn2, els2)
          | MIComp_val (op1, left1, right1), MIComp_val (op2, left2, right2) ->
              compare_comp (op1, left1, right1) (op2, left2, right2)
          | MIBody_val bd1, MIBody_val bd2 ->
              compare_body bd1 bd2
          | MISimple_val _, MIRef_val _
          | MISimple_val _, MIBind_val _ 
          | MISimple_val _, MIIf_val _ 
          | MISimple_val _, MIComp_val _ 
          | MISimple_val _, MIBody_val _ 
          | MISimple_val _, MIBasicFunBody_val _ ->
              1
          | MIComposed_val _, MIRef_val _
          | MIComposed_val _, MIBind_val _ 
          | MIComposed_val _, MIIf_val _ 
          | MIComposed_val _, MIBody_val _ 
          | MIComposed_val _, MIBasicFunBody_val _ ->
              1
          | MIRef_val _, MISimple_val _ 
          | MIBind_val _ , MISimple_val _
          | MIIf_val _ , MISimple_val _
          | MIComp_val _ , MISimple_val _
          | MIBody_val _ , MISimple_val _
          | MIBasicFunBody_val _ , MISimple_val _  ->
              -1
          | MIRef_val _, MIComposed_val _ 
          | MIBind_val _ , MIComposed_val _
          | MIIf_val _ , MIComposed_val _
          | MIBody_val _ , MIComposed_val _
          | MIBasicFunBody_val _ , MIComposed_val _  ->
              -1
          | MIRef_val _,  MIBasicFunBody_val _ 
          | MIRef_val _,  MIBind_val _ 
          | MIRef_val _,  MIComp_val _ 
          | MIRef_val _,  MIIf_val _ 
          | MIRef_val _,  MIBody_val _ -> 
              1
          | MIBasicFunBody_val _ , MIRef_val _ 
          | MIIf_val _ , MIRef_val _ 
          | MIComp_val _ , MIRef_val _ 
          | MIBody_val _ , MIRef_val _ 
          | MIBind_val _, MIRef_val _  -> 
              -1
          | MIBasicFunBody_val _ , MIBind_val _ 
          | MIBasicFunBody_val _ , MIIf_val _ 
          | MIBasicFunBody_val _ , MIBody_val _ 
          | MIBasicFunBody_val _ , MIComp_val _ ->
              1
          | MIBind_val _ , MIBasicFunBody_val _ 
          | MIComp_val _ , MIBasicFunBody_val _ 
          | MIBody_val _ , MIBasicFunBody_val _ 
          | MIIf_val _ , MIBasicFunBody_val _ ->
              -1
          | MIBind_val _, MIIf_val _ 
          | MIBind_val _, MIComp_val _ 
          | MIBind_val _, MIBody_val _ ->
              1
          | MIIf_val _, MIBind_val _  
          | MIComp_val _, MIBind_val _  
          | MIBody_val _, MIBind_val _  ->
              -1
          | MIComp_val _ , MIBody_val _ ->
              1
          | MIBody_val _ , MIComp_val _ ->
              -1

          | _ , _ -> raise (TypeError "Bad type comparison")
      
      let compare a b = type_compare a b
   end

  module MMap = Map.Make(SimpleCore)
  module MSet = Set.Make(SimpleCore)

  type movar = {
    mova_name: int list; (*we have a list when the input is a tuple, for
    exemple in the case "let a,b= 5" then mva_name is ("a","b"). *)
    mova_value: motype_val;
  }

  and moref_val = {
    morv_module : int option; (* None if curmodule *)
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
    mocm_res : int option; 
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
    | MOForkedCmd of mocmd_seq 
    | MOAndCmd of mocmd_seq * mocmd_seq 
    | MOOrCmd of mocmd_seq * mocmd_seq 
    | MOSequenceCmd of mocmd_seq * mocmd_seq 
    | MOPipedCmd of mocmd_seq * mocmd_seq 
 
  and mobase_type_val = 
    | MOTypeStringVal of string
    | MOTypeBoolVal of bool
    | MOTypeIntVal of int
    | MOTypeFloatVal of float
    | MOTypeCmdVal of mocmd_seq
 
  and mocomposed_type_val = {
    mocv_module_def : int option ; 
    mocv_fields: motype_val IntMap.t; 
    mocv_resolved_type : int option *int ; 
  }
  
 
  and mosimple_type_val = 
    | MOBase_val of mobase_type_val 
    | MOTuple_val of motype_val list
    | MOList_val of motype_val list
    | MONone_val 
    | MOSome_val of motype_val
    | MOSet_val of MSet.t
    | MOMap_val of motype_val MMap.t
    | MOFun_val of int StringMap.t * mofunarg list * motype_val 
    | MOEmpty_val
  
  and mofunarg = 
    | MOBaseArg of int
    | MOTupleArg of mofunarg list
  
  and motype_val = 
    | MOSimple_val of mosimple_type_val
    | MOComposed_val of mocomposed_type_val
    | MORef_val of moref_val * motype_val list 
    | MOBasicFunBody_val of mo_expr_operation * motype_val * motype_val
    | MOBind_val of mobinding
    | MOIf_val of motype_val * motype_val * motype_val
    | MOComp_val of mocomp_op * motype_val * motype_val 
    | MOBody_val of motype_val list

  and mocomp_op = GufoParsed.mcomp_op

  and mobinding = {
    mobd_name : (int * int list) list; 
    mobd_debugnames : string IntMap.t;
    mobd_value: motype_val;
    mobd_body: motype_val ;
  }

  and mo_expr_operation = GufoParsed.m_expr_operation
 
  (** A scope contains types and variables*)
  and moscope = {
    mosc_father : moscope option; 
    mosc_vars : movar list;
    mosc_type : motype list;  
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
    mosmv_action: (motype_val list -> topvar_val IntMap.t -> motype_val); 
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
    mosm_typstrfield2int: int StringMap.t; 
    mosm_typstrfield2inttype: int StringMap.t; 
    mosm_typfield2inttype: int IntMap.t; 
    mosm_topvar: mosysmodulemvar IntMap.t;
    mosm_varstr2int: int StringMap.t;
  }



  and  momodultype = 
    | MOUserMod of moprogram
    | MOSystemMod of mosysmodule


  and fullprogopt= {
    mofp_mainprog : moprogram ;
    mofp_progmodules : momodultype IntMap.t ;
    mofp_module_dep : IntSet.t IntMap.t; 
    mofp_progmap : int StringMap.t;
    mofp_progmap_debug : string IntMap.t; 
  }

  and moprogram = {
    mopg_name : int;
    mopg_types : mocomposed_type IntMap.t; 
    mopg_field_to_type: int IntMap.t; 
    mopg_topvar: topvar_val IntMap.t;
    mopg_topvar_bind_vars: IntSet.t StringMap.t IntMap.t; 
    mopg_topcal : motype_val ;
    mopg_topcal_bind_vars: IntSet.t StringMap.t; 
    mopg_topvar2int: int StringMap.t ;
    mopg_topvar_debugname: string IntMap.t ;
    mopg_ctype2int: int StringMap.t; 
    mopg_field2int: int StringMap.t;
  }

  and topvar_val = 
    | MOTop_val of motype_val 
    | MOTupEl_val of int * int list 

  and moprocess = GufoParsed.mprocess

 
  type t = mosimple_type_val


(** transformation from gufoParsed to gufo.core **)
  open SimpleCore

  (**Transformationf from SimpleCore to Core **)

  let rec simple_to_core_stringOrRef_val sor = 
    match sor with 
      | MISORString s -> MOSORString s
      | MISORExpr e -> MOSORExpr (simple_to_core_val e)

  and simple_to_core_cmd_output out =
    match out with
      | MICMDOStdOut -> MOCMDOStdOut
      | MICMDOStdErr -> MOCMDOStdErr
      | MICMDOFile sor -> 
          MOCMDOFile (simple_to_core_stringOrRef_val sor)
      | MICMDOFileAppend sor -> 
          MOCMDOFileAppend (simple_to_core_stringOrRef_val sor)

  and simple_to_core_cmd_outputerr oute = 
    match oute with
      | MICMDEStdOut -> MOCMDEStdOut 
      | MICMDEStdErr -> MOCMDEStdErr
      | MICMDEFile sor ->
          MOCMDEFile (simple_to_core_stringOrRef_val sor)
      | MICMDEFileAppend sor -> 
          MOCMDEFileAppend (simple_to_core_stringOrRef_val sor)

  and simple_to_core_cmd_input inp = 
    match inp with
      | MICMDIStdIn -> MOCMDIStdIn
      | MICMDIFile sor -> 
          MOCMDIFile (simple_to_core_stringOrRef_val sor) 


  and simple_to_core_cmd_val cmd = 
    {
      mocm_cmd = cmd.micm_cmd;
      mocm_args = List.map simple_to_core_stringOrRef_val cmd.micm_args;
      mocm_res = cmd.micm_res; 
      mocm_output = simple_to_core_cmd_output cmd.micm_output; 
      mocm_outputerr = simple_to_core_cmd_outputerr cmd.micm_outputerr; 
      mocm_input_src = simple_to_core_cmd_input cmd.micm_input_src; 
      mocm_input = cmd.micm_input; 
      mocm_print= cmd.micm_print;
      mocm_print_error= cmd.micm_print_error;
      mocm_print_std= cmd.micm_print_std;
    }

  and simple_to_core_cmdseq_val cmdseq = 
    match cmdseq with 
      | MISimpleCmd cmd ->
          MOSimpleCmd (simple_to_core_cmd_val cmd)
      | MIForkedCmd cmdseq -> 
          MOForkedCmd (simple_to_core_cmdseq_val cmdseq)
      | MIAndCmd (cmdseqa, cmdseqb) ->
          MOAndCmd(simple_to_core_cmdseq_val cmdseqa, simple_to_core_cmdseq_val cmdseqb)
      | MIOrCmd (cmdseqa, cmdseqb) -> 
          MOOrCmd(simple_to_core_cmdseq_val cmdseqa, simple_to_core_cmdseq_val cmdseqb)
      | MISequenceCmd (cmdseqa, cmdseqb) -> 
          MOSequenceCmd (simple_to_core_cmdseq_val cmdseqa, simple_to_core_cmdseq_val cmdseqb)
      | MIPipedCmd (cmdseqa, cmdseqb) ->
          MOPipedCmd(simple_to_core_cmdseq_val cmdseqa, simple_to_core_cmdseq_val cmdseqb)
 
  and simple_to_core_funarg funarg = 
    match funarg with 
      | MIBaseArg i -> MOBaseArg i
      | MITupleArg funarglst -> MOTupleArg (List.map simple_to_core_funarg funarglst) 

  and simple_to_core_composed_val cval = 
    {
      mocv_module_def = cval.micv_module_def;
      mocv_fields = IntMap.map simple_to_core_val cval.micv_fields;
      mocv_resolved_type = cval.micv_resolved_type;
    }

  and simple_to_core_simple_val simpleVal=
   match simpleVal with 
    | MIBase_val MITypeStringVal s ->
        MOBase_val (MOTypeStringVal s)
    | MIBase_val MITypeBoolVal b ->
        MOBase_val (MOTypeBoolVal b)
    | MIBase_val MITypeIntVal i ->
        MOBase_val (MOTypeIntVal i)
    | MIBase_val MITypeFloatVal f ->
        MOBase_val (MOTypeFloatVal f)
    | MIBase_val MITypeCmdVal cseq ->
        MOBase_val (MOTypeCmdVal (simple_to_core_cmdseq_val cseq))
    | MITuple_val tuplist -> 
        MOTuple_val (List.map (simple_to_core_val) tuplist)
    | MIList_val lst -> 
        MOList_val (List.map (simple_to_core_val) lst)
    | MINone_val ->
        MONone_val
    | MISome_val somev ->
        MOSome_val (simple_to_core_val somev)
    | MIFun_val (funnamelst, funargslst, body) ->
        MOFun_val (funnamelst,List.map simple_to_core_funarg funargslst,
        simple_to_core_val body)
    | MIEmpty_val -> MOEmpty_val

  and simple_to_core_ref_val rf = 
    {
      morv_module = rf.mirv_module;
      morv_varname = rf.mirv_varname;
      morv_index = 
        (match rf.mirv_index with
          | None -> None
          | Some lst -> Some (List.map simple_to_core_val lst));
      morv_debugname = rf.mirv_debugname;
    }

  and simple_to_core_binding_val bd = 
    {
        mobd_name = bd.mibd_name;
        mobd_debugnames = bd.mibd_debugnames;
        mobd_value= simple_to_core_val bd.mibd_value ;
        mobd_body=  simple_to_core_val bd.mibd_body;
    }

  and simple_to_core_val mt =
    match mt with
      | MISimple_val ms -> MOSimple_val (simple_to_core_simple_val ms)
      | MIComposed_val mc ->
        MOComposed_val (simple_to_core_composed_val mc)
      | MIRef_val (rf, argslst) ->
          MORef_val(simple_to_core_ref_val rf, List.map simple_to_core_val argslst)
      | MIBasicFunBody_val (expr, v1, v2) -> 
        MOBasicFunBody_val (expr, simple_to_core_val v1, simple_to_core_val v2)
      | MIBind_val bd ->
          MOBind_val (simple_to_core_binding_val bd)
      | MIIf_val (cond, thn, els) -> 
          MOIf_val (simple_to_core_val cond, simple_to_core_val thn, simple_to_core_val els)
      | MIComp_val (op, val1, val2) -> 
          MOComp_val(op,simple_to_core_val val1, simple_to_core_val val2)
      | MIBody_val bdlst -> 
          MOBody_val (List.map simple_to_core_val bdlst)

    let rec core_to_simple_composed ct = 
      {
        micv_module_def = ct.mocv_module_def;
        micv_fields= IntMap.map core_to_simple_mtype ct.mocv_fields;
        micv_resolved_type = ct.mocv_resolved_type;
      }

    and core_to_simple_funarg funarg = 
      match funarg with
       | MOBaseArg i -> MIBaseArg i 
       | MOTupleArg funarglst -> MITupleArg (List.map core_to_simple_funarg funarglst)

    and core_to_simple_stringOrRef sor = 
      match sor with
       | MOSORString s -> MISORString s
       | MOSORExpr e -> MISORExpr (core_to_simple_mtype e)

    and core_to_simple_cmd_output c = 
      match c with 
       | MOCMDOStdOut -> MICMDOStdOut
       | MOCMDOStdErr -> MICMDOStdErr 
       | MOCMDOFile f -> MICMDOFile (core_to_simple_stringOrRef f)
       | MOCMDOFileAppend f -> MICMDOFileAppend (core_to_simple_stringOrRef f)

    and core_to_simple_cmd_outputerr c = 
      match c with 
       | MOCMDEStdOut -> MICMDEStdOut
       | MOCMDEStdErr -> MICMDEStdErr
       | MOCMDEFile f -> MICMDEFile (core_to_simple_stringOrRef f)
       | MOCMDEFileAppend f -> MICMDEFileAppend  (core_to_simple_stringOrRef f)

    and core_to_simple_cmd_input c = 
      match c with 
       | MOCMDIStdIn -> MICMDIStdIn
       | MOCMDIFile f -> MICMDIFile (core_to_simple_stringOrRef f)

    and core_to_simple_cmd c =
      {
       micm_cmd = c.mocm_cmd ;
       micm_args = 
         List.map core_to_simple_stringOrRef c.mocm_args;
       micm_res = c.mocm_res;
       micm_output = core_to_simple_cmd_output c.mocm_output;
       micm_outputerr = core_to_simple_cmd_outputerr c.mocm_outputerr; 
       micm_input_src = core_to_simple_cmd_input c.mocm_input_src; 
       micm_input= c.mocm_input;
       micm_print= c.mocm_print;
       micm_print_error= c.mocm_print_error;
       micm_print_std= c.mocm_print_std;
      }

    and core_to_simple_cmdseq c =
     match c with 
       | MOSimpleCmd c -> 
           MISimpleCmd (core_to_simple_cmd c )
       | MOForkedCmd cseq ->
           MIForkedCmd (core_to_simple_cmdseq cseq)
       | MOAndCmd (cseqa, cseqb) ->
           MIAndCmd (core_to_simple_cmdseq cseqa, core_to_simple_cmdseq cseqb)
       | MOOrCmd (cseqa, cseqb)-> 
           MIOrCmd (core_to_simple_cmdseq cseqa, core_to_simple_cmdseq cseqb)
       | MOSequenceCmd (cseqa,cseqb) ->
           MISequenceCmd (core_to_simple_cmdseq cseqa, core_to_simple_cmdseq cseqb)
       | MOPipedCmd (cseqa, cseqb) ->
           MIPipedCmd(core_to_simple_cmdseq cseqa, core_to_simple_cmdseq cseqb)

 
    and core_to_simple_simple_val sv = 
      match sv with 
              | MOBase_val MOTypeStringVal s ->
                  MIBase_val (MITypeStringVal s)
              | MOBase_val MOTypeBoolVal b ->
                  MIBase_val (MITypeBoolVal b)
              | MOBase_val MOTypeIntVal i ->
                  MIBase_val (MITypeIntVal i)
              | MOBase_val MOTypeFloatVal f ->
                  MIBase_val (MITypeFloatVal f)
              | MOBase_val MOTypeCmdVal c ->
                  MIBase_val (MITypeCmdVal( core_to_simple_cmdseq c))
              | MOTuple_val lst
              | MOList_val lst ->
                  MIList_val (List.map core_to_simple_mtype lst )
              | MONone_val -> MINone_val 
              | MOSome_val v -> 
                  MISome_val (core_to_simple_mtype v )
              | MOFun_val (argnameslst, mofunarglst, body) -> 
                  MIFun_val (argnameslst,List.map core_to_simple_funarg mofunarglst , core_to_simple_mtype body)
              | MOEmpty_val -> MIEmpty_val
              | MOSet_val _ -> raise (TypeError "We cannot have set of set, nor use set as map key.")
              | MOMap_val _ ->  raise (TypeError "We cannot have set of map, nor use map as map key.")


    and core_to_simple_ref_val rf = 
      {
        mirv_module = rf.morv_module;
        mirv_varname = rf.morv_varname;
        mirv_index = 
          (match rf.morv_index with
            | None -> None
            | Some lst -> Some (List.map core_to_simple_mtype lst));
        mirv_debugname = rf.morv_debugname;
      }

    and core_to_simple_binding_val bd = 
      {
        mibd_name = bd.mobd_name;
        mibd_debugnames = bd.mobd_debugnames;
        mibd_value= core_to_simple_mtype bd.mobd_value ;
        mibd_body=  core_to_simple_mtype bd.mobd_body;
      }


    and core_to_simple_mtype v = 
      match v with 
        | MOSimple_val sv -> 
            MISimple_val (core_to_simple_simple_val sv )
        | MOComposed_val ct -> 
            MIComposed_val (core_to_simple_composed ct )

        | MORef_val (ref,args) -> 
            MIRef_val (
              core_to_simple_ref_val ref, 
              List.map core_to_simple_mtype args
            )
        | MOBasicFunBody_val (op, arga, argb)  ->
          MIBasicFunBody_val (op, core_to_simple_mtype arga, core_to_simple_mtype argb)
        | MOBind_val (bd) -> 
            MIBind_val (core_to_simple_binding_val bd)
        | MOIf_val (cond, thn, els) ->
            MIIf_val (core_to_simple_mtype cond, core_to_simple_mtype thn, core_to_simple_mtype els)
        | MOComp_val (op, left, right) -> 
            MIComp_val (op, core_to_simple_mtype left, core_to_simple_mtype right)
        | MOBody_val bdlst -> 
            MIBody_val (List.map core_to_simple_mtype bdlst)

  (**END Transformation from SimpleCore to Core **)

  (*Utility *)

  let empty_oprog = 
    {
          mopg_name= -1 ;
          mopg_types = IntMap.empty ;
          mopg_field_to_type= IntMap.empty ;
          mopg_topvar= IntMap.empty;
          mopg_topvar_bind_vars= IntMap.empty; 
          mopg_topcal = MOSimple_val MOEmpty_val;
          mopg_topcal_bind_vars= StringMap.empty; 
          mopg_topvar2int= StringMap.empty;
          mopg_topvar_debugname = IntMap.empty;
          mopg_ctype2int= StringMap.empty;
          mopg_field2int= StringMap.empty;
    }
  
  let empty_ofullprog = 
    {
      mofp_mainprog = empty_oprog;
      mofp_progmodules = IntMap.empty;
      mofp_progmap = StringMap.empty;
      mofp_progmap_debug = IntMap.empty;
      mofp_module_dep = IntMap.empty;
    }

  let is_empty_ofullprog ofullprog = 
    StringMap.is_empty ofullprog.mofp_progmap

  (* EXPR *)
  let empty_expr = MOSimple_val (MOEmpty_val)
  (* END EXPR *)

  (** PRINTER **)
  let rec type_to_string typ = 
    match typ with
      | MOUnique_type typ ->
          (match typ with
            | MOComposed_type mct -> mct.moct_debugname
            | MOBase_type bt -> GufoParsed.basetype_to_str bt
            | MOTuple_type lsttyp -> 
              sprintf "tuple(%s)" 
              (List.fold_left 
                (fun str typ -> sprintf "%s %s," str (type_to_string typ)) 
                "" 
                lsttyp
              )
            | MOList_type typ -> sprintf "list(%s)" (type_to_string typ)
            | MOOption_type typ -> sprintf "option(%s)" (type_to_string typ)
            | MOSet_type typ -> sprintf "set(%s)" (type_to_string typ)
            | MOMap_type (keytyp, valtyp) -> 
                sprintf "%s map(%s)" (type_to_string keytyp) (type_to_string valtyp)
            | MOFun_type(lstargs, rettyp) -> 
                sprintf "fun %s -> (%s)" 
                  (List.fold_left 
                    (fun str arg -> sprintf "%s %s," str (type_to_string arg)) 
                    "" 
                    lstargs) 
                  (type_to_string rettyp)
            | MOUnit_type  -> "unit"
            | MOAll_type i -> sprintf "'%d" i
            | MORef_type (modi, i , deep, args ) -> sprintf "ref %d[%d] %s" i deep 
                                                    (List.fold_left (fun s arg -> sprintf "%s -> %s" s (type_to_string arg)) "" args)
            | MOTupel_type _ -> sprintf "Tupel" 
          )
      | MOOr_type (ortyps,i) ->
          sprintf "( %s )'%d" 
            (List.fold_left 
              (fun acc ortyp -> sprintf "%s %s | " acc (type_to_string (MOUnique_type ortyp)) ) 
              "" ortyps)
            i



  let rec moval_to_string v = 
    match v with 
        | MOSimple_val sv ->
            (match sv with
              | MOBase_val MOTypeStringVal str -> str
              | MOBase_val MOTypeBoolVal true -> "True"
              | MOBase_val MOTypeBoolVal false -> "False"
              | MOBase_val MOTypeIntVal i -> sprintf "%d" i
              | MOBase_val MOTypeFloatVal f -> sprintf "%f" f
              | MOBase_val MOTypeCmdVal cmdseq -> 
                  (match cmdseq with
                    | MOSimpleCmd cmd -> 
                        (match cmd.mocm_print with
                          | None -> "-cmd-"
                          | Some res -> sprintf "%s\n" res
                        )
                    | _ -> sprintf "-cmd-" 
                  )
              | MOTuple_val tuplst ->
                  sprintf " ( %s) " 
                  (List.fold_left 
                    (fun str el -> sprintf "%s -- %s " str (moval_to_string el) ) 
                    (sprintf "%s" (moval_to_string (List.hd tuplst)))
                    (List.tl tuplst))
              | MOList_val [] ->
                  sprintf " [ ] " 
              | MOList_val lst ->
(*                   sprintf " [%s] " (List.fold_left (fun str el -> sprintf "%s %s, " str (moval_to_string el) ) "" lst) *)
                  sprintf " [ %s] " 
                  (List.fold_left 
                    (fun str el -> sprintf "%s , %s " str (moval_to_string el) ) 
                    (sprintf "%s" (moval_to_string (List.hd lst)))
                    (List.tl lst))
              | MONone_val -> "None"
              | MOSome_val sv -> sprintf "Some (%s)" (moval_to_string sv)
              | MOSet_val mset when MSet.is_empty mset -> 
                  sprintf " -< >- " 
              | MOSet_val mset -> 
                  let lst = MSet.elements mset in
                  sprintf " -< %s>- " 
                  (List.fold_left
                    (fun str el -> sprintf "%s , %s " str (moval_to_string (simple_to_core_val el)) ) 
                    (sprintf "%s" (moval_to_string (simple_to_core_val (List.hd lst))))
                    (List.tl lst))
              | MOMap_val amap when MMap.is_empty amap -> 
                  sprintf " -< : >- "
              | MOMap_val amap ->
                  let lst = MMap.bindings amap in
                  sprintf " -< %s>- " 
                  (List.fold_left
                    (fun str (key, el) -> 
                      sprintf "%s , %s : %s " str
                        (moval_to_string (simple_to_core_val key)) 
                        (moval_to_string el))
                    (let key, el = List.hd lst in
                      (sprintf "%s : %s" 
                        (moval_to_string (simple_to_core_val key)))
                        (moval_to_string el)
                    )
                    (List.tl lst))

              | MOFun_val (_,funargs, body) -> "-fun-"
              | MOEmpty_val -> "()"
            )
        | MOComposed_val mct -> "-composed-"
        | MORef_val (ref, args) -> "-ref-"
        | MOBasicFunBody_val (op, arga, argb) -> "-basicfun-"
        | MOBind_val bd -> "-binding-"
        | MOIf_val (cond, thn, els) -> "-if-"
        | MOComp_val (op, left, right) -> "-comp-"
        | MOBody_val lstbodies -> "-body-"


  (** END PRINTER **)

end
