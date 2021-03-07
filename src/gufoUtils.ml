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


(*at GufoParsed Level *)
  (*Tools to iterate over GufoParsed structure *)  
  let rec fold_over_cmd_val_from_cmdseq apply_fun acc seq = 
    match seq with 
      | SimpleCmd cmd -> apply_fun acc cmd.loc_val
      | ForkedCmd seq -> fold_over_cmd_val_from_cmdseq apply_fun acc seq.loc_val
      | OrCmd (seq1, seq2)
      | SequenceCmd (seq1, seq2)
      | PipedCmd (seq1, seq2)
      | AndCmd (seq1, seq2) -> fold_over_cmd_val_from_cmdseq apply_fun 
                                (fold_over_cmd_val_from_cmdseq apply_fun acc seq2.loc_val ) 
                                seq1.loc_val

  let rec fold_over_cmd_val apply_fun acc mtype = 
    match mtype with
    | MComposed_val _ -> acc
    | MSimple_val msimple ->
      (match msimple with 
        | MNone_val -> acc
        | MBase_val bv -> 
            (match bv with 
              | MTypeCmdVal cseq -> fold_over_cmd_val_from_cmdseq apply_fun acc cseq.loc_val 
              | MTypeStringVal _ | MTypeBoolVal _ | MTypeIntVal _ | MTypeFloatVal _ -> acc
            )
        | MEmpty_val -> acc
        | MList_val mtyplst
        | MSet_val mtyplst
        | MTuple_val mtyplst ->
            List.fold_left (fun acc v -> fold_over_cmd_val apply_fun acc v.loc_val) acc mtyplst.loc_val
        | MFun_val (_, mtype) 
        | MSome_val mtype -> fold_over_cmd_val apply_fun acc mtype.loc_val
        | MMap_val keyValLst ->
            List.fold_left (fun acc (key, v) -> 
              fold_over_cmd_val apply_fun 
                                (fold_over_cmd_val apply_fun acc v.loc_val) 
                                key.loc_val) 
            acc keyValLst.loc_val
      )
    | MBody_val (mtyplst) 
    | MBasicFunBody_val (_, mtyplst) -> 
        List.fold_left (fun acc el -> fold_over_cmd_val apply_fun acc el.loc_val) acc mtyplst
    | MRef_val (ref, mtyplst) -> 
              let acc = match ref.loc_val.mrv_index with 
                | None -> acc
                | Some lst -> List.fold_left (fun acc v -> fold_over_cmd_val apply_fun acc v.loc_val) acc lst
              in 
              List.fold_left (fun acc arg -> fold_over_cmd_val apply_fun acc arg.loc_val) 
                acc mtyplst
    | MEnvRef_val _ -> 
      acc
    | MBind_val mbind -> 
        fold_over_cmd_val 
          apply_fun (fold_over_cmd_val apply_fun acc mbind.mbd_value.loc_val) 
          mbind.mbd_body.loc_val
    | MIf_val (cond, thn , els) ->
    fold_over_cmd_val apply_fun 
      (fold_over_cmd_val apply_fun 
        (fold_over_cmd_val apply_fun acc cond.loc_val) 
      thn.loc_val) 
    els.loc_val
    | MComp_val (_, mtyp1, mtyp2) ->
    fold_over_cmd_val apply_fun 
      (fold_over_cmd_val apply_fun acc mtyp2.loc_val)
    mtyp1.loc_val

  let rec fold_over_composed_type_val apply_fun acc mtype = 
    match mtype with
    | MComposed_val ct -> apply_fun acc ct 
    | MSimple_val msimple ->
      (match msimple with 
        | MNone_val  -> acc
        | MBase_val (MTypeCmdVal cmdseq) ->
            let cmd_apply_fun acc cmd = 
              let fold_in_stringOrRef acc sor = 
                match sor with
                  | SORString _ -> acc
                  | SORExpr e -> fold_over_composed_type_val apply_fun acc e.loc_val
              in
              let acc = List.fold_left (fun acc arg -> fold_in_stringOrRef acc arg) acc cmd.mcm_args
              in
              let acc = 
                match cmd.mcm_output with
                  | MCMDOStdOut
                  | MCMDOStdErr -> acc
                  | MCMDOFile sor
                  | MCMDOFileAppend sor -> fold_in_stringOrRef acc sor
              in
              let acc = 
                match cmd.mcm_outputerr with
                  | MCMDEStdOut
                  | MCMDEStdErr -> acc
                  | MCMDEFile sor 
                  | MCMDEFileAppend sor -> fold_in_stringOrRef acc sor
              in
              let acc = 
                match cmd.mcm_input_src with
                 | MCMDIStdIn -> acc
                 | MCMDIFile sor -> fold_in_stringOrRef acc sor
              in acc
            in
            fold_over_cmd_val_from_cmdseq cmd_apply_fun acc cmdseq.loc_val
        | MBase_val _ -> acc
        | MEmpty_val -> acc
        | MList_val mtypelst
        | MSet_val mtypelst
        | MTuple_val mtypelst ->
            List.fold_left (fun acc v -> fold_over_composed_type_val apply_fun acc v.loc_val) acc mtypelst.loc_val
        | MFun_val (_, mtype) 
        | MSome_val mtype -> fold_over_composed_type_val apply_fun acc mtype.loc_val
        | MMap_val keyValLst ->
            List.fold_left (fun acc (key, v) -> 
              fold_over_composed_type_val apply_fun 
                                          (fold_over_composed_type_val apply_fun acc v.loc_val) 
                                          key.loc_val) 
            acc keyValLst.loc_val
      )
    | MBody_val (mtyplst) 
    | MBasicFunBody_val (_, mtyplst) ->
        List.fold_left (fun acc bd -> fold_over_composed_type_val apply_fun acc bd.loc_val) 
          acc mtyplst
    | MRef_val (ref, mtyplst) -> 
              let acc = match ref.loc_val.mrv_index with 
                | None -> acc
                | Some lst -> List.fold_left (fun acc v -> fold_over_composed_type_val apply_fun acc v.loc_val) acc lst
              in 
              List.fold_left (fun acc arg -> fold_over_composed_type_val apply_fun acc arg.loc_val) 
                acc mtyplst
    | MEnvRef_val _ -> 
        acc
    | MBind_val mbind -> 
        fold_over_composed_type_val 
          apply_fun (fold_over_composed_type_val apply_fun acc mbind.mbd_value.loc_val) 
          mbind.mbd_body.loc_val
    | MIf_val (cond, thn , els) ->
    fold_over_composed_type_val apply_fun 
      (fold_over_composed_type_val apply_fun 
        (fold_over_composed_type_val apply_fun acc cond.loc_val) 
      thn.loc_val) 
    els.loc_val
    | MComp_val (_, mtyp1, mtyp2) ->
    fold_over_composed_type_val apply_fun 
      (fold_over_composed_type_val apply_fun acc mtyp2.loc_val)
    mtyp1.loc_val

  let rec fold_over_mref_val apply_fun acc mtype = 
    match mtype with
    | MComposed_val _ -> acc
    | MSimple_val msimple ->
      (match msimple with 
        | MNone_val 
        | MEmpty_val -> acc
        | MBase_val (MTypeCmdVal cmdseq) ->
            let cmd_apply_fun acc cmd = 
              let fold_in_stringOrRef acc sor = 
                match sor with
                  | SORString _ -> acc
                  | SORExpr e -> fold_over_mref_val apply_fun acc e.loc_val
              in
              let acc = List.fold_left (fun acc arg -> fold_in_stringOrRef acc arg) acc cmd.mcm_args
              in
              let acc = 
                match cmd.mcm_output with
                  | MCMDOStdOut
                  | MCMDOStdErr -> acc
                  | MCMDOFile sor
                  | MCMDOFileAppend sor -> fold_in_stringOrRef acc sor
              in
              let acc = 
                match cmd.mcm_outputerr with
                  | MCMDEStdOut
                  | MCMDEStdErr -> acc
                  | MCMDEFile sor 
                  | MCMDEFileAppend sor -> fold_in_stringOrRef acc sor
              in
              let acc = 
                match cmd.mcm_input_src with
                 | MCMDIStdIn -> acc
                 | MCMDIFile sor -> fold_in_stringOrRef acc sor
              in acc
            in
            fold_over_cmd_val_from_cmdseq cmd_apply_fun acc cmdseq.loc_val
        | MBase_val _ -> acc
        | MList_val mtypelst
        | MSet_val mtypelst
        | MTuple_val mtypelst ->
            List.fold_left (fun acc v -> fold_over_mref_val apply_fun acc v.loc_val) acc mtypelst.loc_val
        | MFun_val (_, mtype) 
        | MSome_val mtype -> fold_over_mref_val apply_fun acc mtype.loc_val
        | MMap_val keyValLst ->
            List.fold_left 
              (fun acc (key, v) -> fold_over_mref_val apply_fun 
                                                      (fold_over_mref_val apply_fun acc v.loc_val) 
                                                      key.loc_val) 
              acc keyValLst.loc_val
      )
    | MBody_val (mtypelst) 
    | MBasicFunBody_val (_, mtypelst) ->
            List.fold_left (fun acc bd -> fold_over_mref_val apply_fun acc bd.loc_val) acc mtypelst
    | MRef_val (mref, mtyplst) -> 
              let acc = match mref.loc_val.mrv_index with 
                | None -> acc
                | Some lst -> List.fold_left (fun acc v -> fold_over_mref_val apply_fun acc v.loc_val) acc lst
              in 
              List.fold_left (fun acc arg -> fold_over_mref_val apply_fun acc arg.loc_val) 
                (apply_fun acc mref.loc_val) mtyplst
    | MEnvRef_val _ -> 
      acc
    | MBind_val mbind -> 
        fold_over_mref_val apply_fun (fold_over_mref_val apply_fun acc mbind.mbd_value.loc_val) 
        mbind.mbd_body.loc_val
    | MIf_val (cond, thn , els) ->
    fold_over_mref_val apply_fun 
      (fold_over_mref_val apply_fun 
        (fold_over_mref_val apply_fun acc cond.loc_val) 
      thn.loc_val) 
    els.loc_val
    | MComp_val (_, mtyp1, mtyp2) ->
    fold_over_mref_val apply_fun 
      (fold_over_mref_val apply_fun acc mtyp2.loc_val)
    mtyp1.loc_val

  let fold_over_mref_type apply_fun acc mtype = 
    let rec fold_over_mref_simple_type apply_fun acc ms = 
          match ms.loc_val with 
            | MBase_type _ -> acc
            | MTuple_type mtyplst -> 
                List.fold_left (fold_over_mref_simple_type apply_fun) acc mtyplst
            | MList_type mtyp
            | MOption_type mtyp
            | MSet_type mtyp ->
                fold_over_mref_simple_type apply_fun acc mtyp
            | MMap_type (mtypa, mtypb) ->
                fold_over_mref_simple_type apply_fun 
                  (fold_over_mref_simple_type apply_fun acc mtypb) mtypa
            | MFun_type (mtyplst, mtyp) ->
                List.fold_left (fold_over_mref_simple_type apply_fun) 
                  (fold_over_mref_simple_type apply_fun acc mtyp) mtyplst
            | MRef_type mr ->
                      apply_fun acc mr
            | MUnit
            | MAll_type _ -> acc
    in
    match mtype with
      | MComposed_type mt ->
          List.fold_left (fun acc mtf -> fold_over_mref_simple_type apply_fun acc mtf.mtf_type) acc mt.mct_fields
      | MSimple_type ms -> fold_over_mref_simple_type apply_fun acc ms 
          

  let rec fold_over_binding_val apply_fun acc mtype = 
    match mtype with
    | MComposed_val _ -> acc
    | MSimple_val msimple ->
      (match msimple with 
        | MNone_val 
        | MEmpty_val -> acc
        | MBase_val (MTypeCmdVal cmdseq) ->
            let cmd_apply_fun acc cmd = 
              let fold_in_stringOrRef acc sor = 
                match sor with
                  | SORString _ -> acc
                  | SORExpr e -> fold_over_binding_val apply_fun acc e.loc_val
              in
              let acc = List.fold_left (fun acc arg -> fold_in_stringOrRef acc arg) acc cmd.mcm_args
              in
              let acc = 
                match cmd.mcm_output with
                  | MCMDOStdOut
                  | MCMDOStdErr -> acc
                  | MCMDOFile sor
                  | MCMDOFileAppend sor -> fold_in_stringOrRef acc sor
              in
              let acc = 
                match cmd.mcm_outputerr with
                  | MCMDEStdOut
                  | MCMDEStdErr -> acc
                  | MCMDEFile sor 
                  | MCMDEFileAppend sor -> fold_in_stringOrRef acc sor
              in
              let acc = 
                match cmd.mcm_input_src with
                 | MCMDIStdIn -> acc
                 | MCMDIFile sor -> fold_in_stringOrRef acc sor
              in acc
            in
            fold_over_cmd_val_from_cmdseq cmd_apply_fun acc cmdseq.loc_val
        | MBase_val _ -> acc
        | MList_val mtypelst
        | MSet_val mtypelst
        | MTuple_val mtypelst ->
            List.fold_left (fun acc v -> fold_over_binding_val apply_fun acc v.loc_val) acc mtypelst.loc_val
        | MFun_val (_, mtype) 
        | MSome_val mtype -> fold_over_binding_val apply_fun acc mtype.loc_val
        | MMap_val keyValLst ->
            List.fold_left 
              (fun acc (key, v) -> fold_over_binding_val apply_fun 
                                                         (fold_over_binding_val apply_fun acc v.loc_val) key.loc_val) 
            acc keyValLst.loc_val
      )
    | MBody_val (mtyplst) 
    | MBasicFunBody_val (_, mtyplst) ->
        List.fold_left (fun acc bd -> fold_over_binding_val apply_fun acc bd.loc_val) acc mtyplst
    | MRef_val (ref , mtyplst) -> 
              let acc = match ref.loc_val.mrv_index with 
                | None -> acc
                | Some lst -> List.fold_left (fun acc v -> fold_over_binding_val apply_fun acc v.loc_val) acc lst
              in 
              List.fold_left (fun acc arg -> fold_over_binding_val apply_fun acc arg.loc_val)
                acc mtyplst
    | MEnvRef_val _ -> 
      acc
    | MBind_val mbind -> 
        fold_over_binding_val apply_fun 
          (fold_over_binding_val apply_fun (apply_fun acc mbind) mbind.mbd_value.loc_val) 
        mbind.mbd_body.loc_val
    | MIf_val (cond, thn , els) ->
    fold_over_binding_val apply_fun 
      (fold_over_binding_val apply_fun 
        (fold_over_binding_val apply_fun acc cond.loc_val) 
      thn.loc_val) 
    els.loc_val
    | MComp_val (_, mtyp1, mtyp2) ->
    fold_over_binding_val apply_fun 
      (fold_over_binding_val apply_fun acc mtyp2.loc_val)
    mtyp1.loc_val


let gufoKeywords = 
 List.fold_right StringSet.add 
  ["struct"; 
   "let";
   "fun"; 
   "mod";
   "with";
   "wout";
   "if";
   "then";
   "else";
   "int";
   "float";
   "string";
   "list";
   "set";
   "map";
   "option";
   "bool";
   "cmd";
   "extends";
   "in";
   "None";
   "Some";
   "$$START**";
  ] StringSet.empty

let is_keyword word = 
  StringSet.mem word gufoKeywords
