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

open GufoParsed
open Printf
open Gufo
open Gufo.MCore
open GenUtils


let fold_over_obinding_val apply_fun acc expr = 
  let rec fold_over_obinding_val_ acc expr = 
    let rec fold_over_cmd_val_from_cmdseq acc seq = 
      let rec over_moStringOrRef acc sor = 
        match sor with
          | MOSORString _ -> acc
          | MOSORExpr expr -> fold_over_obinding_val_ acc expr
      in
      let rec over_cmd_output acc out =
        match out with 
          | MOCMDOStdOut
          | MOCMDOStdErr -> acc 
          | MOCMDOFile sor
          | MOCMDOFileAppend sor ->
              over_moStringOrRef acc sor
      in
      let rec over_cmd_outputerr acc out =
        match out with 
          | MOCMDEStdOut
          | MOCMDEStdErr -> acc 
          | MOCMDEFile sor
          | MOCMDEFileAppend sor ->
              over_moStringOrRef acc sor
      in
      let rec over_cmd_input acc inp =
        match inp with 
          | MOCMDIStdIn -> acc 
          | MOCMDIFile sor -> over_moStringOrRef acc sor 
      in
        match seq.loc_val with 
        | MOSimpleCmd cmd -> 
            let cmdv = cmd.loc_val in
            let acc = List.fold_left over_moStringOrRef acc cmdv.mocm_args in
            let acc = over_cmd_output acc cmdv.mocm_output in
            let acc = over_cmd_outputerr acc cmdv.mocm_outputerr in
            over_cmd_input acc cmdv.mocm_input_src 
        | MOForkedCmd seq -> fold_over_cmd_val_from_cmdseq acc seq
        | MOOrCmd (seq1, seq2)
        | MOSequenceCmd (seq1, seq2)
        | MOPipedCmd (seq1, seq2)
        | MOAndCmd (seq1, seq2) -> fold_over_cmd_val_from_cmdseq 
                                  (fold_over_cmd_val_from_cmdseq acc seq2) 
                                  seq1
    in
      match expr.loc_val with
      | MOComposed_val ct -> 
          IntMap.fold (fun _i expr acc -> fold_over_obinding_val_ acc expr) ct.mocv_fields acc
      | MOSimple_val msimple ->
        (match msimple with 
          | MONone_val -> acc
          | MOBase_val bv -> 
            (match bv with 
              | MOTypeCmdVal cseq -> fold_over_cmd_val_from_cmdseq acc cseq 
              | MOTypeStringVal _ | MOTypeBoolVal _ | MOTypeIntVal _ | MOTypeFloatVal _ -> acc
            )
          | MOSet_val set -> MSet.fold (fun el acc -> fold_over_obinding_val_ acc (simple_to_core_val el) ) set.loc_val acc
          | MOList_val mtypelst
          | MOTuple_val mtypelst ->
              List.fold_left fold_over_obinding_val_ acc mtypelst.loc_val
          | MOFun_val fv ->
              fold_over_obinding_val_ acc fv.loc_val.mofv_body
          | MOSome_val mtype -> fold_over_obinding_val_ acc mtype
          | MOMap_val keyValLst ->
              MMap.fold(fun key v acc  -> 
                fold_over_obinding_val_
                  (fold_over_obinding_val_ acc v) 
                  (simple_to_core_val key)) 
              (keyValLst.loc_val) acc 
          | MOEmpty_val -> acc
        )
      | MOBasicFunBody_val (_, e1, e2) ->
          fold_over_obinding_val_ (fold_over_obinding_val_ acc e1) e2
      | MORef_val (mref, mtyplst) -> 
          let acc = 
            match mref.morv_index with  
              | None -> acc
              | Some lst -> List.fold_left fold_over_obinding_val_ acc lst
          in
            List.fold_left fold_over_obinding_val_ acc mtyplst
      | MOBind_val mbind -> 
          let acc = apply_fun acc mbind in 
          let acc = fold_over_obinding_val_ acc mbind.mobd_value in
          fold_over_obinding_val_ acc mbind.mobd_body 
      | MOIf_val (cond, thn , els) ->
      fold_over_obinding_val_ 
        (fold_over_obinding_val_
          (fold_over_obinding_val_ acc cond) 
        thn) 
      els
      | MOComp_val (_, mtyp1, mtyp2) ->
      fold_over_obinding_val_
        (fold_over_obinding_val_ acc mtyp2)
      mtyp1
      | MOBody_val (lst) ->
          List.fold_left fold_over_obinding_val_ acc lst
      | MOEnvRef_val str -> acc
  in
    fold_over_obinding_val_ acc expr



let fold_over_obinding_and_ofun_val apply_bind_fun apply_fun_fun acc expr = 
  let rec fold_over_obinding_and_ofun_val_ acc expr = 
    let rec fold_over_cmd_val_from_cmdseq acc seq = 
      let rec over_moStringOrRef acc sor = 
        match sor with
          | MOSORString _ -> acc
          | MOSORExpr expr -> fold_over_obinding_and_ofun_val_ acc expr
      in
      let rec over_cmd_output acc out =
        match out with 
          | MOCMDOStdOut
          | MOCMDOStdErr -> acc 
          | MOCMDOFile sor
          | MOCMDOFileAppend sor ->
              over_moStringOrRef acc sor
      in
      let rec over_cmd_outputerr acc out =
        match out with 
          | MOCMDEStdOut
          | MOCMDEStdErr -> acc 
          | MOCMDEFile sor
          | MOCMDEFileAppend sor ->
              over_moStringOrRef acc sor
      in
      let rec over_cmd_input acc inp =
        match inp with 
          | MOCMDIStdIn -> acc 
          | MOCMDIFile sor -> over_moStringOrRef acc sor 
      in
        match seq.loc_val with 
        | MOSimpleCmd cmd -> 
            let cmdv = cmd.loc_val in
            let acc = List.fold_left over_moStringOrRef acc cmdv.mocm_args in
            let acc = over_cmd_output acc cmdv.mocm_output in
            let acc = over_cmd_outputerr acc cmdv.mocm_outputerr in
            over_cmd_input acc cmdv.mocm_input_src 
        | MOForkedCmd seq -> fold_over_cmd_val_from_cmdseq acc seq 
        | MOOrCmd (seq1, seq2)
        | MOSequenceCmd (seq1, seq2)
        | MOPipedCmd (seq1, seq2)
        | MOAndCmd (seq1, seq2) -> fold_over_cmd_val_from_cmdseq 
                                  (fold_over_cmd_val_from_cmdseq acc seq2 ) 
                                  seq1
    in
      match expr.loc_val with
      | MOComposed_val ct -> 
          IntMap.fold (fun _i expr acc -> fold_over_obinding_and_ofun_val_ acc expr) ct.mocv_fields acc
      | MOSimple_val msimple ->
        (match msimple with 
          | MONone_val -> acc
          | MOBase_val bv -> 
            (match bv with 
              | MOTypeCmdVal cseq -> fold_over_cmd_val_from_cmdseq acc cseq 
              | MOTypeStringVal _ | MOTypeBoolVal _ | MOTypeIntVal _ | MOTypeFloatVal _ -> acc
            )
          | MOSet_val set -> MSet.fold (fun el acc -> fold_over_obinding_and_ofun_val_ acc (simple_to_core_val el) ) set.loc_val acc
          | MOList_val mtypelst
          | MOTuple_val mtypelst ->
              List.fold_left fold_over_obinding_and_ofun_val_ acc mtypelst.loc_val
          | MOFun_val fv ->
              let fv = fv.loc_val in
              let acc = apply_fun_fun acc fv in
              fold_over_obinding_and_ofun_val_ acc fv.mofv_body
          | MOSome_val mtype -> fold_over_obinding_and_ofun_val_ acc mtype
          | MOMap_val keyValLst ->
              MMap.fold(fun key v acc  -> 
                fold_over_obinding_and_ofun_val_
                  (fold_over_obinding_and_ofun_val_ acc v) 
                  (simple_to_core_val key)) 
              (keyValLst.loc_val) acc 
          | MOEmpty_val -> acc
        )
      | MOBasicFunBody_val (_, e1, e2) ->
          fold_over_obinding_and_ofun_val_ (fold_over_obinding_and_ofun_val_ acc e1) e2
      | MORef_val (mref, mtyplst) -> 
          let acc = 
            match mref.morv_index with  
              | None -> acc
              | Some lst -> List.fold_left fold_over_obinding_and_ofun_val_ acc lst
          in
            List.fold_left fold_over_obinding_and_ofun_val_ acc mtyplst
      | MOEnvRef_val _ ->  acc
      | MOBind_val mbind -> 
          let acc = apply_bind_fun acc mbind in 
          let acc = fold_over_obinding_and_ofun_val_ acc mbind.mobd_value in
          fold_over_obinding_and_ofun_val_ acc mbind.mobd_body 
      | MOIf_val (cond, thn , els) ->
      fold_over_obinding_and_ofun_val_ 
        (fold_over_obinding_and_ofun_val_
          (fold_over_obinding_and_ofun_val_ acc cond) 
        thn) 
      els
      | MOComp_val (_, mtyp1, mtyp2) ->
      fold_over_obinding_and_ofun_val_
        (fold_over_obinding_and_ofun_val_ acc mtyp2)
      mtyp1
      | MOBody_val (lst) ->
          List.fold_left fold_over_obinding_and_ofun_val_ acc lst
  in
    fold_over_obinding_and_ofun_val_ acc expr



let fold_over_oref_val apply_fun acc expr = 
  let rec fold_over_oref_val_ acc expr = 
    let rec fold_over_cmd_val_from_cmdseq acc seq = 
      let rec over_moStringOrRef acc sor = 
        match sor with
          | MOSORString _ -> acc
          | MOSORExpr expr -> fold_over_oref_val_ acc expr
      in
      let rec over_cmd_output acc out =
        match out with 
          | MOCMDOStdOut
          | MOCMDOStdErr -> acc 
          | MOCMDOFile sor
          | MOCMDOFileAppend sor ->
              over_moStringOrRef acc sor
      in
      let rec over_cmd_outputerr acc out =
        match out with 
          | MOCMDEStdOut
          | MOCMDEStdErr -> acc 
          | MOCMDEFile sor
          | MOCMDEFileAppend sor ->
              over_moStringOrRef acc sor
      in
      let rec over_cmd_input acc inp =
        match inp with 
          | MOCMDIStdIn -> acc 
          | MOCMDIFile sor -> over_moStringOrRef acc sor 
      in
        match seq.loc_val with 
        | MOSimpleCmd cmd -> 
            let cmdv = cmd.loc_val in
            let acc = List.fold_left over_moStringOrRef acc cmdv.mocm_args in
            let acc = over_cmd_output acc cmdv.mocm_output in
            let acc = over_cmd_outputerr acc cmdv.mocm_outputerr in
            over_cmd_input acc cmdv.mocm_input_src 
        | MOForkedCmd seq -> fold_over_cmd_val_from_cmdseq acc seq 
        | MOOrCmd (seq1, seq2)
        | MOSequenceCmd (seq1, seq2)
        | MOPipedCmd (seq1, seq2)
        | MOAndCmd (seq1, seq2) -> fold_over_cmd_val_from_cmdseq 
                                  (fold_over_cmd_val_from_cmdseq acc seq2 ) 
                                  seq1
    in
      match expr.loc_val with
      | MOComposed_val ct -> 
          IntMap.fold (fun _i expr acc -> fold_over_oref_val_ acc expr) ct.mocv_fields acc
      | MOSimple_val msimple ->
        (match msimple with 
          | MONone_val -> acc
          | MOBase_val bv -> 
            (match bv with 
              | MOTypeCmdVal cseq -> fold_over_cmd_val_from_cmdseq acc cseq 
              | MOTypeStringVal _ | MOTypeBoolVal _ | MOTypeIntVal _ | MOTypeFloatVal _ -> acc
            )
          | MOSet_val set -> MSet.fold (fun el acc -> fold_over_oref_val_  acc (simple_to_core_val el) ) set.loc_val acc
          | MOList_val mtypelst
          | MOTuple_val mtypelst ->
              List.fold_left fold_over_oref_val_ acc mtypelst.loc_val
          | MOFun_val fv ->
              fold_over_oref_val_ acc fv.loc_val.mofv_body
          | MOSome_val mtype -> fold_over_oref_val_ acc mtype
          | MOMap_val keyValLst ->
              MMap.fold(fun key v acc  -> 
                fold_over_oref_val_
                  (fold_over_oref_val_ acc v) 
                  (simple_to_core_val key)) 
              (keyValLst.loc_val) acc 
          | MOEmpty_val -> acc
        )
      | MOBasicFunBody_val (_, e1, e2) ->
          fold_over_oref_val_ (fold_over_oref_val_ acc e1) e2
      | MORef_val (mref, mtyplst) -> 
          let acc = apply_fun acc mref in 
          let acc = 
            match mref.morv_index with  
              | None -> acc
              | Some lst -> List.fold_left fold_over_oref_val_ acc lst
          in
            List.fold_left fold_over_oref_val_ acc mtyplst
      | MOEnvRef_val _ -> acc
      | MOBind_val mbind -> 
          let acc = fold_over_oref_val_ acc mbind.mobd_value in
          fold_over_oref_val_ acc mbind.mobd_body 
      | MOIf_val (cond, thn , els) ->
      fold_over_oref_val_ 
        (fold_over_oref_val_
          (fold_over_oref_val_ acc cond) 
        thn) 
      els
      | MOComp_val (_, mtyp1, mtyp2) ->
      fold_over_oref_val_
        (fold_over_oref_val_ acc mtyp2)
      mtyp1
      | MOBody_val (lst) ->
          List.fold_left fold_over_oref_val_ acc lst
  in
    fold_over_oref_val_ acc expr

(*
let transform_ref_in_funcall transform_fun expr =
  let rec transform_ref_in_funcall_ transform_fun expr =
    let rec fold_over_cmd_val_from_cmdseq seq = 
      let rec over_moStringOrRef sor = 
        match sor with
          | MOSORString _ -> sor
          | MOSORExpr expr -> MOSORExpr (transform_ref_in_funcall_ expr)
      in
      let rec over_cmd_output out =
        match out with 
          | MOCMDOStdOut
          | MOCMDOStdErr -> out
          | MOCMDOFile sor -> MOCMDOFile (over_moStringOrRef sor)
          | MOCMDOFileAppend sor ->
              MOCMDOFileAppend (over_moStringOrRef sor)
      in
      let rec over_cmd_outputerr out =
        match out with 
          | MOCMDEStdOut
          | MOCMDEStdErr -> out
          | MOCMDEFile sor -> MOCMDEFile (over_moStringOrRef sor)
          | MOCMDEFileAppend sor ->
              MOCMDEFileAppend (over_moStringOrRef acc sor)
      in
      let rec over_cmd_input inp =
        match inp with 
          | MOCMDIStdIn -> inp
          | MOCMDIFile sor -> MOCMDIFile (over_moStringOrRef acc sor)
      in
        match seq with 
        | MOSimpleCmd cmd -> 
          MOSimpleCmd {cmd with 
                mocm_args = List.map over_moStringOrRef cmd.mocm_args;
                mocm_output = over_cmd_output cmd.mocm_output;
                mocm_outputerr = over_cmd_outputerr cmd.mocm_outputerr;
                mocm_input_src = over_cmd_input cmd.mocm_input_src;
              }
        | MOForkedCmd seq -> 
          MOForkedCmd (fold_over_cmd_val_from_cmdseq seq)
        | MOOrCmd (seq1, seq2) ->
          MOOrCmd (fold_over_cmd_val_from_cmdseq seq1, fold_over_cmd_val_from_cmdseq seq2)
        | MOSequenceCmd (seq1, seq2) ->
          MOSequenceCmd (fold_over_cmd_val_from_cmdseq seq1, fold_over_cmd_val_from_cmdseq seq2)
        | MOPipedCmd (seq1, seq2) -> 
          MOPipedCmd (fold_over_cmd_val_from_cmdseq seq1, fold_over_cmd_val_from_cmdseq seq2)
        | MOAndCmd (seq1, seq2) -> 
          MOAndCmd (fold_over_cmd_val_from_cmdseq seq1, fold_over_cmd_val_from_cmdseq seq2)
    in
      match expr with
      | MOComposed_val ct -> 
          MOComposed_val 
          {ct with mocv_fields = transform_ref_in_funcall_ ct.mocv_fields}
      | MOSimple_val msimple ->
        (match msimple with 
          | MONone_val -> MONone_val
          | MOBase_val bv -> 
            (match bv with 
              | MOTypeCmdVal cseq -> 
                  MOBase_val ( MOTypeCmdVal (fold_over_cmd_val_from_cmdseq cseq))
              | MOTypeStringVal _ | MOTypeBoolVal _ | MOTypeIntVal _ | MOTypeFloatVal _ -> 
                MOBase_val bv
            )
          | MOSet_val set -> 
              MOSet_val 
                (MSet.map 
                  (fun el -> core_to_simple_val (transform_ref_in_funcall_ 
                                        (simple_to_core_val el) )) set)
          | MOList_val mtypelst ->
              MOList_val (List.map transform_ref_in_funcall_ mtypelst)
          | MOTuple_val mtypelst ->
              MOTuple_val (List.map transform_ref_in_funcall_ mtypelst)
          | MOFun_val fv ->
              MOFun_val {fv with 
                mofv_body = transform_ref_in_funcall_ fv.mofv_body;
              }
          | MOSome_val mtype -> 
            MOSome_val (transform_ref_in_funcall_ mtyp)
          | MOMap_val keyValLst ->
              MOMap_val 
              (MMap.map 
                (fun el -> (core_to_simple_val (transform_ref_in_funcall_ (simple_to_core_val el)))) 
                keyValLst
              )
          | MOEmpty_val -> MOEmpty_val
        )
      | MOBasicFunBody_val (expr_op, e1, e2) ->
          MOBasicFunBody_val (expr_op, transform_ref_in_funcall_ e1, (transform_ref_in_funcall_ e2))
      | MORef_val (mref, mtyplst) -> 
         let new_mref = 
          { mref with 
            morv_index = (match mref.morv_index with  
                           | None -> None
                           | Some lst -> 
                             List.map transform_ref_in_funcall_ lst)
            ;
          }
          in
          let new_mtyplst = 
            List.map transform_ref_in_funcall_ mtyplst
          in 
          transform_fun new_mref new_mtyplst
      | MOEnvRef_val s -> MOEnvRef_val s
      | MOBind_val mbind -> 
          MOBody_val {mbind with
            mobd_value = transform_ref_in_funcall_ mbind.mobd_value;
            mobd_body = transform_ref_in_funcall_ mbind.mobd_body;
          }
      | MOIf_val (cond, thn , els) ->
          MOIf_val (transform_ref_in_funcall_ cond, 
                    transform_ref_in_funcall_ thn, 
                    transform_ref_in_funcall_ els)
      | MOComp_val (cmp, mtyp1, mtyp2) ->
          MOComp_val (cmp, transform_ref_in_funcall_ mtyp1, transform_ref_in_funcall_ mtyp2)
      | MOBody_val (lst) ->
          MOBody_val (List.map transform_ref_in_funcall_ lst)
  in
    fold_over_oref_val_ acc expr
*)








(*for a funarg type, return the type unfolded list of arguments. *)
let rec unstack_args arg = 
  match arg with 
    | MOBaseArg i -> [i.loc_val]  
    | MOTupleArg arglst ->
       List.fold_left 
        (fun alreadylst arg -> 
          let newlst = unstack_args arg in
          List.append newlst alreadylst
        ) [] arglst.loc_val

(*for a funarg type, return the type unfolded list of arguments. *)
let unstack_args_with_pos arg = 
  let rec unstack_args_with_pos_ arg pos  = 
    match arg with 
      | MOBaseArg i -> [i.loc_val, List.rev pos] , pos
      | MOTupleArg arglst ->
          let pos = 0::pos in
         List.fold_left 
          (fun (alreadylst,pos) arg -> 
            let top_pos, past_pos = List.hd pos, List.tl pos in
            let pos = top_pos+1 :: past_pos in
            let newlst,_ = unstack_args_with_pos_ arg pos in
            (List.append newlst alreadylst, pos)
          ) ([], pos) arglst.loc_val
  in 
  let res , _pos = unstack_args_with_pos_ arg [] in res

(*Simple utility fonction to get type of element in case of multi level list.*)
let rec get_type_at_deep pos typ deep =
  match deep with
    | 0 -> typ 
    | i -> 
        (
          match typ with 
            | MOList_type subtyp
            | MOSet_type subtyp
            | MOMap_type (_,subtyp) ->
                get_type_at_deep pos subtyp (deep - 1)
            |_ -> GufoParsedHelper.raise_typeError 
                    (sprintf "Trying to access sub-element of list (or set or map) for type %s. \n "(type_to_string typ)) 
                    pos
        )


  (*This function is terribly bad and uneficient. Should not use the debug name.
   *The fact is that for now we have no fix fd_modul id to cmd so we cannot get it.

   TODO: fix always the id of CMD (and other system module (maybe to fixed
   negative number) and change this function.
   * *)
let check_is_cmd_field fulloptiprog (fd_modul, fd_id) = 
  match fd_modul with
    | None -> false
    | Some modi -> 
        (String.compare (IntMap.find modi fulloptiprog.mofp_progmap_debug) "Cmd") = 0


let get_ownertype_from_field_intern fulloptiprog optiprog field = 
  let modul, id = field in
    match modul with 
      | None -> 
          let id = IntMap.find id optiprog.mopg_field_to_type in
          IntMap.find id optiprog.mopg_types
      | Some imod -> 
          match IntMap.find imod fulloptiprog.mofp_progmodules with 
          | MOUserMod uprog -> 
            let id = IntMap.find id uprog.mopg_field_to_type in
            IntMap.find id uprog.mopg_types
          | MOSystemMod sysprog -> 
                let id = IntMap.find id sysprog.mosm_typfield2inttype in
                  GufoModules.sysmodctype_to_ctype (IntMap.find id sysprog.mosm_types)


let find_type_in_prog fulloptiprog modi id =
  match modi with
    | None  
    | Some 0 -> IntMap.find id fulloptiprog.mofp_mainprog.mopg_types
    | Some i -> 
        (match IntMap.find i fulloptiprog.mofp_progmodules with
          | MOUserMod prog -> IntMap.find id prog.mopg_types
          | MOSystemMod sysmod -> 
              GufoModules.sysmodctype_to_ctype (IntMap.find id sysmod.mosm_types)
        )


(*field has type (int option * int) *)
(* return the composed type which own the field *)
let get_ownertype_from_field fulloptiprog optiprog field = 
  match check_is_cmd_field fulloptiprog field with
    | true -> MOBase_type (MTypeCmd)
    | false -> MOComposed_type (get_ownertype_from_field_intern fulloptiprog optiprog field )

let get_type_field_from_field fulloptiprog optiprog field = 
  let ctype = get_ownertype_from_field_intern fulloptiprog optiprog field in
  let _,id = field in 
  (IntMap.find id ctype.moct_fields)

let get_type_from_field fulloptiprog optiprog field = 
  let typefield = get_type_field_from_field fulloptiprog optiprog field in
  typefield.motf_type

let get_type_from_ref fulloptiprog optiprog typScope ref = 
  let modi = 
    match ref.morv_module with
      | None -> optiprog.mopg_name
      | Some i -> i
  in
  let id_var, fields = ref.morv_varname in 
  let base_type, pars_pos = IntMap.find id_var.loc_val (IntMap.find modi typScope) in
  (*we have to check the fields*)
  let typ_with_field = 
    match fields with
     | [] -> base_type
     | lst_f -> 
        (*we are only interested in the last field *)
        let last_f = List.hd (List.rev lst_f) in
        get_type_from_field fulloptiprog optiprog last_f
  in
  (*we have to check the indexes*)
  match ref.morv_index with
    | None -> {loc_val = typ_with_field; loc_pos = pars_pos}
    | Some lst -> 
        {loc_val = get_type_at_deep pars_pos typ_with_field (List.length lst);
         loc_pos = pars_pos;
        }




