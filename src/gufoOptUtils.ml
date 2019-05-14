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
        match seq with 
        | MOSimpleCmd cmd -> 
            let acc = List.fold_left over_moStringOrRef acc cmd.mocm_args in
            let acc = over_cmd_output acc cmd.mocm_output in
            let acc = over_cmd_outputerr acc cmd.mocm_outputerr in
            over_cmd_input acc cmd.mocm_input_src 
        | MOForkedCmd seq -> fold_over_cmd_val_from_cmdseq acc seq 
        | MOOrCmd (seq1, seq2)
        | MOSequenceCmd (seq1, seq2)
        | MOPipedCmd (seq1, seq2)
        | MOAndCmd (seq1, seq2) -> fold_over_cmd_val_from_cmdseq 
                                  (fold_over_cmd_val_from_cmdseq acc seq2 ) 
                                  seq1
    in
      match expr with
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
          | MOSet_val set -> MSet.fold (fun el acc -> fold_over_obinding_val_  acc (simple_to_core_val el) ) set acc
          | MOList_val mtypelst
          | MOTuple_val mtypelst ->
              List.fold_left fold_over_obinding_val_ acc mtypelst
          | MOFun_val (_,_, mtype) 
          | MOSome_val mtype -> fold_over_obinding_val_ acc mtype
          | MOMap_val keyValLst ->
              MMap.fold(fun key v acc  -> 
                fold_over_obinding_val_
                  (fold_over_obinding_val_ acc v) 
                  (simple_to_core_val key)) 
              keyValLst acc 
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
      | MOEnvRef_val _ -> acc
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
        match seq with 
        | MOSimpleCmd cmd -> 
            let acc = List.fold_left over_moStringOrRef acc cmd.mocm_args in
            let acc = over_cmd_output acc cmd.mocm_output in
            let acc = over_cmd_outputerr acc cmd.mocm_outputerr in
            over_cmd_input acc cmd.mocm_input_src 
        | MOForkedCmd seq -> fold_over_cmd_val_from_cmdseq acc seq 
        | MOOrCmd (seq1, seq2)
        | MOSequenceCmd (seq1, seq2)
        | MOPipedCmd (seq1, seq2)
        | MOAndCmd (seq1, seq2) -> fold_over_cmd_val_from_cmdseq 
                                  (fold_over_cmd_val_from_cmdseq acc seq2 ) 
                                  seq1
    in
      match expr with
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
          | MOSet_val set -> MSet.fold (fun el acc -> fold_over_obinding_and_ofun_val_ acc (simple_to_core_val el) ) set acc
          | MOList_val mtypelst
          | MOTuple_val mtypelst ->
              List.fold_left fold_over_obinding_and_ofun_val_ acc mtypelst
          | MOFun_val (onames,argslst, mtype) ->
              let acc = apply_fun_fun acc (onames,argslst,mtype) in
              fold_over_obinding_and_ofun_val_ acc mtype
          | MOSome_val mtype -> fold_over_obinding_and_ofun_val_ acc mtype
          | MOMap_val keyValLst ->
              MMap.fold(fun key v acc  -> 
                fold_over_obinding_and_ofun_val_
                  (fold_over_obinding_and_ofun_val_ acc v) 
                  (simple_to_core_val key)) 
              keyValLst acc 
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











(*for a funarg type, return the type unfolded list of arguments. *)
let rec unstack_args arg = 
  match arg with 
    | MOBaseArg i -> [i]  
    | MOTupleArg arglst ->
       List.fold_left 
        (fun alreadylst arg -> 
          let newlst = unstack_args arg in
          List.append newlst alreadylst
        ) [] arglst

(*for a funarg type, return the type unfolded list of arguments. *)
let unstack_args_with_pos arg = 
  let rec unstack_args_with_pos_ arg pos  = 
    match arg with 
      | MOBaseArg i -> [i, List.rev pos] , pos
      | MOTupleArg arglst ->
          let pos = 0::pos in
         List.fold_left 
          (fun (alreadylst,pos) arg -> 
            let top_pos, past_pos = List.hd pos, List.tl pos in
            let pos = top_pos+1 :: past_pos in
            let newlst,_ = unstack_args_with_pos_ arg pos in
            (List.append newlst alreadylst, pos)
          ) ([], pos) arglst
  in 
  let res , _pos = unstack_args_with_pos_ arg [] in res

(*Simple utility fonction to get type of element in case of multi level list.*)
let rec get_type_at_deep typ deep =
  match deep with
    | 0 -> typ 
    | i -> 
        (
          match typ with 
            | MOList_type subtyp
            | MOSet_type subtyp
            | MOMap_type (_,subtyp) ->
                get_type_at_deep subtyp (deep - 1)
            |_ -> raise (TypeError (sprintf "Trying to access sub-element of list (or set or map) for type %s. \n "(type_to_string typ))) 
 
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
  let base_type = IntMap.find id_var (IntMap.find modi typScope) in
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
    | None -> typ_with_field
    | Some lst -> get_type_at_deep typ_with_field (List.length lst)




