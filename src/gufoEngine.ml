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

open Gufo
open GufoParsed
open Gufo.MCore
open GenUtils
open Unix
open Sys
open Array
open Printf


(*DUPLICATED FROM GufoOptUtils, we cannot use for cyclic compilation reaseon*)
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



let fullprog = ref None
let shenv_ = ref None
  
let set_fullprog fp = 
  fullprog:= Some fp

let get_fullprog () = 
  match !fullprog with
    | None -> assert false
    | Some fp -> fp

let set_shenv shenv = 
    shenv_ := Some shenv

let get_shenv () = 
  match !shenv_ with
    | None -> assert false
    | Some shenv -> shenv

(* GUFO SPECIFIC CMD *)
let rewrite_arg shenv arg = 

  (**)
  let check_star_pattern name pattern = 
    let pattern = (String.concat ".*" (String.split_on_char '*' pattern )) in
    let pattern = Str.regexp ("^" ^ pattern ^ "$") in
    Str.string_match pattern name 0
  in
    

  (*transform the '*' into the expected list of file. *)
  let apply_star_pattern str = 
(*     let full_path = GenUtils.get_abs_path (Sys.getcwd() ) str in *)
    let rec _apply_star_pattern pathSet =
      (*Return a stringset of path*)
      let get_pathSet_from_star path_before_star path_after_star = 
        let get_root_dir, file_part = 
          match String.rindex_opt path_before_star '/' with
            | None -> ".", path_before_star
            | Some pos -> split_in_two path_before_star pos
        in

        let after_file_part, after_path = 
          match String.index_opt path_after_star '/' with
            | None -> path_after_star, ""
            | Some pos -> split_in_two path_after_star pos
        in

        let files = Sys.readdir get_root_dir in
        let completions_files = 
        Array.fold_left
          (fun completions_files file -> 
(*
            match (GenUtils.compare_first_chars file_part file &&
                   GenUtils.compare_last_chars after_file_part file
                  )
*)
            match (check_star_pattern file (file_part ^"*" ^ after_file_part)) 
            with
              | true -> 
                  (match after_path with
                  | "" -> StringSet.add (get_root_dir ^ "/" ^ file) completions_files 
                  | _ -> StringSet.add (get_root_dir ^ "/" ^ file ^ "/" ^ after_path) completions_files 
                  )
              | false -> completions_files
          )
          StringSet.empty files
        in completions_files
      in
      StringSet.fold  
        (fun curpath new_pathSet ->
          match String.index_opt curpath '*' with
            | None -> StringSet.add curpath new_pathSet
            | Some pos_star -> 
              let before_star, after_star =
                GenUtils.split_in_two curpath pos_star 
              in
              let new_pathSet = 
                  StringSet.union (get_pathSet_from_star before_star after_star)
                                  new_pathSet
              in _apply_star_pattern new_pathSet
        )
        pathSet StringSet.empty
    in 
    let file_set = _apply_star_pattern (StringSet.singleton str) in
    (*instead of a set I want a list, alphabetically ordered *)
    let els = StringSet.elements file_set in
    match els with
      | [] -> [str]
      | _ -> els
  in
  let str_arg  = 
    match String.get arg 0 with
      | '~' -> 
          let user_dir = get_var shenv "HOME" in
          Str.replace_first (Str.regexp "~") user_dir arg
      | _ -> arg 
  in apply_star_pattern str_arg

(*what happen on the execution of the exit command. *)
let play_exit cmd red_args shenv input_fd output_fd outerr_fd = 
  (exit 0)


let play_cd cmd red_args shenv input_fd output_fd outerr_fd = 
  (*cd will write an error and do nothing else if it has more than 1 arg.*)
  let path, error = 
    match red_args with
      | [] -> "", None (*0 argument is valid but do quite nothing*)
      | [path] -> (*1 is the standard case it give the path*)
                  path, None
      | _ -> "", Some (1, "Cannot call cd with more than one argument.")
  in 
  match error, path with
    | Some (ecode, msg), _ -> 
        write_substring outerr_fd (msg ^ "\n") 0 ((String.length msg)+1);
      ecode,
      shenv, 
      {cmd with mocm_res = Some ecode; 
                mocm_print_error = Some msg}
    | None, path -> 
        try 
          Sys.chdir path;
          0,
          {shenv with 
            mose_curdir = Sys.getcwd ();
          },
          {cmd with mocm_res = Some 0;}
        with Sys_error msg -> 
        write_substring outerr_fd (msg ^ "\n") 0 ((String.length msg)+1);
          1,
          shenv ,
          {cmd with mocm_res = Some 1;
           mocm_print_error = Some msg}
          


(* END GUFO SPECIFIC CMD *)




(* PART TYPE COMPARE *)
  let rec mlist_compare la lb = 
      match (List.length la) - (List.length lb) with 
        | 0 ->
            list_compare val_compare la lb
        | i -> i 

  and mtuple_compare ta tb = 
    let rec el_compare lsta lstb = 
      match lsta, lstb with
        | a::nlsta, b::nlstb ->
            (match val_compare a b with
              | 0 -> el_compare nlsta nlstb
              | i -> i
            )
        | [], [] -> 0
        | _,_ -> assert false
    in
    match (List.length ta) - (List.length tb) with
      | 0 -> el_compare ta tb
      | i -> i 


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
      match String.compare cmda.mocm_cmd cmdb.mocm_cmd with
        | 0  -> 
            (match list_compare 
              (fun arga argb -> 
                match arga, argb with 
                  | MOSORString a, MOSORExpr b -> 1 
                  | MOSORExpr a, MOSORString b -> -1
                  | MOSORString a, MOSORString b -> 
                    String.compare a b
                  | MOSORExpr a, MOSORExpr b -> val_compare a b
              ) cmda.mocm_args cmdb.mocm_args 
            with
              | 0 ->
                  (match cmp_res cmda.mocm_res cmdb.mocm_res with
                    | 0 ->
                        (match cmp_print cmda.mocm_print_error cmdb.mocm_print_error with
                        | 0 ->
                          cmp_print cmda.mocm_print_std cmdb.mocm_print_std
                        | i -> i  
                        )
                    | i -> i 
                  )
              | i -> i
            )
        | i -> i
  
  and cmd_seq_compare cmdseqa cmdseqb =
    match cmdseqa, cmdseqb with
      | MOSimpleCmd cmda, MOSimpleCmd cmdb ->
          cmd_compare cmda cmdb
      | MOForkedCmd cmda, MOForkedCmd cmdb ->
          cmd_seq_compare cmda cmdb
      | _, MOSimpleCmd _ -> 1
      | MOSimpleCmd _, _ -> -1 
      | _, MOForkedCmd _ -> 1
      | MOForkedCmd _, _ -> -1 
      | MOAndCmd (seqa1, seqa2), MOAndCmd (seqb1, seqb2) 
      | MOOrCmd (seqa1, seqa2), MOOrCmd (seqb1, seqb2) 
      | MOSequenceCmd(seqa1, seqa2), MOSequenceCmd(seqb1, seqb2) 
      | MOPipedCmd (seqa1, seqa2), MOPipedCmd (seqb1, seqb2) ->
          (match (cmd_seq_compare seqa1 seqb1, cmd_seq_compare seqa2 seqb2) with
            | (0, 0) -> 0 
            | (0, i) -> i
            | (i, _) -> i
          )
      | _, MOAndCmd _ -> 1
      | MOAndCmd _, _ -> -1 
      | _, MOOrCmd _ -> 1
      | MOOrCmd _, _ -> -1 
      | _, MOSequenceCmd _ -> 1
      | MOSequenceCmd _, _ -> -1 
  
  and option_compare oa ob = 
    match oa, ob with
      | None, None -> 0
      | Some a, Some b -> val_compare a b
      | Some _, None -> 1
      | _, Some _-> -1
  
  and set_compare sa sb = set_compare sa sb
  and map_compare sa sb = assert false
  and fun_compare sa sb = assert false
  and composedType_compare sa sb = assert false

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
        | _,_ -> assert false
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
    match a.morv_module, b.morv_module with
      | None, Some _ -> 1 
      | Some _, None -> -1
      | None, None -> 0
      | Some i, Some ip -> 
          (match i - ip with
            | 0 -> comp_varname a.morv_varname b.morv_varname
            | i -> i
          )

  and simple_val_compare a b = 
    match a, b with
      | MOBase_val aaa, MOBase_val bbb -> 
      (*TODO definir pour chaque type de base une fonction de comparaison*)
          (match aaa, bbb with
            | MOTypeStringVal aaaa, MOTypeStringVal bbbb ->
                  String.compare aaaa bbbb
            | MOTypeBoolVal true, MOTypeBoolVal false ->
                  1
            | MOTypeBoolVal false, MOTypeBoolVal true ->
                  -1
            | MOTypeBoolVal _, MOTypeBoolVal _ ->
                  0
            | MOTypeIntVal aaaa, MOTypeIntVal bbbb ->
                  compare aaaa bbbb
            | MOTypeFloatVal aaaa, MOTypeFloatVal bbbb ->
                if aaaa = bbbb then 0 
                else 
                  if aaaa > bbbb then 1
                  else -1
            | MOTypeCmdVal aaaa, MOTypeCmdVal bbbb ->
                  cmd_seq_compare aaaa bbbb
            | _ , _ -> 
                debugPrint (Printf.sprintf "comparing type %s with %s \n" (moval_to_string (MOSimple_val a)) (moval_to_string (MOSimple_val b)));
                assert false
          )
      | MOTuple_val aaa, MOTuple_val bbb -> 
          mtuple_compare aaa bbb
      | MOList_val aaa, MOList_val bbb -> 
          mlist_compare aaa bbb
      | MONone_val, MONone_val -> 0
      | MONone_val, MOSome_val _ -> 1
      | MOSome_val _ , MONone_val -> -1
      | MOSome_val a, MOSome_val b -> val_compare a b
      | MOSet_val a, MOSet_val b -> MSet.compare a b 
      | MOMap_val a, MOMap_val b -> 
          MMap.compare val_compare a b
      
      | MOFun_val (_,aaaargs, aaabody), MOFun_val (_,bbbargs, bbbbody) -> 
          fun_compare (aaaargs, aaabody) (bbbargs, bbbbody)
      | MOEmpty_val, MOEmpty_val -> 0
      | MOEmpty_val, _ -> 1 
      | _, MOEmpty_val -> -1
      | _ , _ -> raise (TypeError "Bad type comparison")
  
  and val_compare a b =
      match a, b with 
      | MOSimple_val  aa , MOSimple_val bb ->
          simple_val_compare aa bb
      | MOComposed_val aa , MOComposed_val bb ->
          composedType_compare aa bb
      | MORef_val (refa,argsa), MORef_val (refb,argsb) ->
          mref_compare refa refb 
      | MOEnvRef_val (vara), MOEnvRef_val (varb) ->
          String.compare vara varb



      | _ , _ -> raise (TypeError "Bad type comparison")
  
  let compare a b = simple_val_compare a b


(* END PART TYPE COMPARE *)

(*  TEE *)
let stream_tee stream =
    let next self other i =
      try
        if Queue.is_empty self
        then
          let value = Stream.next stream in
          Queue.add value other;
          Some value
        else
          Some (Queue.take self)
      with Stream.Failure -> None in
    let q1 = Queue.create () in
    let q2 = Queue.create () in
    (Stream.from (next q1 q2), Stream.from (next q2 q1));;

(* END FROM TEE *)


let val_from_cmd_field field cmdseq =  
  let merge_opt_string opt1 opt2 = 
    match opt1, opt2 with
      | Some i, Some y -> Some (i^y)
      | Some i, _ -> Some i
      | _, Some i -> Some i
      | _, _ -> None
  in
  let rec get_cmd_res_from_cmdseq cmdseq = 
    match cmdseq with
    | MOSimpleCmd cmdv -> cmdv
    | MOForkedCmd cmdv -> get_cmd_res_from_cmdseq cmdv
    | MOAndCmd (cmda, cmdb) -> 
        let cmda = get_cmd_res_from_cmdseq cmda in
        (match cmda.mocm_res with
          | Some 0 ->
              let cmdb = get_cmd_res_from_cmdseq cmdb in
              {
                mocm_cmd = "";
                mocm_args = [];
                mocm_res = cmdb.mocm_res;
                mocm_output = cmdb.mocm_output; 
                mocm_outputerr = cmdb.mocm_outputerr; 
                mocm_input_src = cmda.mocm_input_src; 
                mocm_input = cmda.mocm_input; 
                mocm_print =  merge_opt_string cmda.mocm_print cmdb.mocm_print;
                mocm_print_error= merge_opt_string cmda.mocm_print_error cmdb.mocm_print_error;
                mocm_print_std=  merge_opt_string cmda.mocm_print_std cmdb.mocm_print_std;
              }
          | None -> raise (ExecutionError "command not executed (gufo internal error.)")
          | Some i -> cmda)
    | MOOrCmd (cmda, cmdb) -> 
        let cmda = get_cmd_res_from_cmdseq cmda in
        (match cmda.mocm_res with
          | Some i ->
              let cmdb = get_cmd_res_from_cmdseq cmdb in
              {
                mocm_cmd = "";
                mocm_args = [];
                mocm_res = cmdb.mocm_res;
                mocm_output = cmdb.mocm_output; 
                mocm_outputerr = cmdb.mocm_outputerr; 
                mocm_input_src = cmda.mocm_input_src; 
                mocm_input = cmda.mocm_input; 
                mocm_print =  merge_opt_string cmda.mocm_print cmdb.mocm_print;
                mocm_print_error= merge_opt_string cmda.mocm_print_error cmdb.mocm_print_error;
                mocm_print_std=  merge_opt_string cmda.mocm_print_std cmdb.mocm_print_std;
              }
          | None -> raise (ExecutionError "command not executed (gufo internal error.)")
          )
    | MOSequenceCmd (cmda, cmdb) ->
        let cmda, cmdb = get_cmd_res_from_cmdseq cmda,
                         get_cmd_res_from_cmdseq cmdb
        in
        {
           mocm_cmd = "";
           mocm_args = [];
           mocm_res = cmdb.mocm_res;
           mocm_output = cmdb.mocm_output; 
           mocm_outputerr = cmdb.mocm_outputerr; 
           mocm_input_src = cmda.mocm_input_src; 
           mocm_input = cmda.mocm_input; 
           mocm_print =  merge_opt_string cmda.mocm_print cmdb.mocm_print;
           mocm_print_error= merge_opt_string cmda.mocm_print_error cmdb.mocm_print_error;
           mocm_print_std=  merge_opt_string cmda.mocm_print_std cmdb.mocm_print_std;
        }
    | MOPipedCmd (cmda, cmdb) -> 
        let cmda, cmdb = get_cmd_res_from_cmdseq cmda,
                         get_cmd_res_from_cmdseq cmdb
        in
        {
           mocm_cmd = "";
           mocm_args = [];
           mocm_res = cmdb.mocm_res;
           mocm_output = cmdb.mocm_output; 
           mocm_outputerr = cmdb.mocm_outputerr; 
           mocm_input_src = cmda.mocm_input_src; 
           mocm_input = cmda.mocm_input; 
           mocm_print =  cmdb.mocm_print;
           mocm_print_error= merge_opt_string cmda.mocm_print_error cmdb.mocm_print_error;
           mocm_print_std=  cmdb.mocm_print_std;
        }
 
  in

  let cmd = get_cmd_res_from_cmdseq cmdseq in
  match field with 
    | 2 (*res*) -> 
        (match cmd.mocm_res with
          | None -> MOSimple_val (MOEmpty_val)
          | Some i -> MOSimple_val (MOSome_val (MOSimple_val (MOBase_val (MOTypeIntVal i))))
        )
    | 3 (*print*) -> 
        (match cmd.mocm_res, cmd.mocm_print_std with
          | None, _ -> MOSimple_val (MOBase_val (MOTypeStringVal ""))
          | Some _res, None -> MOSimple_val (MOBase_val (MOTypeStringVal ""))
          | Some res, Some str -> MOSimple_val (MOBase_val (MOTypeStringVal str))
        )
    | _ -> assert false

let rec find_val_from_field fields moval =
  match fields, moval with
    | ([_, field],MOComposed_val cv) -> IntMap.find field cv.mocv_fields
    | (( _, field)::fields,MOComposed_val cv) -> 
      find_val_from_field fields (IntMap.find field cv.mocv_fields)
    | ([_, field],MOSimple_val (MOBase_val (MOTypeCmdVal cmdseq))) ->  
         val_from_cmd_field field cmdseq 
    | _ -> assert false

let find_var_in_prog varname scope = 
  let rec find_with_pos pos tup_val = 
    match pos with 
      | [] ->  raise (ExecutionError "internal error")
      | [i] -> 
        (match tup_val with
          | MOSimple_val (MOTuple_val lst) -> List.nth lst i 
          | _ -> raise (ExecutionError "internal error"))
      | i::lst -> 
          (match tup_val with
            | MOSimple_val (MOTuple_val tuplst) -> find_with_pos lst (List.nth tuplst i)
            | _ -> raise (ExecutionError "internal error")
          )
   in 
  let find_in_topvar i = 
    match IntMap.find i scope with
      | MOTop_val tv -> tv 
      | MOTupEl_val (id_tupel, pos) -> 
          let tup_val = IntMap.find id_tupel scope in
          (match tup_val with
            | MOTop_val moval -> find_with_pos pos moval
            | _ -> raise (ExecutionError "internal error")
          )
  in
  match varname with 
    | i,[] -> 
        (try find_in_topvar i
        with 
          | Not_found -> assert false)
    | i,fields ->  (* $a.field.field *)
        let from_var = 
          ( try find_in_topvar i
            with 
            | Not_found -> assert false
          )
        in
        find_val_from_field fields from_var





let find_var_in_sysmod varname sysmod = 
  match varname with 
    | i, [] -> IntMap.find i sysmod.mosm_topvar
    | _ -> assert false



let check_is_true expr = 
  match expr with 
    | MOSimple_val (MOBase_val (MOTypeBoolVal true)) -> true
    | MOSimple_val (MOBase_val (MOTypeBoolVal false)) -> false 
    | _ -> assert false

  
let rec apply_opcomp toplevel arg2valMap op leftv rightv =
  let (red_leftv, red_rightv) = 
    (apply_motype_val toplevel arg2valMap leftv, apply_motype_val toplevel arg2valMap rightv)
  in
  match op with 
  | Egal -> 
      MOSimple_val (MOBase_val (MOTypeBoolVal ((val_compare red_leftv red_rightv) == 0)))
  | NotEqual ->
      MOSimple_val (MOBase_val (MOTypeBoolVal (not ((val_compare red_leftv red_rightv) == 0))))
  | LessThan ->
      (match red_leftv, red_rightv with
        | MOSimple_val (MOBase_val (MOTypeIntVal left)), 
          MOSimple_val (MOBase_val (MOTypeIntVal right)) -> 
            MOSimple_val (MOBase_val (MOTypeBoolVal (left < right)))
        | MOSimple_val (MOBase_val (MOTypeFloatVal left)), 
          MOSimple_val (MOBase_val (MOTypeFloatVal right)) -> 
            MOSimple_val (MOBase_val (MOTypeBoolVal(left < right)))
        | _ -> assert false
      )
  | LessOrEq ->
      (match red_leftv, red_rightv with
        | MOSimple_val (MOBase_val (MOTypeIntVal left)), 
          MOSimple_val (MOBase_val (MOTypeIntVal right)) -> 
            MOSimple_val (MOBase_val (MOTypeBoolVal (left <= right)))
        | MOSimple_val (MOBase_val (MOTypeFloatVal left)), 
          MOSimple_val (MOBase_val (MOTypeFloatVal right)) -> 
            MOSimple_val (MOBase_val (MOTypeBoolVal(left <= right)))
        | _ -> assert false
      )
  | GreaterThan ->
      (match red_leftv, red_rightv with
        | MOSimple_val (MOBase_val (MOTypeIntVal left)), 
          MOSimple_val (MOBase_val (MOTypeIntVal right)) -> 
            MOSimple_val (MOBase_val (MOTypeBoolVal (left > right)))
        | MOSimple_val (MOBase_val (MOTypeFloatVal left)), 
          MOSimple_val (MOBase_val (MOTypeFloatVal right)) -> 
            MOSimple_val (MOBase_val (MOTypeBoolVal(left > right)))
        | _ -> assert false
      )
  | GreaterOrEq ->
      (match red_leftv, red_rightv with
        | MOSimple_val (MOBase_val (MOTypeIntVal left)), 
          MOSimple_val (MOBase_val (MOTypeIntVal right)) -> 
            MOSimple_val (MOBase_val (MOTypeBoolVal (left >= right)))
        | MOSimple_val (MOBase_val (MOTypeFloatVal left)), 
          MOSimple_val (MOBase_val (MOTypeFloatVal right)) -> 
            MOSimple_val (MOBase_val (MOTypeBoolVal(left >= right)))
        | _ -> assert false
      )

and apply_in_body toplevel arg2valMap bodieslst = 
  match bodieslst with
    | [] -> assert false
    | [body] -> apply_motype_val toplevel arg2valMap body
    | body::lst -> let _ = apply_motype_val toplevel arg2valMap body in 
    apply_in_body toplevel arg2valMap lst

and apply_core_fun arg2valMap msysmodvar argslst = 
  msysmodvar.mosmv_action argslst arg2valMap

(*it can be a partial application*)
and apply_fun toplevel arg2valMap funval arglst = 
  (*faire pointer chaque arg sur sa valeur*)
  let rec create_pointer acc mofunarglst arglst = 
    List.fold_left
      (fun (acc,pos) argval -> 
        let mofunarg = List.nth mofunarglst pos in
        match mofunarg, argval with
          | MOBaseArg i, _ -> (IntMap.add i (MOTop_val argval) acc, pos + 1)
          | MOTupleArg mofunarglst, MOSimple_val (MOTuple_val tuplst) -> 
              create_pointer acc mofunarglst tuplst
          | MOTupleArg mofunarglst, _ -> assert false
      ) 
      (acc, 0) arglst
  in
  let rec apply_partial_fun full_fun partial_args = 
    match partial_args with
      | [] -> full_fun
      | cur_partial_arg ::partial_args -> 
        (match full_fun with
          | MOSimple_val(MOFun_val (dbg, curfunarg::mofunarglst, body)) ->
            let body_binding = 
              {
                mobd_name = unstack_args_with_pos curfunarg;
                mobd_debugnames = IntMap.empty ; (*at this level, we don't care about debug*)
                mobd_value= cur_partial_arg;
                mobd_body= body ;
              }
            in apply_partial_fun (MOSimple_val (MOFun_val (dbg, mofunarglst, MOBind_val body_binding))) partial_args
          | _ -> assert false
        )
  in 
  match funval with 
    | MOSimple_val(MOFun_val (dbg,mofunarglst, body)) -> 
      (*apply the code using arg2valMap when needed.*)
      (*we check if it is a partial application *)
      (match (Pervasives.compare (List.length arglst) (List.length mofunarglst)) with
        | i when i < 0 -> 
            apply_partial_fun funval arglst
        | 0 -> 
            let arg2valMap,_ = create_pointer arg2valMap mofunarglst arglst in
            apply_motype_val toplevel arg2valMap body
        | _ ->  (*more argument than take the function*)
              let pos = (List.length arglst) - (List.length mofunarglst) in
               let arglst, nargs = list_split_at_idx arglst pos in
               let narg2valMap,_ = create_pointer arg2valMap mofunarglst arglst in
               let funval = (apply_motype_val toplevel narg2valMap body) in 
               apply_fun toplevel arg2valMap funval nargs
      )
    | _ -> apply_motype_val toplevel arg2valMap funval

  (*This was the first version of play_cmd: this is no more used. For now we
   * keep it in case it is still usefull for comphrehension of this part.*)
and _play_cmd_simple toplevel to_fork (pip_write, pip_read) arg2valMap cmd = 
  let apply_mostringOrRef mosr = 
    match mosr with
      | MOSORString str -> str
      | MOSORExpr expr -> 
          match apply_motype_val toplevel arg2valMap expr with
            | MOSimple_val (MOBase_val (MOTypeStringVal s)) -> s (*result has to be a string*)
            | _ -> assert false
  in

  let red_args = List.map apply_mostringOrRef cmd.mocm_args in
  let input_fd = 
    match cmd.mocm_input_src, pip_read with
      | (MOCMDIStdIn, None) -> Unix.stdin
      | (MOCMDIStdIn, Some fd) -> fd
      | MOCMDIFile f,_ -> Unix.openfile (apply_mostringOrRef f) [Unix.O_RDONLY] 0o640 
  in
  let output_fd = 
    match cmd.mocm_output, pip_write with
      | MOCMDOStdOut, None -> Unix.stdout
      | MOCMDOStdOut, Some fd -> fd
      | MOCMDOStdErr,_ -> Unix.stderr
      | MOCMDOFile f,_ -> Unix.openfile (apply_mostringOrRef f) [Unix.O_WRONLY; Unix.O_CREAT; O_TRUNC] 0o640 
      | MOCMDOFileAppend f,_ -> Unix.openfile (apply_mostringOrRef f) [Unix.O_APPEND] 0o640 
  in
  let outerr_fd = 
    match cmd.mocm_outputerr with
      | MOCMDEStdErr -> Unix.stderr
      | MOCMDEStdOut -> Unix.stdout
      | MOCMDEFile f-> Unix.openfile (apply_mostringOrRef f) [Unix.O_WRONLY; Unix.O_CREAT; O_TRUNC] 0o640
      | MOCMDEFileAppend f-> Unix.openfile (apply_mostringOrRef f) [Unix.O_APPEND] 0o640
  in
  let pid = Unix.create_process cmd.mocm_cmd (Array.of_list (cmd.mocm_cmd::red_args)) input_fd output_fd outerr_fd 
  in 
  match to_fork with
    | true -> (0, MOSimpleCmd cmd)
    | false -> let (_, processstatus) =  Unix.waitpid [] pid in
               let res = match processstatus with
                         | Unix.WEXITED i -> i
                         | Unix.WSIGNALED i -> i
                         | Unix.WSTOPPED i -> i
               in 
               let _close_pipe = 
                 match pip_write with
                  | Some fd -> Unix.close fd
                  | _ -> ()
               in
               
               let cmd = 
                 { cmd with 
                  mocm_res = Some res ;
                  mocm_input = None; (*For V0, we don't keep any input/output to avoid over use of memory.*)
                  mocm_print = None;
                  mocm_print_error = None; 
                  mocm_print_std = None;
                 }
               in
               (res, MOSimpleCmd cmd)


and play_cmd toplevel to_fork (pip_write, pip_read) arg2valMap cmd = 
  let apply_mostringOrRef mosr = 
    match mosr with
      | MOSORString str -> str
      | MOSORExpr expr -> 
          match apply_motype_val toplevel arg2valMap expr with
            | MOSimple_val (MOBase_val (MOTypeStringVal s)) -> 
              (*result has to be a string*)
               s 
            | _ -> assert false
  in

  (*TODO: this is a slow and ugly way to check if the process is active, we
   * should find a new one.*)
  let is_process_active pid = 
    let dir = sprintf "/proc/%d" pid in
    let pid_stat =  sprintf "%s/stat" dir in
    match file_exists dir with
      | false -> false
      | true -> 
          let chan = open_in pid_stat in
          let splitted_stat = String.split_on_char ' ' (input_line chan) in
          close_in chan;
          match List.nth splitted_stat 2 with
            | "Z" -> false
            | _ ->  true
            
  in
  let red_args = List.map apply_mostringOrRef cmd.mocm_args in
  let shenv = get_shenv () in
  let red_args = 
    List.fold_left
      (fun red_args arg -> 
        List.append red_args (rewrite_arg shenv arg)
      )
      [] red_args 
  in
  let input_fd = 
    match cmd.mocm_input_src, pip_read with
      | (MOCMDIStdIn, None) -> Unix.stdin
      | (MOCMDIStdIn, Some fd) -> fd
      | MOCMDIFile f,_ -> Unix.openfile (apply_mostringOrRef f) [Unix.O_RDONLY] 0o640 
  in
  let output_fd = 
    match cmd.mocm_output, pip_write with
      | MOCMDOStdOut, None -> Unix.stdout
      | MOCMDOStdOut, Some fd -> fd
      | MOCMDOStdErr,_ -> Unix.stderr
      | MOCMDOFile f,_ -> Unix.openfile (apply_mostringOrRef f) [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o640 
      | MOCMDOFileAppend f,_ -> Unix.openfile (apply_mostringOrRef f) [Unix.O_APPEND] 0o640 
  in
  let outerr_fd = 
    match cmd.mocm_outputerr with
      | MOCMDEStdErr -> Unix.stderr
      | MOCMDEStdOut -> Unix.stdout
      | MOCMDEFile f-> Unix.openfile (apply_mostringOrRef f) [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o640
      | MOCMDEFileAppend f-> Unix.openfile (apply_mostringOrRef f) [Unix.O_APPEND] 0o640
  in

  let check_and_call_system_cmd cmd = 
      match cmd.mocm_cmd with
        | "cd" -> Some (play_cd cmd red_args shenv input_fd output_fd outerr_fd)
        | "exit" -> Some (play_exit cmd red_args shenv input_fd output_fd outerr_fd)
        | _ -> None
  in


  let play_cmd_toplevel () = 
    let pid = Unix.create_process cmd.mocm_cmd (Array.of_list (cmd.mocm_cmd::red_args)) input_fd output_fd outerr_fd in
    match to_fork with
      | true -> (0, MOSimpleCmd cmd)
      | false -> 
                let (_, processstatus) =  Unix.waitpid [] pid in
                let res = match processstatus with
                          | Unix.WEXITED i -> i
                          | Unix.WSIGNALED i -> i
                          | Unix.WSTOPPED i -> i
                in 

                 let _close_pipe = 
                   match pip_read with
                    | Some fd -> Unix.close fd
                    | _ -> ()
                 in

                 let _close_pipe = 
                   match pip_write with
                    | Some fd -> Unix.close fd
                    | _ -> ()
                 in
                 let cmd = 
                   { cmd with 
                    mocm_res = Some res ;
                    mocm_input = None; (*Field to remove*)
                    mocm_print = None; (*Field to remove*)
                    mocm_print_error = None; 
                    mocm_print_std = None;
                   }
                 in
                 (res, MOSimpleCmd cmd)
  in
  let play_cmd_nottoplevel () =
    let (child_stdout_descr, proc_stdout_descr) =
      Unix.pipe ~cloexec:true ()
    in
    set_nonblock proc_stdout_descr;
    let pid = Unix.create_process cmd.mocm_cmd (Array.of_list (cmd.mocm_cmd::red_args)) input_fd proc_stdout_descr outerr_fd
    in 
    match to_fork with
      | true -> (0, MOSimpleCmd cmd)
      | false -> 
                let stdout_channel_in = Unix.in_channel_of_descr child_stdout_descr in
                let output_fd = 
                  (match cmd.mocm_output, pip_write with
                    | MOCMDOStdOut, None -> Unix.openfile "/dev/null" [Unix.O_APPEND] 0o640
                    | _,_ -> output_fd )
                in
                let stdout_channel_out = Unix.out_channel_of_descr output_fd in
                let stdout_stream = Stream.of_channel stdout_channel_in in
                let stdout_stream, stdout_memory_stream = stream_tee stdout_stream in 

                 Unix.close proc_stdout_descr;
                while is_process_active pid do
                   try 
                     while true do 
                       flush_all ();
                       output_char stdout_channel_out (Stream.next stdout_stream) ;
                     done 
                   with _ -> () 
                done;
                  Stream.iter (fun achar -> output_char stdout_channel_out achar) stdout_stream; 
                let (_, processstatus) =  Unix.waitpid [] pid in
                let res = match processstatus with
                          | Unix.WEXITED i -> i
                          | Unix.WSIGNALED i -> i
                          | Unix.WSTOPPED i -> i
                in 

                let stdout_lst_char = 
                  Stream.npeek GufoConfig.max_char_print_buffer stdout_memory_stream in
                let stdout_lst_char = List.map (fun achar -> String.make 1 achar ) stdout_lst_char in
                let stdout_str = String.concat "" stdout_lst_char in
                (*TODO close stdout_channel*)
                 let _close_pipe = 
                   match pip_write with
                    | Some fd -> Unix.close fd
                    | _ -> ()
                 in
                 close_in stdout_channel_in;
                 
                 let _close_pipe = 
                   match pip_read with
                    | Some fd -> Unix.close fd
                    | _ -> ()
                 in

                 let cmd = 
                   { cmd with 
                    mocm_res = Some res ;
                    mocm_input = None; (*Field to remove*)
                    mocm_print = None; (*Field to remove*)
                    mocm_print_error = None; 
                    mocm_print_std = Some stdout_str;
                   }
                 in
                 (res, MOSimpleCmd cmd)
  in
  let is_system = check_and_call_system_cmd cmd in
  (* Create channel from process *)
  match is_system, toplevel with 
    | None, true -> play_cmd_toplevel ()
    | None, false -> play_cmd_nottoplevel ()
    | Some (res, shenv, cmd), _  -> 
        let _ = set_shenv shenv in
        (res, MOSimpleCmd cmd)


  (*TODO:
    *
    * The current implementation is bad on pipe:
    *   in ls -l | wc -l, wc -l will only start after the termination of ls -l
    *   which means an important performance loss.  
    * *)
and apply_cmdseq_val toplevel to_fork (pip_write,pip_read) arg2valMap cmdseq = 
  let apply_cmd to_fork (pip_write, pip_read) cmd = 
    (*we never play several times the same command*)
    match cmd.mocm_res with 
      | None -> play_cmd toplevel to_fork (pip_write, pip_read) arg2valMap cmd 
      | Some i -> i, MOSimpleCmd cmd
  in
  match cmdseq with 
    | MOSimpleCmd cmd -> apply_cmd to_fork (pip_write, pip_read) cmd
    | MOForkedCmd mocmd_seq -> 
        let (res, seq) = apply_cmdseq_val toplevel true (pip_write, pip_read) arg2valMap mocmd_seq  in
        (res, MOForkedCmd seq)
    | MOAndCmd (cmda, cmdb) ->
       (match apply_cmdseq_val toplevel to_fork (pip_write, pip_read) arg2valMap cmda with
          | (0, cmdseqa) -> 
              let (res, cmdseqb) = apply_cmdseq_val toplevel to_fork (pip_write, pip_read) arg2valMap cmdb in 
                (res, MOAndCmd (cmdseqa, cmdseqb))
          | (i, cmdseqa) -> 
              (i, MOAndCmd (cmdseqa, cmdb))
       )
    | MOOrCmd (cmda, cmdb) -> 
       (match apply_cmdseq_val toplevel to_fork (pip_write, pip_read) arg2valMap cmda with
          | (0, cmdseqa) -> 
                (0, MOOrCmd (cmdseqa, cmdb))
          | (i, cmdseqa) -> 
              let (res, cmdseqb) =  apply_cmdseq_val toplevel to_fork (pip_write, pip_read) arg2valMap cmdb in 
              (res, MOOrCmd (cmdseqa, cmdseqb))
       )
    | MOSequenceCmd (cmda, cmdb) ->
        let (_, cmdseqa) = apply_cmdseq_val toplevel to_fork (pip_write, pip_read) arg2valMap cmda in
        let (res, cmdseqb) = apply_cmdseq_val toplevel to_fork (pip_write, pip_read) arg2valMap cmdb in
        (res, MOSequenceCmd(cmdseqa, cmdseqb))
    | MOPipedCmd (cmda, cmdb) ->
        let (newpip_read, newpip_write) = Unix.pipe ~cloexec:true () in 
        let (_,cmda_run) = 
          apply_cmdseq_val toplevel to_fork (Some newpip_write, pip_read) arg2valMap cmda
        in
        let (res, cmdb_run) = apply_cmdseq_val toplevel to_fork (pip_write, Some newpip_read) arg2valMap cmdb
        in (res, MOPipedCmd (cmda_run, cmdb_run))

and apply_basic_fun toplevel arg2valMap op arga argb =
  let reduced_arga = apply_motype_val toplevel arg2valMap arga in
  let reduced_argb = apply_motype_val toplevel arg2valMap argb in
  
  (*TODO: improve perf *)
  match op, reduced_arga, reduced_argb with 
  | MAddition, MOSimple_val (MOBase_val (MOTypeStringVal a)),
    MOSimple_val (MOBase_val (MOTypeStringVal b)) -> 
      MOSimple_val (MOBase_val (MOTypeStringVal 
      (Printf.sprintf "%s%s" a b)))
  | MAddition, MOSimple_val (MOBase_val (MOTypeIntVal a)),
    MOSimple_val (MOBase_val (MOTypeIntVal b)) -> 
      MOSimple_val (MOBase_val (MOTypeIntVal  (a + b)))

  | MAddition, MOSimple_val (MOBase_val (MOTypeFloatVal a)),
    MOSimple_val (MOBase_val (MOTypeFloatVal b)) -> 
      MOSimple_val (MOBase_val (MOTypeFloatVal (a +. b)))
  | MAddition, _, _ -> assert false
  | MSoustraction, MOSimple_val (MOBase_val (MOTypeIntVal a)),
    MOSimple_val (MOBase_val (MOTypeIntVal b)) -> 
      MOSimple_val (MOBase_val (MOTypeIntVal (
      a - b)))
  | MSoustraction, MOSimple_val (MOBase_val (MOTypeFloatVal a)),
    MOSimple_val (MOBase_val (MOTypeFloatVal b))  -> 
      MOSimple_val (MOBase_val (MOTypeFloatVal (a -. b)))
  | MSoustraction, _,_ -> assert false
  | MMultiplication, MOSimple_val (MOBase_val (MOTypeIntVal a)),
    MOSimple_val (MOBase_val (MOTypeIntVal b)) -> 
      MOSimple_val(MOBase_val (MOTypeIntVal 
      (a * b) ))
  | MMultiplication, MOSimple_val (MOBase_val (MOTypeFloatVal a)), 
    MOSimple_val (MOBase_val (MOTypeFloatVal b)) -> 
      MOSimple_val (MOBase_val (MOTypeFloatVal (a *. b)))
  | MMultiplication, _, _ -> assert false
  | MDivision, MOSimple_val (MOBase_val (MOTypeIntVal a)),
    MOSimple_val (MOBase_val (MOTypeIntVal b)) ->
      MOSimple_val (MOBase_val (MOTypeIntVal (a / b)))
  | MDivision, MOSimple_val (MOBase_val (MOTypeFloatVal a)), 
    MOSimple_val (MOBase_val (MOTypeFloatVal b)) -> 
      MOSimple_val (MOBase_val (MOTypeFloatVal(a /. b)))
  | MModulo, MOSimple_val (MOBase_val (MOTypeIntVal el1)),
                        MOSimple_val (MOBase_val (MOTypeIntVal el2)) -> 
      MOSimple_val(MOBase_val (MOTypeIntVal (el1/el2)))
  | MModulo, MOSimple_val (MOBase_val (MOTypeFloatVal el1)),
                          MOSimple_val (MOBase_val (MOTypeFloatVal el2)) -> 
      MOSimple_val (MOBase_val (MOTypeFloatVal (mod_float el1 el2)))
  | MWith, MOSimple_val(MOList_val lst1), MOSimple_val (MOList_val lst2) ->
      MOSimple_val(MOList_val (List.concat [lst1; lst2]))
  | MWith, MOSimple_val(MOSet_val set1), MOSimple_val (MOSet_val set2) ->
      MOSimple_val(MOSet_val (MSet.union set1 set2))
  | MWith, MOSimple_val(MOMap_val map1), MOSimple_val (MOMap_val map2) ->
      MOSimple_val(MOMap_val (MMap.merge (fun k v1 v2 -> 
                                            match v1, v2 with
                                              | None, Some v -> Some v
                                              | Some v, None -> Some v
                                              | Some v1, Some v2 -> Some v2
                                              | None, None -> None
                                          ) map1 map2))
  | MHas, MOSimple_val(MOSet_val set1), possibleEl ->
    MOSimple_val( MOBase_val (MOTypeBoolVal (MSet.mem (core_to_simple_mtype possibleEl) set1)))
  | MHas, MOSimple_val(MOMap_val map1), possibleEl ->
    MOSimple_val( MOBase_val (MOTypeBoolVal (MMap.mem (core_to_simple_mtype possibleEl) map1)))
  | MWithout, MOSimple_val(MOSet_val set1), MOSimple_val (MOSet_val set2) ->
      MOSimple_val(MOSet_val (MSet.diff set1 set2))
  | MWithout, MOSimple_val(MOMap_val map1), MOSimple_val (MOMap_val map2) ->
      MOSimple_val(MOMap_val (MMap.merge (fun k v1 v2 -> 
                                            match v1, v2 with
                                              | None, Some v -> None
                                              | Some v, None -> Some v
                                              | Some v1, Some v2 -> None
                                              | None, None -> None
                                          ) map1 map2))
  | _ -> assert false




and apply_binding toplevel arg2valMap mbind = 
  (*we apply every tuple element to its value, and then go in the body.*)
  let rec get_value_from_position position res_value = 
    match position with
      | [] -> MOTop_val (res_value) (*not a tuple, we immediatly return the value.*)
      | [i] -> (*one dimension tuple *)
          (match res_value with
            | MOSimple_val (MOTuple_val tupv) -> MOTop_val (List.nth tupv i)
            | _ -> raise (ExecutionError "This is an internal error from gufo. Please complain.")
          )
      | i::lst ->
          (match res_value with 
            | MOSimple_val (MOTuple_val tupv) -> get_value_from_position lst (List.nth tupv i)
            | _ -> raise (ExecutionError "This is an internal error from gufo. Please complain.")
          )
  in
  let res_value = (apply_motype_val false arg2valMap mbind.mobd_value ) in
  (*associate the resulting value tuple to the binding tuple*)
  let arg2valMap = 
  List.fold_left
    (fun arg2valMap (id_bd, position) -> 
      IntMap.add id_bd (get_value_from_position position res_value) arg2valMap
    )
    arg2valMap mbind.mobd_name 
  in 
  apply_motype_val toplevel arg2valMap mbind.mobd_body 

and apply_composed_type toplevel arg2valMap mct = 
  MOComposed_val 
  {
    mct with  
    mocv_fields = IntMap.map (apply_motype_val toplevel arg2valMap) mct.mocv_fields;
  }

and apply_mosimpletype_val toplevel arg2valMap aval = 
  match aval with 
    | MOBase_val (MOTypeCmdVal cmdseq) -> 
        let (_, cmd) = 
          (apply_cmdseq_val toplevel false (None,None) arg2valMap cmdseq)
        in 
        MOSimple_val (MOBase_val (MOTypeCmdVal cmd))
    | MOBase_val bv -> MOSimple_val (MOBase_val bv)
    | MOTuple_val tuplst ->
        MOSimple_val (MOTuple_val (List.map (apply_motype_val toplevel arg2valMap) tuplst))
    | MOList_val lst ->
        MOSimple_val (MOList_val (List.map (apply_motype_val toplevel arg2valMap) lst))
    | MOSet_val set ->
        let nset = MSet.fold (fun aval nset -> 
          MSet.add (core_to_simple_mtype (apply_motype_val toplevel arg2valMap 
                    (simple_to_core_val aval))) nset) set MSet.empty
        in
        MOSimple_val (MOSet_val nset)
    | MOMap_val mmap ->
        MOSimple_val (MOMap_val (
          MMap.fold 
          (fun key value newmap ->
            MMap.add 
              (core_to_simple_mtype
                (apply_motype_val toplevel arg2valMap (simple_to_core_val key))) 
            (apply_motype_val toplevel arg2valMap value) newmap
          ) mmap MMap.empty))
    | MONone_val  -> MOSimple_val (MONone_val)
    | MOSome_val sv -> MOSimple_val (MOSome_val (apply_motype_val toplevel arg2valMap sv))

    | MOFun_val (onames, arglst, body) ->
        MOSimple_val (MOFun_val (onames, arglst, body))
    | MOEmpty_val -> MOSimple_val (MOEmpty_val)

and apply_in_list toplevel arg2valMap ref indices =
  let rec apply_in_list_ cur_lst indices =
    try 
      match cur_lst with
        | MOSimple_val (MOList_val lst_val) -> 
            (match indices with 
              | [] -> assert false
              | [idx] -> 
                  (match idx with 
                    | MOSimple_val (MOBase_val (MOTypeIntVal i))-> List.nth lst_val i
                    | _ -> assert false
                  )
              | idx::others_idx -> 
                  (match idx with 
                    | MOSimple_val (MOBase_val (MOTypeIntVal i)) -> 
                      apply_in_list_ (List.nth lst_val i) others_idx
                    | _ -> assert false
                  )
            )
        | _ -> assert false
    with Failure _ -> raise (RuntimeError "Invalid index.\n")
    in 
  apply_in_list_ (find_var_in_prog ref.morv_varname arg2valMap) indices

and apply_motype_val toplevel arg2valMap aval = 
  match aval with 
    | MOSimple_val sv ->
        apply_mosimpletype_val toplevel arg2valMap sv
    | MOComposed_val mct -> apply_composed_type toplevel arg2valMap mct 
    | MORef_val (ref, argslst) ->
      (match ref.morv_module with 
        | None -> 
            let funvar = 
              (match ref.morv_index with
                | None -> 
                    (find_var_in_prog ref.morv_varname arg2valMap)
                | Some idx_lst -> 
                    let idx_lst = List.map (apply_motype_val toplevel arg2valMap) idx_lst in
                    apply_in_list toplevel arg2valMap ref idx_lst 
              )
            in
            (apply_fun toplevel arg2valMap funvar (List.map (apply_motype_val toplevel arg2valMap) argslst) )
        | Some m -> 
            (match IntMap.find m (get_fullprog ()).mofp_progmodules with
              | MOUserMod modul -> 
                let funvar = 
                  (match ref.morv_index with
                    | None -> 
                        (find_var_in_prog ref.morv_varname modul.mopg_topvar)
                    | Some idx_lst -> 
                        let idx_lst = List.map (apply_motype_val toplevel arg2valMap) idx_lst in
                        apply_in_list toplevel arg2valMap ref idx_lst 
                  )
                in
              (apply_fun toplevel arg2valMap funvar (List.map (apply_motype_val toplevel arg2valMap) argslst)) 
              | MOSystemMod msymodule -> 
              (apply_core_fun arg2valMap (find_var_in_sysmod ref.morv_varname msymodule) (List.map (apply_motype_val toplevel arg2valMap) argslst) )
              )
      )
    | MOEnvRef_val (var) ->
      MOSimple_val (MOBase_val (MOTypeStringVal (get_var (get_shenv ()) var)))
    | MOBasicFunBody_val (op, arga, argb) -> apply_basic_fun toplevel arg2valMap op arga argb
    | MOBind_val mbind ->
        apply_binding toplevel arg2valMap mbind
    | MOIf_val (cond, thn, els) -> 
        (match check_is_true (apply_motype_val toplevel arg2valMap cond) with
        | true -> apply_motype_val toplevel arg2valMap thn
        | _ -> apply_motype_val toplevel arg2valMap els
        )
    | MOComp_val (opcomp, leftval, rightval) ->
      apply_opcomp toplevel arg2valMap opcomp leftval rightval 
    | MOBody_val bodylst -> 
        apply_in_body toplevel arg2valMap bodylst


let exec prog shenv=
  set_shenv shenv ;
  set_fullprog prog ;
  (*play simple variable (function with 0 arguments) *)
  let prog = 
    {prog with
      mofp_mainprog = 
        {prog.mofp_mainprog with mopg_topvar = 
          IntMap.map 
            (fun topvar -> 
              match topvar with
                | MOTop_val topvar -> 
                  MOTop_val (apply_motype_val false prog.mofp_mainprog.mopg_topvar topvar)
                | MOTupEl_val _ -> topvar
            )
            prog.mofp_mainprog.mopg_topvar
        }
    }
  in
  set_fullprog prog ;
  (*play main expr*)
  let start_expr = prog.mofp_mainprog.mopg_topcal in
  let res = apply_motype_val true prog.mofp_mainprog.mopg_topvar start_expr in
  let redprog = 
    { 
      prog with mofp_mainprog = 
        {
          prog.mofp_mainprog with mopg_topcal= res
        }
    } in 
  redprog, get_shenv ()
