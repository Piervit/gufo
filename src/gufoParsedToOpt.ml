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

(*The transformer from low level representation to high level representation
 * and the type checker.*)

open GufoParsed
open Gufo
open Gufo.MCore
open Gufo.MCore.SimpleCore
open GenUtils
open GufoUtils
open GufoOptUtils
open Format
open Printf
open GufoLocHelper
open GufoParsedHelper

(*################## DEBUG OR PRINTING ##################*)
let debug_var_type var_types = 
  debug_print "dumping var_types" ; 
  IntMap.iter
    (fun i vmap -> 
      debug_print (sprintf "Module %d \n" i); 
      IntMap.iter
        (fun iv (typ, _pars_pos) -> 
          debug_print (sprintf "var %d : %s \n" iv (type_to_string typ)); 
        )
        vmap
    )
    var_types

(*If debug is enable, dump a full representation the parsed program. *)
let dump_prog_representation optiprog = 
  match GufoConfig.get_debug_level () with
    | GufoConfig.DBG_NO_DEBUG -> ()
    | GufoConfig.DBG_INFO
    | GufoConfig.DBG_FULL ->
        (debug_print "Dumping program representation\n");
        (debug_print "*** Topvars \n");
        IntMap.iter
        (fun i topv -> 
          debug_print (sprintf "let $%d = %s \n" i (topvar_to_string topv))
        )
        optiprog.mopg_topvar;
        (debug_print "*** Topcall \n");
        (debug_print (sprintf "%s \n"(moval_to_string optiprog.mopg_topcal.loc_val)))
 

(*################## END DEBUG OR PRINTING ##################*)

(*##################  POSITION HANDLING ################## *)
(*This part is a mecanism used to track position of expressions within the code.*)

  type moPosElement = 
    | PosComposedType of string

  type moPosition = moPosElement list

  let add_to_position curPos posElement = 
    posElement::curPos

(*This is for MComposed_val(the first kind of val . )*)
  let add_composedType_to_position curPos fulloptiprog imod idTyp = 
  let debugname = 
    match imod with 
      | None -> ((IntMap.find idTyp fulloptiprog.mofp_mainprog.mopg_types).moct_debugname)
      | Some (MOUserMod userprog) -> (IntMap.find idTyp userprog.mopg_types).moct_debugname
      | Some (MOSystemMod sysprog) -> (IntMap.find idTyp sysprog.mosm_types).mosmt_name
  in
    add_to_position curPos debugname

(*TODO for other vals.*)
 
(*##################  END POSITION HANDLING ################## *)

(*##################  ERROR MESSAGE ################## *)

let raise_varError msg pos = 
  let msg = Printf.sprintf "%s Position: %s" msg 
            (GufoParsedHelper.string_of_position pos) in
  raise (VarError msg )




(*##################  END ERROR MESSAGE ################## *)


(** transformation from gufoParsed to gufo.core **)

(*First part of the job will be to get every modules that we have to import. *)

let search_modules mprogram =
  let add_module_from_mref module2int mref =
    let add_if_not_already modul map = 
      match StringMap.mem modul map with
        | true -> map
        | false -> 
                    StringMap.add modul (get_fresh_int ()) map

    in
    (*first look at the variable *)
    let map = 
      match mref.mrv_module with
        | None -> module2int 
        | Some modul -> add_if_not_already modul module2int
    in
    List.fold_left 
      (fun map curfieldname -> 
        match start_with_uppercase curfieldname with
          | true -> add_if_not_already curfieldname map
          | false -> map
      )
      map
      mref.mrv_varname.loc_val
  in
  (*first look in types declaration*)
  let module2int = StringMap.fold 
                    (fun _typ_name mtyp module2int -> 
                      fold_over_mref_type add_module_from_mref module2int mtyp) 
                    mprogram.mpg_types StringMap.empty     
  in
  (*then in variables *)
  let module2int = List.fold_left (fun module2int mvar ->
                    fold_over_mref_val 
                      add_module_from_mref
                      module2int mvar.mva_value.loc_val)
                    module2int mprogram.mpg_topvar
  in
  (*then in toplevel*)
  let module2int = fold_over_mref_val add_module_from_mref module2int mprogram.mpg_topcal.loc_val
  in module2int

  (* PART 3: TYPE DETERMINATION *)

(*Add a constraint to the constraint set.*)
let rec add_constraints id typs constraint_map = 
  let typsAsSet = (TypeSet.of_list typs) in
  match (IntMap.find_opt id constraint_map) with
    | None -> IntMap.add id typsAsSet constraint_map
    | Some constSet -> IntMap.add id (TypeSet.union typsAsSet constSet) constraint_map


(*See the comment of the box_val function. 
  At some stage, we box values within variables to be sure to register every
  constraints we need.
  This extract_boxed_value function, return the variable id the boxed value.
*)
and extract_boxed_value boxed_val =
  match boxed_val.loc_val with
    | MOBind_val bd_val ->
        (*Boxed value should not contains tuple, so we can just take the first
        element.*)
        let id,_loc_pos,_  = List.hd (bd_val.mobd_name) in 
        let aval = bd_val.mobd_value in
        id,aval
    | _ -> raise (InternalError (sprintf "Gufo type checker internal erreur."))

(*fonction retrieving the type of the element of a tuple according to its position.
 * tupl_typ is the general type of the tuple while position is the position of the element.*)
let rec get_type_from_tuple_el dbg_pos tupl_typ position = 
  match position with 
    | [] -> raise (InternalError "Gufo internal error.")
    | [last_pos] -> 
        (match tupl_typ.loc_val with
          | MOTuple_type subtyplst -> 
              (try List.nth subtyplst last_pos
                with _ -> raise_typeError "The tuple value cannot be assigned to the assigned variable tuples." dbg_pos
              )
          | MORef_type (modul, iref, idx, args) -> 
              ({tupl_typ with 
                 loc_val = MOTupel_type (modul, iref, idx, args, position)})
          | _ -> raise_typeError "Tuple is not correctly matched. \n"  dbg_pos
        )
    | i::lst -> 
        (match tupl_typ.loc_val with
          | MOTuple_type subtuple -> 
              get_type_from_tuple_el dbg_pos 
                                     (List.nth subtuple i)
                                     lst
          | MORef_type (modul, iref, idx, args)-> 
              ({tupl_typ with loc_val = MOTupel_type (modul, iref, idx, args, position)})
          | _ -> raise (InternalError "type checker error.")
        )


let rec determine_constraint_cmdseq fulloptiprog optiprog constraint_map cmdseq = 
    let determine_constraint_stringOrRef constraint_map sor =
      match sor with
        | MOSORString sloc -> constraint_map
        | MOSORExpr e -> 
              let id, e = extract_boxed_value e in
              let _, constraint_map = 
                determine_constraint fulloptiprog optiprog constraint_map e 
              in
                add_constraints id 
                                [({e with loc_val = MOBase_type MTypeString})]
                                constraint_map
    in
  
  let determine_constraint_cmd constraint_map cmd =
    (*we have to check the arguments and the input/output*)
    let constraint_map = 
      List.fold_left 
        (fun constraint_map arg -> 
              determine_constraint_stringOrRef constraint_map arg
        ) constraint_map cmd.loc_val.mocm_args 
    in
    let constraint_map = 
      match cmd.loc_val.mocm_output with
        | MOCMDOStdOut
        | MOCMDOStdErr -> constraint_map
        | MOCMDOFile sor 
        | MOCMDOFileAppend sor -> determine_constraint_stringOrRef constraint_map sor
    in
    let constraint_map = 
      match cmd.loc_val.mocm_outputerr with
        | MOCMDEStdOut
        | MOCMDEStdErr -> constraint_map
        | MOCMDEFile sor 
        | MOCMDEFileAppend sor -> determine_constraint_stringOrRef constraint_map sor
    in
    let constraint_map = 
      match cmd.loc_val.mocm_input_src with
        | MOCMDIStdIn -> constraint_map
        | MOCMDIFile sor -> determine_constraint_stringOrRef constraint_map sor
    in
    MTypeCmd, constraint_map
  in
  match cmdseq.loc_val with 
    | MOSimpleCmd cmd -> determine_constraint_cmd constraint_map cmd
    | MOForkedCmd cmdseq -> 
        determine_constraint_cmdseq fulloptiprog optiprog constraint_map cmdseq
    | MOAndCmd (cmdseqa, cmdseqb) 
    | MOOrCmd (cmdseqa, cmdseqb) 
    | MOSequenceCmd (cmdseqa, cmdseqb) 
    | MOPipedCmd (cmdseqa, cmdseqb) ->
        let _typ, _locScope = 
          determine_constraint_cmdseq fulloptiprog optiprog constraint_map cmdseqa
        in
        determine_constraint_cmdseq fulloptiprog optiprog constraint_map cmdseqb

and determine_constraint_base_val fulloptiprog optiprog constraint_map bv = 
  let typ, locScope = 
    match bv with
      | MOTypeStringVal s -> MTypeString, constraint_map
      | MOTypeBoolVal b -> MTypeBool, constraint_map
      | MOTypeIntVal i -> MTypeInt, constraint_map 
      | MOTypeFloatVal f-> MTypeFloat, constraint_map
      | MOTypeCmdVal cseq -> 
          determine_constraint_cmdseq fulloptiprog optiprog constraint_map cseq
  in 
    MOBase_type typ, constraint_map

and determine_constraint_composed fulloptiprog optiprog constraint_map mct = 
  let modul, id = mct.mocv_resolved_type in 
  match modul with
    | None -> MOComposed_type (IntMap.find id optiprog.mopg_types)
    | Some i -> 
        match IntMap.find i fulloptiprog.mofp_progmodules with
          | MOUserMod prog -> MOComposed_type (IntMap.find id prog.mopg_types)
          | MOSystemMod sysmod ->  
              MOComposed_type (GufoModules.sysmodctype_to_ctype 
              (IntMap.find i sysmod.mosm_types))


and determine_constraint_comp fulloptiprog optiprog constraint_map op arga argb =
    (*next two line are for locScope precision*)
    let id_a, vala = extract_boxed_value arga in
    let id_b, valb = extract_boxed_value argb in
    let typa, constraint_map = determine_constraint_
                           fulloptiprog optiprog constraint_map vala in
    let typb, constraint_map = determine_constraint_
                           fulloptiprog optiprog constraint_map valb in
    let constraint_map = add_constraints id_a 
                                         [{vala with loc_val = typa};
                                          {valb with loc_val = typb};
                                         ] 
                                         constraint_map in
    let constraint_map = add_constraints id_b 
                                         [{vala with loc_val = typa};
                                          {valb with loc_val = typb}] 
                                         constraint_map in
      MOBase_type (MTypeBool), constraint_map
 

and determine_constraint_basic_fun fulloptiprog optiprog constraint_map op arga argb = 
  let for_op expectedType arga argb constraint_map =
    let ida, arga_unbox = extract_boxed_value arga in
    let idb, argb_unbox = extract_boxed_value argb in
    let typa, constraint_map = 
      determine_constraint_ fulloptiprog optiprog constraint_map arga_unbox in
    let typb, constraint_map = 
      determine_constraint_ fulloptiprog optiprog constraint_map argb_unbox in
    let typaloc = {arga_unbox with loc_val = typa} in
    let typbloc = {argb_unbox with loc_val = typb} in
    let constraint_map = add_constraints ida [typaloc;typbloc;expectedType]
                         constraint_map in
    let constraint_map = add_constraints idb [typaloc;typbloc;expectedType] 
                         constraint_map in
    typa, constraint_map
  in

  let typ,constraint_map= 
    match op.loc_val with 
    | MConcatenation ->
        let expectedType = {op with loc_val = (MOBase_type MTypeString)} in
        for_op expectedType arga argb constraint_map
    | MAddition 
    | MMultiplication 
    | MModulo 
    | MDivision 
    | MSoustraction -> 
        let expectedType = {op with loc_val = (MOBase_type MTypeInt)} in
        for_op expectedType arga argb constraint_map
    | MAdditionFloat 
    | MMultiplicationFLoat
    | MModuloFloat
    | MDivisionFloat
    | MSoustractionFloat -> 
        let expectedType = {op with loc_val = (MOBase_type MTypeFloat)} in
        for_op expectedType arga argb constraint_map
    | MWithList -> 
        let full_or_type = 
          {op with loc_val = 
             MOList_type ({loc_val = MOAll_type (get_fresh_int ()) ;
             loc_pos = pos_merging arga.loc_pos argb.loc_pos;})
          }
        in
        for_op full_or_type arga argb constraint_map
    | MWithSet -> 
        let full_or_type = 
          {op with loc_val = MOSet_type 
                              ({op with loc_val =(MOAll_type (get_fresh_int ()))})} 
        in
        for_op full_or_type arga argb constraint_map
    | MWithMap -> 
        let full_or_type = {op with loc_val = MOMap_type 
              ({op with loc_val = (MOAll_type (get_fresh_int ()))}, 
              {op with loc_val = MOAll_type (get_fresh_int ())}) 
        }
        in
        for_op full_or_type arga argb constraint_map
    | MWithoutSet -> 
        let full_or_type = {op with loc_val = 
          MOSet_type ({op with loc_val = MOAll_type (get_fresh_int ())}) 
        }
        in
        for_op full_or_type arga argb constraint_map
    | MWithoutMap -> 
        let full_or_type = 
          {op with loc_val = 
              MOMap_type ({op with loc_val = (MOAll_type (get_fresh_int ()))},
                          {op with loc_val = MOAll_type (get_fresh_int ())}) }
        in
        for_op full_or_type arga argb constraint_map
    | MHasSet -> raise (InternalError "MHas is deprecated.") 
    | MHasMap -> raise (InternalError "SHas is deprecated.") 
  in 
  typ, constraint_map

and determine_constraint_binding fulloptiprog optiprog constraint_map bd = 
  let determine_in_known_tuple dbg_pos tupl_name tupl_typ constraint_map  = 
    (*we have to check compatibilty between tupl_name and tupl_typ before putting link in constraint_map*)
    List.fold_left 
    (fun constraint_map (id_el,pars_pos, position) -> 
      let curel_typ = get_type_from_tuple_el pars_pos tupl_typ position in
      add_constraints id_el [curel_typ] 
                      constraint_map
    ) constraint_map tupl_name
  in

  let db_typ, constraint_map = determine_constraint_ fulloptiprog optiprog constraint_map bd.mobd_value in

  let constraint_map = 
    match bd.mobd_name, db_typ with 
      | [(single_id, _pars_pos,[])], _ -> 
          add_constraints single_id [{bd.mobd_value with loc_val = db_typ}] 
            constraint_map 
      | tupl_name, MOTuple_type _tupl_typ ->
          determine_in_known_tuple bd.mobd_value.loc_pos tupl_name 
            {bd.mobd_value with loc_val = db_typ} 
            constraint_map
      | tupl_name, MORef_type (modul, iref, deep, args) ->
          List.fold_left 
            (fun constraint_map (id, pars_pos, position) -> 
              add_constraints id 
                              [{loc_val = MOTupel_type(modul, iref, deep,args, position);
                                loc_pos = pars_pos;
                               }] 
                              constraint_map
            )
            constraint_map tupl_name
      | tupl_name, otherType -> raise (InternalError "Type checker error")
  in
  let body_type, constraint_map = 
    determine_constraint_ fulloptiprog optiprog constraint_map bd.mobd_body 
  in
  body_type, constraint_map



(*we only check the ref here, without worying about possible args.*)
and determine_constraint_ref fulloptiprog optiprog constraint_map ref = 

  let rec determine_with_fields fields index = 
    match fields with 
      | [] -> raise (InternalError "Gufo internal error")
      | [field] -> 
        let tfield = get_type_field_from_field fulloptiprog optiprog field in
          tfield.motf_type
      | field::lstfield -> 
          determine_with_fields lstfield index
  in
  let (varname, fields ) = ref.morv_varname in 

  (*we should check if there are field vars*)
  let typ = 
    match fields with 
      | [] -> 
        (match ref.morv_module with
         | None -> 
                {varname with loc_val = MORef_type (None, varname.loc_val, 0, [])}
          | Some i -> 
                   (match ref.morv_index with
                     | None -> 
                        {varname with loc_val = 
                          MORef_type (Some i, varname.loc_val, 0, [] )}
                     | Some idx -> 
                        {varname with loc_val = 
                          MORef_type (Some i, varname.loc_val, List.length idx, [] )}
                   )
        )
      | lst_fields -> 
          determine_with_fields lst_fields ref.morv_index
  in 
   typ, constraint_map

(*we don't do intermodule type checking for now, so we only check ref
 * defined in this module.*)
and determine_ref_with_index ref ref_vtyp  = 
  (match ref.morv_index with
    | None -> ref_vtyp
    | Some idx -> 
        let idx_deep = List.length idx in 
        let ref_name = ref.morv_debugname in
        let pos = let v,_ = ref.morv_varname in v.loc_pos in
        let rec determine_ref_with_index_ idx_deep ref_vtyp = 
          match idx_deep with
            | 0 -> raise (VarError (sprintf "You are trying to access to the index of a variable (%s) which is not a map or a list." ref_name))
            | 1 -> 
              (match ref_vtyp.loc_val with 
                | MOList_type subtyp -> subtyp
                | MOSet_type( subtyp) ->  subtyp 
                | MOMap_type (_, subtyp) -> subtyp
                | MOAll_type i -> 
                    {loc_val = MOAll_type (get_fresh_int ()); loc_pos = pos}
                | _ -> raise (InternalError 
                        "Gufo internal error: no deep for the given type.") 
                        (*we don't have deep for other types.*)
              )
            | i -> 
              (match ref_vtyp.loc_val with 
                | MOList_type subtyp -> determine_ref_with_index_ (i-1) subtyp 
                | MOSet_type( subtyp) -> determine_ref_with_index_ (i-1) subtyp 
                | MOMap_type (_, subtyp) -> determine_ref_with_index_ (i-1) subtyp 
                | MOAll_type i -> 
                    {loc_val = MOAll_type (get_fresh_int ()); loc_pos = pos}
                | _ -> raise (InternalError 
                        "Gufo internal error: no deep for the given type.") 
                        (*we don't have deep for other types.*)
              )
        in
        determine_ref_with_index_ idx_deep ref_vtyp
    )

(**Determine typ of a reference acting as a function call (with arguments). 
*)
and determine_constraint_ref_function_call fulloptiprog optiprog constraint_map
                                     ref args typ_ref =

  let precise_with_args constraint_map id ref_typ args_typ =
    match args_typ with
      | [] -> 
        add_constraints id.loc_val [ref_typ] constraint_map
      | typ :: oargs_typ -> 
        
        add_constraints id.loc_val
          [{loc_val = (MOFun_type (typ::oargs_typ, 
                        {id with loc_val = MOAll_type (get_fresh_int ())}));
            loc_pos = id.loc_pos}] 
          constraint_map
  in
  (*Determine type of arguments.*)
  let lst_typ_args, constraint_map = 
    List.fold_left 
    (fun (lst_typ_args, constraint_map ) arg -> 
      let typ, constraint_map = 
        determine_constraint fulloptiprog optiprog constraint_map arg 
      in
      typ::lst_typ_args, constraint_map  
    ) ([], constraint_map ) args 
  in
  let lst_typ_args = List.rev lst_typ_args in
  let idloc,_ = ref.morv_varname in

  let constraint_map = 
    precise_with_args constraint_map idloc typ_ref lst_typ_args 
  in
  let expr_typ = determine_ref_with_index ref typ_ref in 
  expr_typ, constraint_map

(* 
 *
 * This function is the central constraint creator of the type checking part.
 * It should determine every constraints of an expr e.
 *
 * fulloptiprog is the representation of the full programs in optimised representation
 * optiprog is the current module in optimised representation
 * 
 * constraint_map is the current map of constraint.
 *
 * e is the expression value we want to determine the type.
 *
 * It returns the found type and the updated locScope.
 *
 * *)
and determine_constraint fulloptiprog optiprog constraint_map e =
  let typ, constraint_map = determine_constraint_ 
                              fulloptiprog optiprog constraint_map e in
  {e with loc_val = typ}, constraint_map

and determine_constraint_ fulloptiprog optiprog constraint_map e =
  let typ_res, constraint_map = 
    (match e.loc_val with
      | MOComposed_val mct -> 
          determine_constraint_composed fulloptiprog optiprog constraint_map mct,
          constraint_map
      | MOSimple_val st -> 
          (match st with 
            | MOBase_val bv -> 
                determine_constraint_base_val fulloptiprog optiprog constraint_map bv
            | MOTuple_val tv -> 
                let  nlst, constraint_map = 
                (List.fold_left 
                      (fun (ntv, constraint_map) tvel -> 
                        let (nel, constraint_map) =
                          determine_constraint_ fulloptiprog optiprog constraint_map tvel in
                        ( {tvel with loc_val = nel}::ntv, constraint_map)
                      )
                      ([], constraint_map) tv.loc_val
                )
                in MOTuple_type (List.rev nlst), 
                    constraint_map
            | MOList_val lv ->
                let lst_el_constraints, constraint_map = 
                  List.fold_left 
                    (fun (constraints,constraint_map) lval -> 
                      let _id, expr = extract_boxed_value lval in
                      let typ, constraint_map = determine_constraint_
                        fulloptiprog optiprog constraint_map expr
                      in 
                      {expr with loc_val = typ}::constraints, constraint_map
                    )
                  ([],constraint_map) lv.loc_val
                in 
                let constraint_map = 
                  List.fold_left 
                    (fun (constraint_map) lval -> 
                      let id, _expr = extract_boxed_value lval in
                      add_constraints id lst_el_constraints constraint_map
                    )
                    constraint_map lv.loc_val
                in
                  MOList_type ((List.hd lst_el_constraints)), constraint_map
            | MONone_val -> 


                MOOption_type 
                  ({e with loc_val = MOAll_type (get_fresh_int ())}), constraint_map
            | MOSome_val el -> 
                let opt_typ, constraint_map = 
                  determine_constraint fulloptiprog optiprog constraint_map el 
                in
                MOOption_type (opt_typ), constraint_map
            | MOSet_val sett -> 
                let lst_el_constraints, constraint_map = 
                  MSet.fold
                    (fun lval (constraints,constraint_map) -> 
                      let _id, expr = extract_boxed_value (simple_to_core_val lval) 
                      in
                      let typ, constraint_map = determine_constraint_ 
                        fulloptiprog optiprog constraint_map expr
                      in 
                      {expr with loc_val = typ}::constraints, constraint_map
                    )
                  sett.loc_val ([],constraint_map) 
                in 
                let constraint_map = 
                  MSet.fold
                    (fun lval (constraint_map) -> 
                      let id, _expr = extract_boxed_value (simple_to_core_val lval) 
                      in
                      add_constraints id lst_el_constraints constraint_map
                    )
                    sett.loc_val (constraint_map) 
                in
                  MOSet_type (List.hd lst_el_constraints), constraint_map
            | MOMap_val map -> 
                let constraints_key, constraints_el, constraint_map = 
                  MMap.fold
                    (fun key el
                          (constraints_key, constraints_el ,constraint_map)
                       -> 
                      let _id, exprk = 
                        extract_boxed_value (simple_to_core_val key) 
                      in
                      let _id, exprel = extract_boxed_value el in
                      let typk, constraint_map = determine_constraint_
                        fulloptiprog optiprog constraint_map exprk
                      in 
                      let typv, constraint_map = determine_constraint_
                        fulloptiprog optiprog constraint_map exprel
                      in 
                      ({exprk with loc_val = typk}::constraints_key),
                      ({exprel with loc_val = typv}::constraints_el), 
                      constraint_map)
                  map.loc_val ([], [] ,constraint_map) 
                in 
                let constraint_map = 
                  MMap.fold
                    (fun key el (constraint_map) -> 
                      let idk, _exprk = extract_boxed_value (simple_to_core_val key) in
                      let idel, _exprel = extract_boxed_value el in
                      let constraint_map = 
                        add_constraints idk  constraints_key constraint_map
                      in
                        add_constraints idel constraints_el constraint_map
                    )
                    map.loc_val (constraint_map) 
                in
                  MOMap_type((List.hd constraints_key), 
                              (List.hd constraints_el)), 
                  constraint_map
            | MOFun_val fv ->
                let body_type, constraint_map = 
                  determine_constraint fulloptiprog optiprog constraint_map
                                 fv.loc_val.mofv_body 
                in

                let rec funarg_to_ref constraint_map  funarg =
                  match funarg with 
                     | MOBaseArg bt -> 
                         (add_constraints bt.loc_val 
                             [{bt with loc_val= MORef_type (None, bt.loc_val, 0, [])}] 
                             constraint_map),
                         {bt with loc_val = MORef_type (None, bt.loc_val, 0, [])}
                     | MOTupleArg bt ->
                         let constraint_map, tupTyp = 
                           List.fold_left_map
                            (fun constraint_map bt -> 
                              funarg_to_ref constraint_map bt
                            )
                            constraint_map bt.loc_val
                          in constraint_map, {bt with loc_val = MOTuple_type tupTyp}
                in
                let constraint_map, args_type = 
                  List.fold_left_map 
                    (fun constraint_map funarg -> 
                      funarg_to_ref constraint_map funarg 
                    )
                    constraint_map
                    fv.loc_val.mofv_args_id.loc_val
                in
                MOFun_type (args_type, body_type), constraint_map
            | MOEmpty_val ->
              MOUnit_type, constraint_map
          )
      | MORef_val (ref, args) -> 
          (*we use locScope and only put to MOAll_type if not found in scope.*)
          (*we don't do intermodule type checking for now, so we only check ref
           * defined in this module.*)
          let typ_ref, constraint_map = 
            determine_constraint_ref fulloptiprog optiprog constraint_map ref 
          in
          let ref_id = let tmp,_ = ref.morv_varname in tmp in
          (match args with 
            | [] -> 
              let typ_ref = determine_ref_with_index ref typ_ref in
(*
              let constraint_map = 
                add_constraints ref_id.loc_val 
                                [typ_ref] 
                                constraint_map 
              in
*)
              (typ_ref.loc_val, constraint_map)
            | args -> 
                let expr_typ, constraint_map = 
                  determine_constraint_ref_function_call 
                    fulloptiprog optiprog constraint_map ref args typ_ref in
                expr_typ.loc_val, constraint_map
          )
      | MOEnvRef_val _ -> 
        MOBase_type (MTypeString), constraint_map
      | MOBasicFunBody_val (op, arga, argb)
        ->
          determine_constraint_basic_fun 
            fulloptiprog optiprog constraint_map op arga argb
      | MOBind_val bd ->
          determine_constraint_binding fulloptiprog optiprog constraint_map bd
      | MOIf_val (cond, thn, els) ->
          (*for now we do not worry about cond as it does not give info about
           * return type.*)
          let idcond, econd = extract_boxed_value cond in
          let idthn, ethn = extract_boxed_value thn in
          let idels, eels = extract_boxed_value els in
          let cond_typ, constraint_map = 
            determine_constraint_ fulloptiprog optiprog constraint_map econd 
          in
          let cond_typ =  {econd with loc_val = cond_typ} 
          in
          let booltype = {econd with loc_val = (MOBase_type MTypeBool)} in
          let constraint_map = 
            add_constraints idcond [booltype;cond_typ] constraint_map
          in
                            
          let thn_typ, constraint_map = determine_constraint_
                                    fulloptiprog optiprog constraint_map ethn 
          in
          let els_typ, constraint_map = (determine_constraint_
                                    fulloptiprog optiprog constraint_map eels) 
          in
          let thn_typ =  {ethn with loc_val = thn_typ}  in
          let els_typ =  {eels with loc_val = els_typ}  in
          let constraint_map = 
            add_constraints idthn [thn_typ; els_typ] constraint_map
          in
           let constraint_map = 
            add_constraints idels [thn_typ; els_typ] constraint_map
          in
            thn_typ.loc_val, constraint_map
      | MOComp_val (op, a, b ) ->
          (*a comparison can only return a boolean *)
          determine_constraint_comp fulloptiprog optiprog constraint_map op a b
      | MOBody_val bdlst ->
          (*return the type of the last body*)
          let lastbd = List.nth bdlst ((List.length bdlst) -1) in
          determine_constraint_ fulloptiprog optiprog constraint_map lastbd


    )
  in typ_res, constraint_map 

(*Just iter on topval to feed constraint.*)
let module_extract_constraints fulloptiprog optiprog = 
  let constraint_map = 
    IntMap.fold
      (fun id value constraint_map ->
        match value with 
          | MOTop_val e -> 
              let typ, constraint_map = determine_constraint fulloptiprog 
                                          optiprog constraint_map e 
              in
                add_constraints id [typ] constraint_map
          | MOTupEl_val _ -> constraint_map 
          (*TODO: we to do this, maybe later. *)
      )
      optiprog.mopg_topvar IntMap.empty
  in
  let _typ, constraint_map = determine_constraint fulloptiprog optiprog constraint_map optiprog.mopg_topcal in
    constraint_map

let extract_constraints fulloptiprog = 
  let toptypes_modules = IntMap.empty in 
  let toptypes_modules = 
    IntMap.fold 
      (fun modi el toptypes_modules-> 
        match el with 
          | MOSystemMod s -> 
              IntMap.add modi (GufoModules.get_types_map s) toptypes_modules 
          | MOUserMod optiprog -> (*TODO: if depend on nothing, gen type?*)
              IntMap.add modi (module_extract_constraints 
                                fulloptiprog optiprog) toptypes_modules
      )
    fulloptiprog.mofp_progmodules
    toptypes_modules
  in
  (*for main prog *)
  let toptypes_modules = IntMap.add 0 
                           (module_extract_constraints fulloptiprog 
                              fulloptiprog.mofp_mainprog) toptypes_modules in
  toptypes_modules


  (* END PART 3: TYPE DETERMINATION *)


  (* PART 2: TRANSFORMATION OF EXPR *)
  (* From parsed to opt but with only minimal type checking (deductible from 
   * the type transformation.*)

let get_bind_vars_from_topvar topvar = 
  let get_bind_debugnames acc bind = 
    IntMap.fold
      (fun vid vname acc -> 
        let curset = 
          match StringMap.find_opt vname acc with
            | None -> IntSet.empty
            | Some set -> set
        in
        StringMap.add vname (IntSet.add vid curset) acc
      )
    bind.mobd_debugnames
    acc
  in
  let get_fun_debugnames acc fv = 
    let onames_intset = 
    StringMap.map 
      (fun id -> IntSet.singleton id)
      fv.mofv_args_name
    in 
    StringMap.merge 
      (fun s a b -> 
        match a, b with
          | _, Some b -> Some b
          | Some a, _ -> Some a
          | _ -> None
      ) 
      acc onames_intset 
  in
    fold_over_obinding_and_ofun_val get_bind_debugnames get_fun_debugnames StringMap.empty topvar
   


  (*we want to fill mopg_topvar.
   *
   * At that point mainprog and modules of fulloptiprog are set with
   * the followings filled fields:
   * - mopg_types
   * - mopg_field_to_type
   * - mopg_field2int
   * - mopg_topvar2int
   * - mopg_ctype2int 
   *
   * past_var_map is set to None when we are not in the console interactive. In
   * console interactive mode it contains a map which contains as key a new int
   * id for topvar and as value, the previous id value for the "same string"
   * var.
   *
   * *)
let parsedToOpt_topval fulloptiprog oldprog optiprog is_main_prog past_var_map =

(*
box a value within a variable.
  in gufo code, "aval" become "let $x = aval in ($x) ." 
  The goal of the boxing is to give information to the type checker: 
  The type checker first collect constraints (on variables) and then solve the
  constraint.
  We cannot express constraint over expressions, so adding interna variables
  though this boxing allows to have variables where it is needed.
  
*)
  let rec box_val ?topvar:(topvar = None) locScope aval = 

    let new_internal_var = get_fresh_int () in
      {aval with loc_val = MOBind_val 
      {
       mobd_name = [(new_internal_var, aval.loc_pos ,[])];
       mobd_debugnames = IntMap.empty;
       mobd_value = parsedToOpt_expr ~topvar optiprog locScope aval;
       mobd_body = {aval with loc_val = 
                     MORef_val( {
                     morv_module = Some (optiprog.mopg_name); 
                     morv_varname =  {aval with loc_val=new_internal_var} , [];
                     morv_index = None; 
                     morv_debugname = "";
                 },[])} ;
       }}
 

  and parsedToOpt_composed_val ?topvar:(topvar = None) optiprog locScope mct = 
    (*the field name allow to recognize the composed type, we then check every
     * field has name according to the same object.*)
    let parsedToOpt_field def_modul fd =
      let parsedToOpt_field_in_prog prog fd = 
          let fdv = fd.loc_val in
          match StringMap.find_opt fdv.mtfv_name.loc_val prog.mopg_field2int with 
            | None -> raise_typeError ("field "^fdv.mtfv_name.loc_val ^" does not belong to a known type") fdv.mtfv_name.loc_pos
            | Some i -> IntMap.find i prog.mopg_field_to_type
      in
      match def_modul with 
        | None -> parsedToOpt_field_in_prog optiprog fd
        | Some (MOUserMod userprog) -> parsedToOpt_field_in_prog userprog fd
        | Some (MOSystemMod sysmod) -> 
            (match StringMap.find_opt fd.loc_val.mtfv_name.loc_val sysmod.mosm_typstrfield2inttype with
              | None -> raise_typeError ("field "^fd.loc_val.mtfv_name.loc_val ^
                        " does not belong to a known type") fd.loc_val.mtfv_name.loc_pos
              | Some i -> i)
    in
    let check_valid_fields def_modul ctype field =
      let check_valid_fields_in_prog prog = 
        (*check the field belong to the given ctype *)
          match StringMap.find_opt field.mtfv_name.loc_val prog.mopg_field2int with 
            | None -> raise_typeError ("field "^field.mtfv_name.loc_val ^" does not belong to a known type") field.mtfv_name.loc_pos
            | Some i -> ctype = IntMap.find i prog.mopg_field_to_type
      in
        match def_modul with
          | None -> check_valid_fields_in_prog optiprog 
          | Some (MOUserMod userprog) -> check_valid_fields_in_prog userprog
          | Some (MOSystemMod sysmod) -> 
            (match StringMap.find_opt field.mtfv_name.loc_val sysmod.mosm_typstrfield2inttype with 
              | None ->raise_typeError ("field "^field.mtfv_name.loc_val^" does not belong to a known type") field.mtfv_name.loc_pos
              | Some i -> ctype = i
            )
    in
    let add_to_field_map def_modul locScope fd fieldsmap = 
      let fdv = fd.loc_val in
      let iname = 
        match def_modul with
          | None -> (StringMap.find fdv.mtfv_name.loc_val optiprog.mopg_field2int)
          | Some (MOUserMod userprog ) -> (StringMap.find fdv.mtfv_name.loc_val userprog.mopg_field2int)
          | Some (MOSystemMod sysmod) -> (StringMap.find fdv.mtfv_name.loc_val sysmod.mosm_typstrfield2int)
      in
      IntMap.add iname (parsedToOpt_expr ~topvar optiprog locScope fdv.mtfv_val) fieldsmap
    in
  
    let def_modul, def_modul_i = 
      match mct.mcv_module_def with
        | None -> None , None
        | Some modstr -> 
          let modulint = 
            (match StringMap.find_opt modstr.loc_val fulloptiprog.mofp_progmap with
              | None -> raise (VarError (modstr.loc_val^" does not appear to be an available module."))
              | Some i -> i 
            )
          in
          Some (IntMap.find modulint fulloptiprog.mofp_progmodules ), Some modulint
    in
    let field1 = (List.hd mct.mcv_fields) in
    let ctype =  parsedToOpt_field def_modul field1 in
    let _check = 
      List.iter 
        (fun fd -> 
          match check_valid_fields def_modul ctype fd.loc_val with
            | true -> ()
            | false -> raise_typeError (fd.loc_val.mtfv_name.loc_val^" does not belong to the correct struct.") fd.loc_val.mtfv_name.loc_pos
        )
        (List.tl mct.mcv_fields)
    in
    let mo_fields = 
      List.fold_left
        (fun map fd -> add_to_field_map def_modul locScope fd map)
        IntMap.empty mct.mcv_fields
    in
    MOComposed_val {
        mocv_module_def = 
          (match mct.mcv_module_def with 
            | None -> None
            | Some mdstring -> 
                Some (StringMap.find mdstring.loc_val fulloptiprog.mofp_progmap ))
            ;
        mocv_fields = mo_fields;
        mocv_resolved_type = (def_modul_i, ctype);
      }
  
  and parsedToOpt_stringOrRef ?topvar:(topvar = None) optiprog locScope sor = 
    match sor with 
    | SORString s -> MOSORString s
    | SORExpr tval -> 
        MOSORExpr (box_val ~topvar locScope tval)


  and parsedToOpt_cmd_output ?topvar:(topvar = None) optiprog locScope cout = 
    match cout with 
      | MCMDOStdOut -> MOCMDOStdOut
      | MCMDOStdErr -> MOCMDOStdErr
      | MCMDOFile sor -> MOCMDOFile (parsedToOpt_stringOrRef ~topvar optiprog locScope sor)
      | MCMDOFileAppend sor -> MOCMDOFileAppend (parsedToOpt_stringOrRef ~topvar optiprog locScope sor)
  
  and parsedToOpt_cmd_outputerr ?topvar:(topvar = None) optiprog locScope coute = 
    match coute with 
      | MCMDEStdOut -> MOCMDEStdOut
      | MCMDEStdErr -> MOCMDEStdErr
      | MCMDEFile sor -> MOCMDEFile (parsedToOpt_stringOrRef ~topvar optiprog locScope sor)
      | MCMDEFileAppend sor -> MOCMDEFileAppend (parsedToOpt_stringOrRef ~topvar optiprog locScope sor)
  
  and parsedToOpt_cmd_input_src ?topvar:(topvar = None) optiprog locScope min =
    match min with
    | MCMDIStdIn -> MOCMDIStdIn
    | MCMDIFile sor -> MOCMDIFile (parsedToOpt_stringOrRef ~topvar optiprog locScope sor)
  
  
  and parsedToOpt_cmd_val ?topvar:(topvar = None) optiprog locScope cseq = 
    let cseqv = cseq.loc_val in
    {cseq with loc_val = 
      {
        mocm_cmd = cseqv.mcm_cmd;
        mocm_args = List.map (parsedToOpt_stringOrRef ~topvar optiprog locScope) cseqv.mcm_args;
        mocm_output = parsedToOpt_cmd_output ~topvar optiprog locScope cseqv.mcm_output;
        mocm_outputerr = parsedToOpt_cmd_outputerr ~topvar optiprog locScope cseqv.mcm_outputerr;
        mocm_input_src = parsedToOpt_cmd_input_src ~topvar optiprog locScope cseqv.mcm_input_src;
        mocm_res = None;
        mocm_input = None;
        mocm_print = None;
        mocm_print_error = None;
        mocm_print_std = None;
      }
    }
  
  and parsedToOpt_cmd_seq_val ?topvar:(topvar = None) optiprog locScope cseq = 
    match cseq.loc_val with 
      | SimpleCmd cval -> 
          {cseq with 
            loc_val = MOSimpleCmd 
              (parsedToOpt_cmd_val ~topvar optiprog locScope cval)
          }
      | ForkedCmd fcmd -> 
          {cseq with 
            loc_val = MOForkedCmd 
              (parsedToOpt_cmd_seq_val ~topvar optiprog locScope fcmd)
          }
      | AndCmd (cseq1, cseq2) -> 
          {cseq with 
            loc_val = MOAndCmd 
              (parsedToOpt_cmd_seq_val ~topvar optiprog locScope cseq1,
                    parsedToOpt_cmd_seq_val ~topvar optiprog locScope cseq2)
          }
      | OrCmd (cseq1, cseq2)-> 
          {cseq with 
            loc_val = MOOrCmd 
              (parsedToOpt_cmd_seq_val ~topvar optiprog locScope cseq1,
                   parsedToOpt_cmd_seq_val ~topvar optiprog locScope cseq2)
          }
      | SequenceCmd (cseq1, cseq2)-> 
          {cseq with
            loc_val = MOSequenceCmd 
              (parsedToOpt_cmd_seq_val ~topvar optiprog locScope cseq1,
                         parsedToOpt_cmd_seq_val ~topvar optiprog locScope cseq2)
          }
      | PipedCmd (cseq1, cseq2)-> 
          {cseq with 
            loc_val = MOPipedCmd 
              (parsedToOpt_cmd_seq_val ~topvar optiprog locScope cseq1,
                      parsedToOpt_cmd_seq_val ~topvar optiprog locScope cseq2)
          }
  
  
  and parsedToOpt_base_val ?topvar:(topvar = None) optiprog locScope bv = 
    match bv with
      | MTypeStringVal s -> MOTypeStringVal s
      | MTypeBoolVal b -> MOTypeBoolVal b
      | MTypeIntVal i -> MOTypeIntVal i
      | MTypeFloatVal f-> MOTypeFloatVal f
      | MTypeCmdVal cseq -> 
          MOTypeCmdVal (parsedToOpt_cmd_seq_val ~topvar optiprog locScope cseq)
  
  and parsedToOpt_funarg nameMap locScope funarg =
   match funarg with  
    | MBaseArg s -> 
        let i = get_fresh_int () in
        StringMap.add s.loc_val i nameMap, 
        MOBaseArg {loc_val = i; loc_pos = s.loc_pos}, 
        StringMap.add s.loc_val (i, s.loc_pos) locScope
    | MTupleArg arglst ->
        let onames, locScope, newlst = 
          List.fold_left
            (fun (onames,locScope,newlst) arg -> 
              let nonames, el, locScope = 
                parsedToOpt_funarg nameMap locScope arg
              in
              (StringMap.merge (fun s a b -> a) onames nonames, locScope, el::newlst)
            )
            (StringMap.empty,locScope,[]) arglst.loc_val
        in
        onames, 
        MOTupleArg {loc_val=newlst; loc_pos=arglst.loc_pos}, 
        locScope
    
  
  and parsedToOpt_simple_val ?topvar:(topvar = None) optiprog locScope ms =
    match ms with 
    | MEmpty_val -> MOEmpty_val
    | MBase_val bv -> 
        MOBase_val (parsedToOpt_base_val ~topvar optiprog locScope bv)
    | MTuple_val tup -> 
        MOTuple_val ({tup with loc_val = List.map (parsedToOpt_expr ~topvar optiprog locScope) tup.loc_val})
    | MList_val lst -> 
        MOList_val {lst with 
                    loc_val = (List.map 
                                (box_val ~topvar locScope) 
                              lst.loc_val)
                   }
    | MNone_val -> MONone_val
    | MSome_val s -> 
        MOSome_val (parsedToOpt_expr ~topvar optiprog locScope s)
    | MSet_val slst ->
        MOSet_val {slst with 
                    loc_val = 
                      (List.fold_left (fun set el ->
                        MSet.add (core_to_simple_val (box_val ~topvar locScope el)) set
                      )
                      MSet.empty slst.loc_val);
                   }
    | MMap_val mlst -> 
        let map = 
          (List.fold_left 
            (fun map (k,el) ->
              MMap.add (core_to_simple_val (box_val ~topvar locScope k))
                       (box_val ~topvar locScope el) map
          )
          MMap.empty mlst.loc_val);
        in MOMap_val({mlst with loc_val = map})
    | MFun_val (args, body) -> 
      let onames,locScope, ofunargs = 
        List.fold_left 
          (fun (onames,locScope, newlst) arg -> 
            let nonames, oarg, locScope = parsedToOpt_funarg onames locScope arg in
            nonames,locScope, oarg::newlst
          ) 
          (StringMap.empty,locScope,[]) args.loc_val
      in 
      let obody = parsedToOpt_expr ~topvar optiprog locScope body in
      MOFun_val 
        {loc_val = 
          {mofv_args_name = onames; 
           mofv_args_id = {loc_val = ofunargs; loc_pos = args.loc_pos}; 
           mofv_body = obody};
         loc_pos = pos_merging args.loc_pos  obody.loc_pos;
        }
  
  
  and parsedToOpt_ref ?topvar:(topvar = None) optiprog locScope ref  = 

    let resolve_fd prog fdname = 
      match StringMap.find_opt fdname prog.mopg_field2int with
        | None -> raise_typeError 
                    (sprintf "field %s does not belong to a known field." fdname )
                    ref.mrv_varname.loc_pos
        | Some i -> i
    in
    let resolve_fd_modul modul fdname = 
      let imod = StringMap.find modul fulloptiprog.mofp_progmap in 
        match IntMap.find imod fulloptiprog.mofp_progmodules with
        | MOUserMod prog -> Some imod, (resolve_fd prog fdname)
        | MOSystemMod sysmod -> Some imod, StringMap.find fdname sysmod.mosm_typstrfield2int
    in
    let search_in_scope locScope s = StringMap.find_opt s locScope in
    let search_in_topvar optiprog s = StringMap.find_opt s optiprog.mopg_topvar2int 
    in
    let resolve_main_var modul ref = 
      let vname = List.hd ref.mrv_varname.loc_val in
      let id_pos = ref.mrv_varname.loc_pos in
      (*get the variable*)
      let ires = 
        match modul  with
          | None ->
            (match search_in_scope locScope vname with 
              | Some (vint, _pars_pos) -> vint
              | None -> 
                  (match search_in_topvar optiprog vname with
                    | Some vint -> vint
                    | None -> raise (VarError (sprintf "Trying to use undefined variable %s ." vname))
                  )
            )
          | Some MOUserMod prog -> 
                (match search_in_topvar prog vname with
                  | Some vint -> vint
                  | None -> raise (VarError (sprintf "Trying to use undefined variable %s ." vname))
                )
          | Some MOSystemMod sysmod -> 
                (match StringMap.find_opt vname sysmod.mosm_varstr2int with
                  | Some vint -> vint
                  | None -> raise (VarError (sprintf "Trying to use undefined variable %s ." vname))
                )
      in 
      let using_a_topvar topvars past_var_map ires =
        IntSet.find_first_opt(fun topvar -> topvar - ires = 0 ) topvars
      in
      match topvar with
          | None -> {loc_val = ires; loc_pos = id_pos;}
          | Some (topvars, past_var_map) ->
              (
                match using_a_topvar topvars past_var_map ires with
                  | None -> {loc_val = ires; loc_pos = id_pos;} 
                  | Some topvar -> 
                      (*When topvar is the same id as the one found (we want to
                        take value from past_var_map if available in it.)*)
                      (match IntMap.find_opt topvar past_var_map with
                        | None -> raise (VarError (sprintf "Trying to use undefined variable %s ." vname))
                        | Some i -> {loc_val = i ; loc_pos = id_pos;}
                      )
              )

    in
    let resolve_fields ref = 
      let fields = 
        List.fold_left
          (fun (newlst, prec) fdname ->
            match prec,GenUtils.start_with_uppercase fdname with
              | None, false -> (None, (resolve_fd optiprog fdname)) ::newlst, None
              | None, true -> newlst , Some fdname
              | Some modul, false -> (resolve_fd_modul modul fdname) :: newlst, None
              | Some modul, true -> raise (VarError ("Invalid syntax, not possible to have a module reference after another module reference: "^modul^"."^fdname^" ."))
          )
          ([], None) (List.tl ref.mrv_varname.loc_val)
      in
        let newlst,_ = fields in 
       newlst 
    in
    let resolve_index ref = 
      (match ref.mrv_index with
                | None -> None
                | Some vlst -> 
                    Some (List.map (parsedToOpt_expr ~topvar optiprog locScope) 
                    vlst)
      ) 
    in
    match ref.mrv_module with
      | None -> 
          {
            morv_module = None;
            morv_varname = resolve_main_var None ref, resolve_fields ref;
            morv_index = resolve_index ref ;
            morv_debugname = 
              List.fold_left 
                (fun str curstr -> sprintf "%s.%s" str curstr) 
                "" ref.mrv_varname.loc_val;
          }
      | Some s -> 
          let modi = 
            match StringMap.find_opt s fulloptiprog.mofp_progmap with
              | None -> 
                  debug_print (sprintf "Dumping fulloptiprog modules: %s \n"
                          (fulloptiprogModules_to_string fulloptiprog));
                  raise (InternalError (sprintf "Cannot find module %s." s))
              | Some modi -> modi
          in
          {
            morv_module = Some modi;
            morv_varname = 
              resolve_main_var (Some (IntMap.find modi fulloptiprog.mofp_progmodules)) ref, 
              resolve_fields ref;
            morv_index = resolve_index ref ;
            morv_debugname = 
              List.fold_left 
                (fun str curstr -> sprintf "%s.%s" str curstr) "" ref.mrv_varname.loc_val;
          }
  and parsedToOpt_binding ?topvar:(topvar = None) optiprog locScope bd = 
    let add_in_bindlist bindScope bindList debugnames elname position = 
      match StringMap.find_opt elname.loc_val bindScope with
        | None -> 
            let id = get_fresh_int () in 
            (StringMap.add elname.loc_val (id,elname.loc_pos) bindScope), 
            (id, elname.loc_pos,  position)::bindList,
            IntMap.add id elname.loc_val debugnames,
            position
        | Some _ -> (raise_varError ("Cannot bind same variable name in same tuple: variable $"^elname.loc_val^" ." ) elname.loc_pos)
    in
    let rec register_tuple_vars decl bindScope bindList debugnames position = 
      match decl with
        | MBaseDecl str -> 
            let nposition = 
            (match position with
              | [] -> []
              | lst -> 
                   (List.hd lst + 1)::(List.tl lst) 
            )
              in add_in_bindlist bindScope bindList debugnames str nposition
        | MTupDecl decllst -> 
          let nposition = -1::position in
          List.fold_left 
            (fun (bindScope,bindList,debugnames,nposition) decl -> 
              register_tuple_vars decl bindScope bindList debugnames nposition) 
            (bindScope, bindList, debugnames,nposition) decllst
    in 
    let bindScope, bindList, debugnames,_pos = 
      register_tuple_vars bd.mbd_name StringMap.empty [] IntMap.empty [] 
    in 
    let bindList = List.rev bindList in
    let newlocScope = 
      StringMap.union 
        (fun id ellocScope elBindScope -> Some elBindScope) 
        locScope bindScope 
    in
    {
      mobd_name = bindList;
      mobd_debugnames = debugnames;
      mobd_value = 
        (*the idea is the when
         * the var is a function, we want it to be potientially
         * recursive and so it can use reference to itself to be
         * recursive.
         * 
         * When working on something other than a function, we want the a
         * previous definition of the variable to be usable (if there was
         * one).
         * *)
        (match bd.mbd_value.loc_val with 
                | MSimple_val (MFun_val (_, _)) -> parsedToOpt_expr ~topvar optiprog newlocScope bd.mbd_value;
                | _  -> parsedToOpt_expr ~topvar optiprog locScope bd.mbd_value
        );
      mobd_body = parsedToOpt_expr ~topvar optiprog newlocScope bd.mbd_body;
    }


  (*
    This function is the main transformation from the low level representation
    (gufoParsed) to the high level representation (gufo.Core).
      
      topvar: 
        is an optionnal map, allowing to access other toplevel
        definition. This is only set when we are in console mod. It allows to
        retrieve a potential previous id of a variable with same string name.
      optiprog: 
        the current (not fully completed) high level representation.
      locScope: 
        it is the binding to declared var name to their int counterpart.
      expr: 
        the low level expression to convert to a top level expression.
      position:
        retrieve the current position of the expr
  
 *)
  and parsedToOpt_expr ?topvar:(topvar = None) optiprog locScope expr = (* position =*)
    let nexpr_val = 
      match expr.loc_val with
        | MComposed_val mct -> 
            parsedToOpt_composed_val ~topvar optiprog locScope mct
        | MSimple_val sv -> MOSimple_val (parsedToOpt_simple_val ~topvar optiprog locScope sv)
        | MRef_val (ref, args) -> 
            let oref = parsedToOpt_ref ~topvar optiprog locScope ref.loc_val  in
            MORef_val (oref, 
                       List.map 
                         (fun arg -> parsedToOpt_expr ~topvar optiprog locScope arg)
                         args)
        | MEnvRef_val (var) -> 
            MOEnvRef_val var.loc_val
        | MBasicFunBody_val (op, args) ->
            (match args with 
            |  [a;b] -> 
                let ba = box_val ~topvar locScope a in
                let bb = box_val ~topvar locScope b in
                MOBasicFunBody_val (op, ba, 
                                        bb)
            |  _ -> raise_syntaxError "basic operators (+,-,/,mod...) are binary." op.loc_pos
            )
        | MBind_val bd -> 
            MOBind_val (parsedToOpt_binding ~topvar optiprog locScope bd)
        | MIf_val (cond, thn, els) ->
            
              let boxed_cond = box_val ~topvar locScope cond in
              let boxed_thn = box_val ~topvar locScope thn in
              let boxed_els = box_val ~topvar locScope els in
              MOIf_val(boxed_cond,
                     boxed_thn, 
                     boxed_els)
        | MComp_val (op, left, right) -> 
            let boxed_left = box_val ~topvar locScope left in 
            let boxed_right = box_val ~topvar locScope right in 
            MOComp_val (op, boxed_left, boxed_right)
        | MBody_val dblst -> 
            MOBody_val 
              (List.map (fun bd -> parsedToOpt_expr ~topvar optiprog locScope bd) dblst)
    in {expr with loc_val = nexpr_val}
  
  in
  (*return the content optimised val of mvarlst and the information for mopg_topvar_bind_vars*)
  let mapping_mpg_topvar optiprog mvarlst = 
    List.fold_left
      (fun map mvar -> 
        match mvar.mva_name.loc_val with
          | MBaseDecl aname ->
              let iname = (StringMap.find aname.loc_val optiprog.mopg_topvar2int) in
              let res_expr = 
              match mvar.mva_value.loc_val with
              (*the idea is that when the topvar is a function, we want it to
                 be potientially recursive and so it can use reference to
                itself to be recursive.

                When working on something other than a function, we want the a
                previous definition of the variable to be usable (if there was
                one).
                *)
                | MSimple_val (MFun_val (_, _)) ->
                  (parsedToOpt_expr  ~topvar:(None) optiprog 
                    (StringMap.add aname.loc_val (iname, aname.loc_pos) StringMap.empty) 
                    mvar.mva_value)
                | _  ->
                    (match past_var_map with 
                      | None -> parsedToOpt_expr ~topvar:(None) optiprog StringMap.empty mvar.mva_value
                      | Some past_var_map -> 
                          parsedToOpt_expr ~topvar:(Some ((IntSet.singleton iname), 
                                                          past_var_map)) 
                                           optiprog StringMap.empty 
                                           mvar.mva_value
                    )
              in
              IntMap.add iname
                (MOTop_val res_expr) 
                map
          | MTupDecl decl_lst -> 
              let rec add_tup_el map tupel tup_id position = 
                match tupel with
                  | MBaseDecl aname ->
                      let new_position = 
                        match position with
                        | [] -> [0]
                        | position -> ((List.hd position) + 1)::(List.tl position) 
                      in 
                      IntMap.add (StringMap.find aname.loc_val optiprog.mopg_topvar2int)
                        (MOTupEl_val ((val_at_pos tup_id aname.loc_pos), List.rev (new_position))) map, new_position
                  | MTupDecl lst -> 
                      let new_position = 
                        match position with
                        | [] -> [-1;0]
                        | position ->  
                            -1::((List.hd position) + 1)::(List.tl position)
                      in
                      let map, new_position = 
                        List.fold_left 
                          (fun (map,new_position) el -> add_tup_el map el tup_id new_position ) 
                        (map,new_position) lst
                      in 
                        map, List.tl new_position
              in
              (*we compute the value affected to the whole tuple.*)
              let tup_val = 
                (match past_var_map with 
                  | None -> (MOTop_val (parsedToOpt_expr optiprog StringMap.empty mvar.mva_value))
                  | Some past_var_map -> 
                    (*We have to compute the topvar elements *)
                    let inames =
                      let rec find_inames acc decl_lst = 
                        (match decl_lst with
                          | [] -> acc
                          | decl::decl_lst ->
                           (match decl with
                            | MBaseDecl aname -> 
                              find_inames 
                                (IntSet.add (StringMap.find aname.loc_val
                                              optiprog.mopg_topvar2int) acc) 
                                decl_lst
                            | MTupDecl lst_vardecl ->
                                let acc = find_inames acc lst_vardecl in
                                find_inames acc decl_lst
                           )
                        )
                      in find_inames IntSet.empty decl_lst
                    in 
                    (MOTop_val (parsedToOpt_expr ~topvar:(Some (inames, past_var_map)) optiprog StringMap.empty mvar.mva_value))
                )
              in 
              let tup_id = get_fresh_int () in
              let map = IntMap.add tup_id tup_val map in
              (*we also add the elements of the map as MOTupEl_val *)
              let (map, _) = 
                List.fold_left (fun (map,position) decl -> 
                  let map, position = add_tup_el map decl tup_id position in
                  map, List.rev position
                ) (map,[]) decl_lst
              in map
      )
    optiprog.mopg_topvar mvarlst
  in
  let optiprog = 
    let topvar = mapping_mpg_topvar optiprog oldprog.mpg_topvar in
    let bind_vars = 
      IntMap.fold
        (fun i topvar bind_vars -> 
          match topvar with
            | MOTop_val var_var ->
              IntMap.add i (get_bind_vars_from_topvar var_var) bind_vars
            | MOTupEl_val _ -> bind_vars
        )
        topvar
        optiprog.mopg_topvar_bind_vars
    in
    {optiprog with 
      mopg_topvar = topvar;
      mopg_topvar_bind_vars = bind_vars;
    }
  in
  match is_main_prog with
    | true -> 
        let topcal = parsedToOpt_expr optiprog StringMap.empty oldprog.mpg_topcal in
        let topcap_bind_vars = get_bind_vars_from_topvar topcal in
        {
          optiprog with 
          mopg_topcal = topcal;
          mopg_topcal_bind_vars = topcap_bind_vars;
        }
    | false -> optiprog
    

(*We want to be able to refers every topvar as a int, so we start by resolving
 * them.
 *
 * RAISED ERROR: WHEN several fields would have same name.
 *
 * *)
let parsedToOpt_topval_intmap prog optiprog = 
  let mapping_for_mvars mvarlst = 
    let add_topvar name map = 

      match StringMap.find_opt name map with
        | None -> StringMap.add name (get_fresh_int ()) map
        | Some _ -> raise (VarError ("Cannot redeclare toplevel variable: variable $"^name^" ." ))
    in
    let rec add_topel el map = 
      match el with 
        | MBaseDecl aname -> add_topvar aname.loc_val map
        | MTupDecl nlst -> List.fold_left (fun map el -> add_topel el map) map nlst
    in
    List.fold_left
      (fun map mvar -> add_topel mvar.mva_name.loc_val map) 
      StringMap.empty mvarlst
  in
  let topvar2int = mapping_for_mvars prog.mpg_topvar in 
  let topvar_debug = 
    StringMap.fold
      (fun str_v int_v map -> IntMap.add int_v str_v map )
      topvar2int IntMap.empty
  in
  let optiprog = {optiprog with 
                  mopg_topvar2int = topvar2int}
  in
  let optiprog = {optiprog with 
                  mopg_topvar_debugname = topvar_debug}
  in optiprog
 
(*we here replace every MORef_type to the MOComposed_type they represent.*)
let moref_to_moctype fulloptiprog optiprog = 
  let replace_field fd = 
    let rec ref_to_ctype typ = 
      {typ with loc_val = ref_to_ctype_ typ }
    
    and ref_to_ctype_ typ = 
      match typ.loc_val with
        |  MOUnit_type  
        |  MOAll_type _ 
        |  MOBase_type _ -> typ.loc_val
        |  MOTuple_type lst -> 
           MOTuple_type (List.map ref_to_ctype lst)
        |  MOList_type typ -> 
           MOList_type (ref_to_ctype typ)
        |  MOOption_type typ -> 
           MOOption_type (ref_to_ctype typ)
        |  MOSet_type typ -> 
           MOSet_type (ref_to_ctype typ)
        |  MOMap_type (ktyp,eltyp) -> 
           MOMap_type (ref_to_ctype ktyp, ref_to_ctype eltyp)
        |  MOFun_type (argtyps, rettyp) ->
           MOFun_type (List.map ref_to_ctype argtyps, rettyp)
        |  MORef_type (optmod, id,_,_) -> 
            let ctyp = 
              match optmod with 
                | None -> IntMap.find id optiprog.mopg_types
                | Some modul -> 
                    match IntMap.find modul fulloptiprog.mofp_progmodules with 
                    | MOUserMod oprog -> IntMap.find id oprog.mopg_types
                    | MOSystemMod sysmod -> 
                       GufoModules.sysmodctype_to_ctype (IntMap.find id sysmod.mosm_types)
            in MOComposed_type ctyp

        | MOComposed_type ctyp  -> 
            MOComposed_type 
            { ctyp with 
              moct_fields = 
                IntMap.map (fun fd -> {fd 
                                        with motf_type = ref_to_ctype fd.motf_type;
                                      }
                           ) ctyp.moct_fields;
            }
        | MOTupel_type (md, id, deep, args, pos ) ->
          MOTupel_type (md, id, deep, List.map ref_to_ctype args, pos )
    in
    {fd with motf_type = ref_to_ctype fd.motf_type}
  in
  let new_mopg_types = 
    IntMap.map
    (fun mct -> 
      {mct with moct_fields= (IntMap.map replace_field mct.moct_fields)}
    )
    optiprog.mopg_types
  in {optiprog with mopg_types = new_mopg_types}



  (* END PART 2: TRANSFORMATION OF EXPR *)

  (* PART 1: TRANSFORMATION OF TYPE *)

(*for every prog, it just fill mopg_ctype2int only checking that is has not already been added.*)
let provide_type_id oldprog optiprog = 
  let typestr2int = 
    StringMap.fold 
      (fun strel _ newmap ->
          StringMap.add strel (get_fresh_int ()) newmap 
      ) 
      oldprog.mpg_types StringMap.empty
  in
  {optiprog with mopg_ctype2int = typestr2int}



(*at that point: 
  * fulloptiprog only has mopg_ctype2int set for mainprogs and modules.
 *  oldprog is the program to check in the old format
 *  optiprog is the current prog to check (with only mopg_ctype2int check at start)
 *
 *  the idea here is to fill mopg_types, mopg_field_to_type, mopg_field2int
 * - we check we only use defined type.
 * - we check we have unique fields.
 *  
 * *)
let parsedToOpt_type fulloptiprog oldprog optiprog = 

  let handle_ref_val_from_prog optiprog ref = 
    let typname = List.hd ref.mrv_varname.loc_val in
    let inttyp = StringMap.find_opt typname optiprog.mopg_ctype2int in
    (match inttyp, ref.mrv_varname.loc_val with 
      | Some inttyp, [typname] -> 
          let modul = 
            (match ref.mrv_module with 
                              | None -> None 
                              | Some str -> Some (GufoModules.get_intname_from_modulestr str fulloptiprog)
            )
          in
          MORef_type (modul, inttyp, 0, [])
      | None, [typename] -> raise_typeError ("Type not found: "^ typename) 
                                            ref.mrv_varname.loc_pos
      | _,  lst -> raise_typeError 
                            ("unvalide type declaration"^
                            (List.fold_left (fun str el -> str^" "^el)"" lst)) 
                          ref.mrv_varname.loc_pos
      )
  in

  let rec parsedToOpt_type_simpletype st = 
    let stval = 
      (match st.loc_val with 
      | MBase_type mt -> MOBase_type mt
      | MTuple_type tup ->  MOTuple_type (List.map parsedToOpt_type_simpletype tup)
      | MList_type lst ->  MOList_type (parsedToOpt_type_simpletype lst)
      | MOption_type optt ->  MOOption_type (parsedToOpt_type_simpletype optt)
      | MSet_type settyp ->  MOSet_type (parsedToOpt_type_simpletype settyp )
      | MMap_type (keytyp, eltyp) -> 
           MOMap_type (parsedToOpt_type_simpletype keytyp, parsedToOpt_type_simpletype eltyp )
      | MFun_type (typlst, rettyp) -> 
             MOFun_type(List.map parsedToOpt_type_simpletype typlst, 
                         parsedToOpt_type_simpletype rettyp)
      | MUnit -> MOUnit_type
      | MRef_type ref -> 
          (match ref.mrv_module with 
            | None ->  handle_ref_val_from_prog optiprog ref
            | Some mo -> 
                let modprog = GufoModules.get_module_prog_from_modulestr mo fulloptiprog in
                (match modprog with 
                  | MOUserMod oprog_mod -> handle_ref_val_from_prog oprog_mod ref
                  | MOSystemMod _ -> 
                      let modul, id = GufoModules.get_oref_from_sysmodule ref fulloptiprog
                      in
                      MORef_type (Some modul, id, 0, [])
                )
          )
      | MAll_type v -> raise (InternalError "Gufo internal error") (*TODO*)
    )
    in {st with loc_val = stval}

  in



  let transform_composed_type_field fd = 
    let intname = get_fresh_int () in
    {
      motf_name  = {fd.mtf_name with loc_val = intname};
      motf_type = parsedToOpt_type_simpletype fd.mtf_type;
      motf_debugname = fd.mtf_name.loc_val;
    }
  in

  let check_fields optiprog mc_intname mc = 
       List.fold_left
       (fun (optiprog,map_res) fd -> 
         let ofd = transform_composed_type_field fd in
         (*we need to check there is no other field with the same name*)
         match StringMap.mem fd.mtf_name.loc_val optiprog.mopg_field2int with
          | true ->
            raise_typeError ("the field "^fd.mtf_name.loc_val^" is used several times (possibly in different structs)") fd.mtf_name.loc_pos
          | false  -> 
              (*we add it*)
            let optiprog = {optiprog with mopg_field2int = StringMap.add fd.mtf_name.loc_val ofd.motf_name.loc_val optiprog.mopg_field2int} in
            let optiprog = {optiprog with mopg_field_to_type = IntMap.add ofd.motf_name.loc_val mc_intname optiprog.mopg_field_to_type} in
            (optiprog, (IntMap.add ofd.motf_name.loc_val ofd map_res ))
       )
       (optiprog,IntMap.empty) mc.mct_fields
  in

    StringMap.fold 
      (fun typestr atype optiprog ->  
        match atype with 
          | MComposed_type mc -> 
              let inttype = StringMap.find typestr optiprog.mopg_ctype2int in
              let optiprog,ofds = check_fields optiprog inttype mc in
              let omc = 
                {
                  moct_name = inttype;
                  moct_fields = ofds; 
                  moct_internal_val = List.fold_left (fun map (name, args) -> StringMap.add name args map) StringMap.empty mc.mct_internal_val;
                  moct_debugname = mc.mct_name;
                } in
              {optiprog with mopg_types = IntMap.add inttype omc optiprog.mopg_types}
          | MSimple_type st -> raise (InternalError "Internal error: feature expected for V2. ")


      ) oldprog.mpg_types optiprog



  (* END PART 1: TRANSFORMATION OF TYPE *)



(*
  This is the main analysing function.
  It returns:
    - a full parsed opt prog.
    - a map of type for every variable of every module (type intMap intMap).
*)
let parsedToOpt fullprog = 
  debug_info (debug_title1 "Transforming a full gufo program to a full optimized gufo program.");
  let open Format in
  let fulloptiprog = empty_ofullprog in 
  (*FROM PART 1*)
  debug_info (debug_title2 "Part 1: collecting of types.");
  (*first, we translate the progmap of the full prog*)
  let fulloptiprog = {fulloptiprog with mofp_progmap = fullprog.mfp_progmap} in
  let fulloptiprog = {fulloptiprog with mofp_progmap_debug = fullprog.mfp_progmap_debug} in
  let fulloptiprog = {fulloptiprog with mofp_module_dep= fullprog.mfp_module_dep} in
  (*do the provide_type_id for the main prog*)
  let mainoprog = provide_type_id fullprog.mfp_mainprog empty_oprog in
  let mainoprog = {mainoprog with mopg_name = 0 } in
  let fulloptiprog = {fulloptiprog with mofp_mainprog = mainoprog} in
  (*do the same for modules*)
  let modul_progs = 
    IntMap.mapi
    (fun progi progmod -> 
      match progmod with
        | MUserMod prog -> 
            let oprog = (provide_type_id prog empty_oprog) in
            let oprog = {oprog with mopg_name=progi} in 
              MOUserMod oprog
        | MSystemMod strsysmod-> MOSystemMod (GufoModules.parse_system_module strsysmod)
      ) fullprog.mfp_progmodules 
  in
  let fulloptiprog = {fulloptiprog with mofp_progmodules = modul_progs}  in
  (*fully check type for the main prog *)
  let fulloptiprog = {fulloptiprog with mofp_mainprog = (parsedToOpt_type fulloptiprog fullprog.mfp_mainprog fulloptiprog.mofp_mainprog)} in
  debug_print "main prog collected \n";
  (*do the same for modules  *)
  let modul_progs = 
    IntMap.mapi 
    (fun modi optimod -> 
      let oldprog = IntMap.find modi fullprog.mfp_progmodules in
      match optimod,oldprog with
        | MOUserMod optiprog, MUserMod oldprog -> MOUserMod (parsedToOpt_type fulloptiprog oldprog optiprog)
        | MOSystemMod sysmod, MSystemMod _ -> MOSystemMod sysmod
        | _ -> raise (InternalError "Internal error concerning module system.)")
      ) 
    fulloptiprog.mofp_progmodules 
  in
  debug_print "modules collected \n";
  let fulloptiprog = {fulloptiprog with mofp_progmodules = modul_progs} in
  (*FROM PART 2*)
  (*we want to fill mopg_topvar2int*)
  (*for main prog*)
  debug_info (debug_title2 "part2: converting topvar to optimized representation for main program");
  let fulloptiprog = {fulloptiprog with mofp_mainprog = (parsedToOpt_topval_intmap fullprog.mfp_mainprog fulloptiprog.mofp_mainprog)} in
  (*for the modules*)
  debug_print"converting topvar to optimized representation for modules \n";
  let modul_progs = 
    IntMap.mapi 
    (fun modi optimod -> 
      let oldprog = IntMap.find modi fullprog.mfp_progmodules in
      match optimod,oldprog with
        | MOUserMod optiprog, MUserMod oldprog -> MOUserMod (parsedToOpt_topval_intmap oldprog optiprog)
        | MOSystemMod sysmod, MSystemMod _ -> MOSystemMod sysmod
        | _ -> raise (InternalError "Internal error concerning module system.)")
      ) 
    fulloptiprog.mofp_progmodules 
  in
  let fulloptiprog = {fulloptiprog with mofp_progmodules = modul_progs} in
  (*we want to do the transformation *)
  (*for main module*)
  debug_print "fully converting to optimized representation main program \n";
  let fulloptiprog = {fulloptiprog with mofp_mainprog = 
                      (parsedToOpt_topval fulloptiprog fullprog.mfp_mainprog 
                                          fulloptiprog.mofp_mainprog true None )} 
  in
  (*for modules*)
  debug_print "fully converting to optimized representation modules \n";
  let modul_progs = 
    IntMap.mapi 
    (fun modi optimod -> 
      let oldprog = IntMap.find modi fullprog.mfp_progmodules in
      match optimod,oldprog with
        | MOUserMod optiprog, MUserMod oldprog -> MOUserMod (parsedToOpt_topval fulloptiprog oldprog optiprog false None)
        | MOSystemMod sysmod, MSystemMod _ -> MOSystemMod sysmod
        | _ -> raise (InternalError "Internal error concerning module system.)")
      ) 
    fulloptiprog.mofp_progmodules 
  in
  let fulloptiprog = {fulloptiprog with mofp_progmodules = modul_progs} in
  (*we here replace every MORef_type to the MOComposed_type they represent.*)
  (*for main module*)
  let fulloptiprog = {fulloptiprog with mofp_mainprog = 
                      (moref_to_moctype fulloptiprog fulloptiprog.mofp_mainprog )} 
  in
  (*for modules*)
  let modul_progs = 
    IntMap.mapi 
    (fun modi optimod -> 
      match optimod with
        | MOUserMod optiprog -> MOUserMod (moref_to_moctype fulloptiprog optiprog)
        | MOSystemMod sysmod -> MOSystemMod sysmod
      ) 
    fulloptiprog.mofp_progmodules 
  in
  let fulloptiprog = {fulloptiprog with mofp_progmodules = modul_progs} in
  (*FROM PART 3*)
  (*Trying determine type.
    We first compute a set a constraints.
    Map each program variable to a set of constraint.
  *)
  debug_info (debug_title2 "part3: full type checking");
  let constraints = extract_constraints fulloptiprog in 
  let _debug_constraints = GufoSolver.debug_constraints constraints in 
  let resolvedTypes = GufoSolver.solver constraints in
  let resolvedTypes = 
    IntMap.map
      (fun modMap ->
        IntMap.map 
        (fun loctype -> loctype.loc_val, loctype.loc_pos)
        modMap
      )
      resolvedTypes
  in
  fulloptiprog, resolvedTypes

  (*Second stop, we want to remove references type from the toplevels types.*)
(*   debug_to_level_type fulloptiprog var_types ; *)
  (*PART 4 *)
(*   debug_info (debug_title2 "Part4: full type checking"); *)
(*   let var_types = type_check_bottom_to_top fulloptiprog var_types in *)
(*   fulloptiprog, var_types *)
(*   fulloptiprog, IntMap.empty *)






  (*fulloptiprog is the provious verified oprog.
   *fullprog is the new prog to include in fulloptiprog.
    This function is expected to be called in the context of a console program :
    it means that we make transformation choice considering this context, for
    exemple we remove the topcal of the old mainprog.

   In the code, we will create nfulloptiprog, the new fulloptiprog including fullprog.
   * *)
let add_prog_to_optprog fulloptiprog fullprog = 
  (*For modules there are no merges: if a new module is detected, it is added.
   * Else we keept the old modules.
   *
   * For main prog, we have to carrefully add the new part in the existing
   * program.
   * *)

  (*oprog is the existing mainprog. mprog is the new one.*)
  let add_main_prog_step1 nfulloptiprog oprog mprog = 
    (*first we remove the topcal of oprog.*)
    let oprog = {oprog with 
      mopg_topcal = box_loc (empty_expr);
    }
    in
    (*then we check what we are adding.*)
      match StringMap.is_empty mprog.mpg_types with
        | true -> oprog 
        | false -> 
           parsedToOpt_type nfulloptiprog mprog  
            (provide_type_id mprog oprog)

  in
  let add_main_prog_step2_topval_intmap oprog mprog =
    (*we check if mprog add a new topvar*)
    match List.length mprog.mpg_topvar with
      | 0 -> oprog , None
      | _ -> 
          (*In case of redeclaration:
          * the old var still exist and for exemple if a function use it, it still use the old var.
          * But the old var is no more reachable for new code.
          * exemple:
          * % let $a = 5
          * % let $adda $b = $a + $b
          * % $adda 6
          *   (11)
          * % let $a= 10
          * % $adda 6
          *   (11)
          * % let $adda $b = $a + $b
          * % $adda 6
          *   (16)
          *
          * *)

          let past_map = ref IntMap.empty in
          let add_topvar name map = 

          let fresh_int = get_fresh_int () in
            (match StringMap.find_opt name map with
              | None -> StringMap.add name ((fresh_int)) map
              | Some old_i -> 
                  let ni = fresh_int in
                  past_map := IntMap.add ni old_i (!past_map); 
                  StringMap.add name ni map
            )
          in
          let rec add_topel el map = 
            match el with 
              | MBaseDecl aname -> add_topvar aname.loc_val map
              | MTupDecl nlst -> List.fold_left (fun map el -> add_topel el map) map nlst
          in
          let topvar2int = 
            List.fold_left
                (fun map mvar -> add_topel mvar.mva_name.loc_val map)
              oprog.mopg_topvar2int mprog.mpg_topvar
          in
          (*TODO: this could be optimized: we iterate on every program var*)
          let topvar_debug = 
            StringMap.fold
              (fun str_v int_v map -> IntMap.add int_v str_v map )
              topvar2int oprog.mopg_topvar_debugname
          in
          let oprog = {oprog with 
                          mopg_topvar2int = topvar2int}
          in
          let oprog = {oprog with 
                          mopg_topvar_debugname = topvar_debug}
          in oprog, Some (!past_map)
  in 
  let add_main_prog_step2_intmap nfulloptiprog oprog mprog past_var_map =
    parsedToOpt_topval nfulloptiprog mprog oprog true past_var_map
  in
  let add_main_prog_step2_moref_to_moctype nfulloptiprog oprog = 

    moref_to_moctype nfulloptiprog oprog
  in

  (*This table translate a module id from fullprog to a module id in fulloptiprog.*)
  (*in fact we don't need it.*)
(*   let translatation_map_module_id = IntMap.empty in *)

  let apply_for_modules apply_fun acc = 
  match StringMap.cardinal fullprog.mfp_progmap with 
    | 0 -> raise (InternalError "Internal error.")
    | 1 -> (*only mainprog*) 
        acc
    | _ -> (**mainprog and other programs*)
        StringMap.fold 
          (fun strmod imod acc -> 
            (*we ignore 0 (mainprog) *)
            if imod = 0 then acc
            else 
              (*we have to check if the module is already in fulloptprog. *)
              (match StringMap.mem strmod fulloptiprog.mofp_progmap with
                | true -> acc
                | false -> apply_fun acc imod strmod
              )
               
          )
          fullprog.mfp_progmap
          acc
  in

  let construct_step1 nfulloptiprog progi mprog = 
    let mainoprog = provide_type_id mprog empty_oprog in
    let mainoprog = {mainoprog with mopg_name = progi } in
    (parsedToOpt_type nfulloptiprog mprog mainoprog) 
  in

  let parse_new_progmodules nfulloptiprog modi progmodule =
      match progmodule with
        | MUserMod mprog -> MOUserMod (construct_step1 nfulloptiprog modi mprog )
        | MSystemMod str -> MOSystemMod (GufoModules.parse_system_module str)
    (*TODO*)
  in 
  (*nfulloptiprog with fields mofp_progmap, mofp_progmap_debug, mofp_module_dep,
   * mofp_progmodules merged with fullprog.*)
  let step_1_mod_fun nfulloptiprog imod strmod = 
    match StringMap.mem strmod nfulloptiprog.mofp_progmap with 
      | true -> nfulloptiprog
      | false -> (*we need to import it*)
          let nfulloptiprog = 
            {nfulloptiprog with 
              mofp_progmap = StringMap.add strmod imod
                                           nfulloptiprog.mofp_progmap;
              mofp_progmap_debug = IntMap.add imod
                                              strmod nfulloptiprog.mofp_progmap_debug;
              mofp_module_dep = IntMap.add imod
                                          (IntMap.find imod fullprog.mfp_module_dep) 
                                          nfulloptiprog.mofp_module_dep;
              mofp_progmodules = IntMap.add imod
                                            (parse_new_progmodules nfulloptiprog 
                                            imod
                                            (IntMap.find imod fullprog.mfp_progmodules)) 
                                            nfulloptiprog.mofp_progmodules;
            }
          in
          nfulloptiprog

  in
  debug_info(debug_title2 "PART 1");
  let nfulloptiprog = 
    apply_for_modules step_1_mod_fun fulloptiprog
  in

  (*for mainprog*)
  let nfulloptiprog = 
    {nfulloptiprog with 
      mofp_mainprog = add_main_prog_step1 nfulloptiprog nfulloptiprog.mofp_mainprog fullprog.mfp_mainprog
    }
  in
  (*PART 2 *)
  debug_info(debug_title2 "ParsedToOpt PART 2");
  (*for topval*)
  let step2_topval_intmap nfulloptiprog imod strmod = 
    let momod = IntMap.find imod nfulloptiprog.mofp_progmodules in
    let mmod = IntMap.find imod fullprog.mfp_progmodules in
    match mmod, momod with
      | MUserMod mprog, MOUserMod mainoprog -> 
        {nfulloptiprog with 
          mofp_progmodules = IntMap.add imod (MOUserMod (parsedToOpt_topval_intmap mprog mainoprog))
                              nfulloptiprog.mofp_progmodules
        }
      | _ -> nfulloptiprog
  in
  let nfulloptiprog = 
    apply_for_modules step2_topval_intmap nfulloptiprog 
  in
  (*do the same for mainprog*)
  let main_prog, past_var_map = add_main_prog_step2_topval_intmap nfulloptiprog.mofp_mainprog fullprog.mfp_mainprog 
  in
  let nfulloptiprog = 
    {nfulloptiprog with 
      mofp_mainprog =  main_prog
                        
    }
  in
  let step2_gen nfulloptiprog imod strmod = 
    let momod = IntMap.find imod nfulloptiprog.mofp_progmodules in
    let mmod = IntMap.find imod fullprog.mfp_progmodules in
    match mmod, momod with
      | MUserMod mprog, MOUserMod mainoprog -> 
        {nfulloptiprog with 
          mofp_progmodules = IntMap.add imod (MOUserMod(parsedToOpt_topval nfulloptiprog mprog mainoprog true None)) 
                              nfulloptiprog.mofp_progmodules
        }
      | _ -> nfulloptiprog
  in
  let nfulloptiprog = 
    apply_for_modules step2_gen nfulloptiprog 
  in
  (*for mainprog*)
  let nfulloptiprog = 
    {nfulloptiprog with 
      mofp_mainprog = add_main_prog_step2_intmap nfulloptiprog
                        nfulloptiprog.mofp_mainprog fullprog.mfp_mainprog past_var_map
    }
  in
  let step2_moref_to_moctype nfulloptiprog imod strmod = 
    let momod = IntMap.find imod nfulloptiprog.mofp_progmodules in
    let mmod = IntMap.find imod fullprog.mfp_progmodules in
    match mmod, momod with
      | MUserMod _mprog, MOUserMod mainoprog -> 
        {nfulloptiprog with 
          mofp_progmodules = IntMap.add imod (MOUserMod(moref_to_moctype nfulloptiprog mainoprog )) nfulloptiprog.mofp_progmodules
        }
      | _ -> nfulloptiprog
  in
  let nfulloptiprog = 
    apply_for_modules step2_moref_to_moctype nfulloptiprog 
  in
  (*for mainprog*)
  let nfulloptiprog = 
    {nfulloptiprog with 
      mofp_mainprog = add_main_prog_step2_moref_to_moctype nfulloptiprog nfulloptiprog.mofp_mainprog
    }
  in
  dump_prog_representation nfulloptiprog.mofp_mainprog;
  (*PART 3 *)
  (* var_types contains every type of every defined var, not only toplevel.
     Currently var_types is a IdModule map --> (Id varable map --> type).
     Maybe it should become a simple Id variable map --> type shared between
     modules with variable id unique within the whole program. This would make
     it easier to get type from this map.*)
  debug_info(debug_title2 "PART 3");
  let constraints = extract_constraints nfulloptiprog in 
  debug_info(debug_title2 "extracting constraint");
  let _debug_constraints = GufoSolver.debug_constraints constraints in 
  debug_info(debug_title2 "solving constraint");
  let resolvedTypes = GufoSolver.solver constraints in
  let resolvedTypes = 
    IntMap.map
      (fun modMap ->
        IntMap.map 
        (fun loctype -> 
            loctype.loc_val, loctype.loc_pos)
        modMap
      )
      resolvedTypes
  in
  nfulloptiprog, resolvedTypes
(*   let var_types = top_level_types nfulloptiprog in  *)
  (*PART 3 *)
(*
  debug_print "dump var_types \n";
  IntMap.iter
    (fun ivar (typ, _pars_pos) -> debug_print (sprintf "\t\t %d : %s\n" ivar (type_to_string typ)))
    (IntMap.find 0 var_types);


  (*PART 3 *)
   debug_to_level_type nfulloptiprog var_types ;
  (*PART 4 *)
  debug_info(debug_title2 "PART 4");
  debug_print "dumping var_types at beginning of part 4";
  debug_var_type var_types;
  let var_types = type_check_bottom_to_top nfulloptiprog var_types in
  nfulloptiprog, var_types
*)

  
let add_module_to_optprog progname fulloptiprog moduleprog = 
  (*this function allows the added module to take an unused key.*)
  let get_module_key map = 
    let rec find_valid i =
      match IntMap.find_opt i map with
        | None -> i 
        | Some _ -> find_valid (i + 1)
    in
    find_valid (IntMap.cardinal map)
  in
  let module_key = get_module_key fulloptiprog.mofp_progmodules in

  let progmap = fulloptiprog.mofp_progmap in 
  let progmap_debug = fulloptiprog.mofp_progmap_debug in 
  let fulloptiprog = {fulloptiprog with 
      mofp_progmap = StringMap.add progname module_key progmap ;
      mofp_progmap_debug = IntMap.add module_key progname progmap_debug;
    }
  in

  (*FROM PART 1*)
  (*do the provide_type_id for the moduleprog*)
  debug_info(debug_title2 "PART 1");
  let omoduleprog = (provide_type_id moduleprog empty_oprog) in
  let omoduleprog = {omoduleprog with mopg_name=module_key} in 
  (*fully check type for the prog *)
  let omoduleprog = parsedToOpt_type fulloptiprog moduleprog omoduleprog in
  let modul_progs = IntMap.add module_key (MOUserMod omoduleprog)  fulloptiprog.mofp_progmodules in
  let fulloptiprog = {fulloptiprog with mofp_progmodules = modul_progs}  in
  (*END PART 1*)
  (*FROM PART 2*)
  debug_info(debug_title2 "PART 2");
  let omoduleprog = parsedToOpt_topval_intmap moduleprog omoduleprog in
  let omoduleprog = parsedToOpt_topval fulloptiprog moduleprog omoduleprog false None in
  let omoduleprog = moref_to_moctype fulloptiprog omoduleprog in
  (*END PART 2*)
  let modul_progs = IntMap.add module_key (MOUserMod omoduleprog) fulloptiprog.mofp_progmodules in
  let fulloptiprog = {fulloptiprog with mofp_progmodules = modul_progs} in
  (*FROM PART 3*)
  debug_info(debug_title2 "PART 3");
  let constraints = extract_constraints fulloptiprog in 
  debug_info(debug_title2 "extracting constraint 3-1");
  let _debug_constraints = GufoSolver.debug_constraints in 
  let resolvedTypes = GufoSolver.solver constraints in
(*   nfulloptiprog, resolvedTypes *)
(*
  let var_types = top_level_types fulloptiprog in 
  debug_to_level_type fulloptiprog var_types ;
  (*END PART 3*)
  (*PART 4 *)
  debug_info(debug_title2 "PART 4");
  let var_types = type_check_bottom_to_top fulloptiprog var_types in
  debug_print (sprintf "Dumping fulloptiprog modules after adding modules: %s \n"
          (fulloptiprogModules_to_string fulloptiprog));

*)
  let resolvedTypes = 
    IntMap.mapi
      (fun modi modMap ->
        IntMap.mapi
        (fun vari loctype -> 
          Printf.printf "final map %d %d\n" modi vari;
          loctype.loc_val, loctype.loc_pos)
        modMap
      )
      resolvedTypes
  in
  fulloptiprog, resolvedTypes

