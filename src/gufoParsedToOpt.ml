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


let debug_var_type var_types = 
  debug_print "dumping var_types" ; 
  IntMap.iter
    (fun i vmap -> 
      debug_print (sprintf "Module %d \n" i); 
      IntMap.iter
        (fun iv typ -> 
          debug_print (sprintf "var %d : %s \n" iv (type_to_string typ)); 
        )
        vmap
    )
    var_types

(*POSITION HANDLING *)
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
 
(*END POSITION HANDLING *)

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
        (debug_print (sprintf "%s \n"(moval_to_string optiprog.mopg_topcal)))
 




(** transformation from gufoParsed to gufo.core **)

let fresh_int = ref 1
let get_fresh_int () = 
  fresh_int:= (!fresh_int + 1); !fresh_int

(*First part of the job will be to get every modules that we have to import. *)

let search_modules mprogram =
  let add_module_from_mref module2int mref =
    let add_if_not_already modul map = 
      match StringMap.mem modul map with
        | true -> map
        | false -> StringMap.add modul (get_fresh_int ()) map

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
      mref.mrv_varname
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
                      module2int mvar.mva_value)
                    module2int mprogram.mpg_topvar
  in
  (*then in toplevel*)
  let module2int = fold_over_mref_val add_module_from_mref module2int mprogram.mpg_topcal
  in module2int

  (* PART 3: FULL TYPE CHECKING *)
  (* We will have 3 parts in the type checking part:
    * - determine_type_* : is the main folding in the whole code, it try to
    *    dive in the code to give type to the result of topval. From the value
    *    it determines a type.
    * - constraint_type: is called by determine_type after the dive occured, it
    *   worries about the operation in the other direction: from a type, we
    *   precise the element of the expression. 
    * - refine_type_* is about precising type from 2 expression which should
    * match the same type.
    *
    * *)



(*fonction retrieving the type of the element of a tuple according to its position.
 * tupl_typ is the general type of the tuple while position is the position of the element.*)
let rec get_type_from_tuple_el tupl_typ position = 
  match position with 
    | [] -> raise (InternalError "Gufo internal error.")
    | [last_pos] -> 
        (match tupl_typ with
          | MOTuple_type subtyplst -> 
              (try List.nth subtyplst last_pos
              with _ -> raise (TypeError "The tuple value cannot be assigned to the assigned variable tuples."))
          | MORef_type (modul, iref, idx, args) -> 
              (MOTupel_type (modul, iref, idx, args, position) )
          | _ -> raise (InternalError (sprintf "type checker error.\n" ))
        )
    | i::lst -> 
        (match tupl_typ with
          | MOTuple_type subtuple -> 
              get_type_from_tuple_el (List.nth subtuple i) lst
          | MORef_type (modul, iref, idx, args)-> 
              (MOTupel_type (modul, iref, idx, args, position))
          | _ -> raise (InternalError "type checker error.")
        )

let determine_refine_type_composed i j = 
  match i.moct_name - j.moct_name with
    | 0 -> i
    | _ -> raise (TypeError (sprintf "Cannot merge type %d and %d together (TODO: give source name) (heritage not implemented))" i.moct_name j.moct_name))

let determine_refine_base_type i j = 
  match i, j with 
    | MTypeString, MTypeString -> MTypeString
    | MTypeBool, MTypeBool -> MTypeBool
    | MTypeInt, MTypeInt -> MTypeInt
    | MTypeFloat, MTypeFloat -> MTypeFloat
    | MTypeCmd, MTypeCmd -> MTypeCmd
    | _, _ -> raise (TypeError (sprintf "Cannot merge together type %s and type %s ." (basetype_to_str i) (basetype_to_str j)))

(*
* alltypeMap give for a given 'alltype' id a list of constraints to which it is
linked.  * for exemple '63 can be constrained to be an integer, or can be
constrained * to another  'alltype'.
*)
(* let alltypeMap = ref IntMap.empty *)

(*Try to refines the expr into a reduced set of compatible types. 
  There is a complex issue when both e1 and e2 are MOAll_type with i and i'.
  if you have a function f : set('70) -> '70 -> bool, this is not the same
  typing as a function g : set('80') -> '81 -> bool.

  That is why when using this function, we should consider e1 as the most
constrained type: if both are MOAll_type, the MOAll_type val of e1 will be
taken.
*)
let rec determine_refine_type ?id_var:(id_var = None) t1 t2 = 
  match t1, t2 with
    | MOUnit_type, MOUnit_type -> 
        MOUnit_type
    | MOUnit_type, _ -> 
        raise (TypeError "TypeError, cannot refine unit with other type")
    | t1, MOAll_type i 
    | MOAll_type i , t1 -> 
(*
        (let currentIConstraints = 
          match (IntMap.find_opt i (!alltypeMap)) with
            | None -> []
            | Some lst -> lst
         in
         alltypeMap:= IntMap.add i (t1::currentIConstraints) (!alltypeMap)
        );
*)
        t1
    | MOComposed_type i , MOComposed_type j -> 
        MOComposed_type (determine_refine_type_composed i j )
    | MOBase_type i , MOBase_type j  -> 
        MOBase_type (determine_refine_base_type i j )
    | MOTuple_type ilst , MOTuple_type jlst  ->
        (match (List.length ilst - List.length jlst ) with 
          | 0 -> MOTuple_type (List.map2 (determine_refine_type ~id_var:None) ilst jlst )
          | _ -> raise (TypeError "Cannot merge together tuple types of different lengths. ")
        )
    | MOList_type i , MOList_type j  -> 
        MOList_type (determine_refine_type i j)
    | MOOption_type i, MOOption_type j -> 
        MOOption_type (determine_refine_type  i j)
    | MOSet_type i, MOSet_type j ->
        MOSet_type (determine_refine_type  i j)
    | MOMap_type (k,v), MOMap_type(kp,vp) ->
        MOMap_type (determine_refine_type k kp, determine_refine_type v vp)
    (*TODO: for now we consider no ref type here, it should be handled elsewhere.*)
    | MOComposed_type _ , _ -> 
        raise (TypeError "Cannot refine composed type to other type")
    | MORef_type (imod, i, deep,args ), MORef_type(imod2, i2, deep2, args2) -> 
        (*we do the match to avoid a typ beiing a ref to itself. 
         *
         * Sometimes, while we have just usage without any definition, we have
         * a ref on ourself but at the moment we get the definition, we improve
         * and in particular, if the definition is on another ref, we want to
         * replace the type by this another ref.
         * *)
        (match id_var with
          | Some (ni) when ni = i -> MORef_type(imod2, i2, deep2, args2)
          | _ -> MORef_type (imod, i, deep, args)
        )
      (*TODO: eventually this might
    be improvable to keep link that both ref should have same type.*)
    | MORef_type _, typB -> typB
    | typA, MORef_type _ -> typA
    | MOFun_type(aargs, aret), MOFun_type(bargs, bret) -> 
        let lst_args = List.map2 determine_refine_type  aargs bargs in
        let ret = determine_refine_type aret bret in
        MOFun_type (lst_args, ret)
    | _,_ -> raise (TypeError 
              (sprintf "Cannot infer correct type between %s and %s " (type_to_string t1) (type_to_string t2)))
    
(*Determining the type of every top level function *)
    (*Caution, we try here to deduce toplevel type, not to check low level
     * coherency. We don't check here validity of element of MTypeCmdVal*)

let rec add_in_scope ref_vname typ locScope = 
  (*fds is the eventual field, if there are fields, we don't add to locScope*)
  let id_ref,fds = ref_vname in 
  match fds, IntMap.find_opt id_ref locScope with
    | [], None -> IntMap.add id_ref typ locScope
    | [], Some atyp -> 
        IntMap.add id_ref (determine_refine_type ~id_var:(Some id_ref) typ atyp) locScope
    | _,_ -> locScope

and determine_type_cmdseq fulloptiprog optiprog locScope cmdseq = 
  let determine_type_stringOrRef locScope sor =
    match sor with
      | MOSORString _-> locScope
      | MOSORExpr e -> 
          let const =  Some (MOBase_type MTypeString) in
          let _typ, locScope = determine_type fulloptiprog optiprog locScope const e in
          locScope
  in

  let determine_type_cmd locScope cmd =
    (*we have to check the arguments and the input/output*)
    let locScope = List.fold_left (fun locScope arg -> determine_type_stringOrRef locScope arg ) locScope cmd.mocm_args in
    let locScope = 
      match cmd.mocm_output with
        | MOCMDOStdOut
        | MOCMDOStdErr -> locScope
        | MOCMDOFile sor 
        | MOCMDOFileAppend sor -> determine_type_stringOrRef locScope sor
    in
    let locScope = 
      match cmd.mocm_outputerr with
        | MOCMDEStdOut
        | MOCMDEStdErr -> locScope
        | MOCMDEFile sor 
        | MOCMDEFileAppend sor -> determine_type_stringOrRef locScope sor
    in
    let locScope = 
      match cmd.mocm_input_src with
        | MOCMDIStdIn -> locScope
        | MOCMDIFile sor -> determine_type_stringOrRef locScope sor
    in
    MTypeCmd, locScope
  in
  match cmdseq with 
    | MOSimpleCmd cmd -> determine_type_cmd locScope cmd
    | MOForkedCmd cmdseq -> determine_type_cmdseq fulloptiprog optiprog locScope cmdseq
    | MOAndCmd (cmdseqa, cmdseqb) 
    | MOOrCmd (cmdseqa, cmdseqb) 
    | MOSequenceCmd (cmdseqa, cmdseqb) 
    | MOPipedCmd (cmdseqa, cmdseqb) ->
        let _typ, locScope = 
          determine_type_cmdseq fulloptiprog optiprog locScope cmdseqa
        in
        determine_type_cmdseq fulloptiprog optiprog locScope cmdseqb

and determine_type_base_val fulloptiprog optiprog locScope bv = 
  let typ, locScope = 
    match bv with
      | MOTypeStringVal s -> MTypeString, locScope
      | MOTypeBoolVal b -> MTypeBool, locScope
      | MOTypeIntVal i -> MTypeInt, locScope 
      | MOTypeFloatVal f-> MTypeFloat, locScope
      | MOTypeCmdVal cseq -> determine_type_cmdseq fulloptiprog optiprog locScope cseq
  in 
  MOBase_type typ, locScope




and determine_type_composed fulloptiprog optiprog mct = 
  let modul, id = mct.mocv_resolved_type in 
  match modul with
    | None -> IntMap.find id optiprog.mopg_types
    | Some i -> 
        match IntMap.find i fulloptiprog.mofp_progmodules with
          | MOUserMod prog -> IntMap.find id prog.mopg_types
          | MOSystemMod sysmod ->  GufoModules.sysmodctype_to_ctype 
                                    (IntMap.find i sysmod.mosm_types)

and determine_type_comp fulloptiprog optiprog locScope const op arga argb =   
  let () = 
  (match const with 
     | None 
     | Some (MOAll_type _) 
     | Some  (MOBase_type (MTypeBool)) -> ()
     | Some const ->  raise (TypeError (sprintf "Comparison return a boolean type while a type %s is expected. \n" (type_to_string const)) )
  )
  in
    (*next two line are for locScope precision*)
(*
  let const = 
    match op with 
      | Egal 
      | NotEqual -> Some (MOAll_type (get_fresh_int())) ;
      | LessThan  
      | LessOrEq  
      | GreaterThan 
      | GreaterOrEq -> Some (MOOr_type([
        MOBase_type(MTypeInt);
        MOBase_type(MTypeFloat);
          ], get_fresh_int()))
  in 
*)
    let typa, locScope = determine_type fulloptiprog optiprog locScope None arga in
    let typb, locScope = determine_type fulloptiprog optiprog locScope None argb in
    let _typ = determine_refine_type typa typb in
      MOBase_type (MTypeBool), locScope
    
and determine_type_basic_fun fulloptiprog optiprog locScope const op arga argb = 
  let for_basic_op expectedType arga argb =
        let typa, locScope = 
          determine_type fulloptiprog optiprog locScope expectedType arga in
        let typb, locScope = 
          determine_type fulloptiprog optiprog locScope expectedType argb in
        determine_refine_type typa typb, locScope
  in
  let for_dict_op expectedType arga argb = 
    let typ_a, locScope = determine_type fulloptiprog optiprog locScope const arga in 
    let typ_b, locScope = determine_type fulloptiprog optiprog locScope const argb in
    (determine_refine_type (determine_refine_type expectedType typ_a) 
                           (determine_refine_type expectedType typ_b)) , locScope

  in
  let typ,locScope = 
    match op with 
    | MConcatenation ->
        let expectedType = Some (MOBase_type MTypeString) in
        for_basic_op expectedType arga argb
    | MAddition 
    | MMultiplication 
    | MModulo 
    | MDivision 
    | MSoustraction -> 
        let expectedType = Some (MOBase_type MTypeInt) in
        for_basic_op expectedType arga argb
    | MAdditionFloat 
    | MMultiplicationFLoat
    | MModuloFloat
    | MDivisionFloat
    | MSoustractionFloat -> 
        let expectedType = Some (MOBase_type MTypeFloat) in
        for_basic_op expectedType arga argb
    | MWithList -> 
        let full_or_type = MOList_type (MOAll_type (get_fresh_int ())) in
        for_dict_op full_or_type arga argb
    | MWithSet -> 
        let full_or_type = MOSet_type (MOAll_type (get_fresh_int ())) in
        for_dict_op full_or_type arga argb
    | MWithMap -> 
        let full_or_type = MOMap_type (MOAll_type (get_fresh_int ()), MOAll_type (get_fresh_int ())) in
        for_dict_op full_or_type arga argb
    | MWithoutSet -> 
        let full_or_type = MOSet_type (MOAll_type (get_fresh_int ())) in
        for_dict_op full_or_type arga argb
    | MWithoutMap -> 
        let full_or_type = MOMap_type (MOAll_type (get_fresh_int ()), MOAll_type (get_fresh_int ())) in
        for_dict_op full_or_type arga argb
    | MHasSet -> 
        let full_or_type = MOSet_type (MOAll_type (get_fresh_int ())) in
        let typ_a, locScope = determine_type fulloptiprog optiprog locScope (Some full_or_type) arga in 
        (match typ_a with 
          | MOSet_type keytype ->
            let typ_b, locScope = determine_type fulloptiprog optiprog locScope (Some keytype) argb in
            MOBase_type(MTypeBool), locScope
          | _ -> raise (InternalError "Gufo internal error.") (*Cannot append *)
        )
    | MHasMap -> 
        let full_or_type = MOMap_type (MOAll_type (get_fresh_int ()), MOAll_type (get_fresh_int ())) in
        let typ_a, locScope = determine_type fulloptiprog optiprog locScope (Some full_or_type) arga in 
        (match typ_a with 
          | MOMap_type (keytype, _vtype) ->
            let typ_b, locScope = determine_type fulloptiprog optiprog locScope (Some keytype) argb in
            MOBase_type(MTypeBool), locScope
          | _ -> raise (InternalError "Gufo internal error.") (*Cannot append *)
        )
  in 
  typ, locScope


and determine_type_binding fulloptiprog optiprog locScope const bd = 
  let determine_in_known_tuple tupl_name tupl_typ locScope = 
    (*we have to check compatibilty between tupl_name and tupl_typ before putting link in locScope *)
          List.fold_left 
          (fun locScope (id_el, position) -> 
            let curel_typ = get_type_from_tuple_el tupl_typ position in
            IntMap.add id_el curel_typ locScope
          ) locScope tupl_name
  in
  let db_typ, locScope = determine_type fulloptiprog optiprog locScope None bd.mobd_value in
  let locScope = 
    match bd.mobd_name, db_typ with 
      | [(single_id,[])], _ -> IntMap.add single_id db_typ locScope 
      | tupl_name, MOTuple_type tupl_typ ->
          determine_in_known_tuple tupl_name db_typ locScope
      | tupl_name, MORef_type (modul, iref, deep, args) ->
          List.fold_left 
            (fun locScope (id, position) -> IntMap.add id (MOTupel_type(modul, iref, deep,args, position)) locScope) 
            locScope tupl_name
      | tupl_name, otherType -> raise (InternalError "Type checker error")
  in
  let body_type, locScope = determine_type fulloptiprog optiprog locScope const bd.mobd_body in
  let value_const =
    (match bd.mobd_name with 
      | [(single_id,[])] -> IntMap.find single_id locScope 
      | tupl_name ->
          let lsttyp = 
          List.map (fun (id, position) -> IntMap.find id locScope) tupl_name
          in MOTuple_type (lsttyp)
    )
  in
  let _db_val, locScope = determine_type fulloptiprog optiprog locScope (Some value_const) bd.mobd_value in
  body_type, locScope

(**Determine typ of a reference acting as a function call (with arguments). 
*)
and determine_type_ref_function_call fulloptiprog optiprog const locScope 
                                     ref args typ_ref =
  (*args_const is the list of the constrained argument.
    ret_const is the constrained return type.*)
  (let args_const, ret_const = 
    (match const with 
      | None 
      | Some (MOAll_type _) -> 
          List.map (fun _ -> None ) args , None
      | Some (MOFun_type (typargs, argret)) ->
          List.map (fun typ -> Some typ) typargs, Some argret
      | _ -> List.map (fun _ -> None) args, None (*TODO:maybe to easy*)
    )
  in

  (*lst_typ_args is the refination of the args with the constrained args.*)
  let lst_typ_args, locScope, bd_alltype = 
    List.fold_left2 
    (fun (lst_typ_args,locScope, bd_alltype) arg const -> 
      let typ, locScope = 
        determine_type fulloptiprog optiprog locScope const arg 
      in

      let typ, bd_alltype = 
        match const with 
        | Some (MOAll_type i ) ->
          refine_bd_alltype i typ bd_alltype 
        | _ -> typ, bd_alltype
      in
      typ::lst_typ_args, locScope, bd_alltype
    ) ([], locScope, IntMap.empty) args args_const
  in
  let lst_typ_args = List.rev lst_typ_args in
  let typ_ref = precise_with_args typ_ref lst_typ_args in

  let expr_typ = determine_ref_with_index ref typ_ref in 
  (*update the type of typ_ref using the information from the argument*)
  let expr_typ = determine_refapplication_type expr_typ lst_typ_args bd_alltype in
  
  debug_print (sprintf "determine_type_ref_function_call ending with type : '%s' \n"  (type_to_string expr_typ));

  let locScope = add_in_scope ref.morv_varname typ_ref locScope in
  expr_typ, locScope
  )

(*we don't do intermodule type checking for now, so we only check ref
 * defined in this module.*)
and determine_ref_with_index ref ref_vtyp  = 
  (match ref.morv_index with
    | None -> ref_vtyp
    | Some idx -> 
        let idx_deep = List.length idx in 
        let ref_name = ref.morv_debugname in
        let rec determine_ref_with_index_ idx_deep ref_vtyp = 
          match idx_deep with
            | 0 -> raise (VarError (sprintf "You are trying to access to the index of a variable (%s) which is not a map or a list." ref_name))
            | 1 -> 
              (match ref_vtyp with 
                | MOList_type subtyp -> subtyp
                | MOSet_type( subtyp) ->  subtyp 
                | MOMap_type (_, subtyp) -> subtyp
                | MOAll_type i -> MOAll_type (get_fresh_int ())
                | _ -> raise (InternalError 
                        "Gufo internal error: no deep for the given type.") 
                        (*we don't have deep for other types.*)
              )
            | i -> 
              (match ref_vtyp with 
                | MOList_type subtyp -> determine_ref_with_index_ (i-1) subtyp 
                | MOSet_type( subtyp) -> determine_ref_with_index_ (i-1) subtyp 
                | MOMap_type (_, subtyp) -> determine_ref_with_index_ (i-1) subtyp 
                | MOAll_type i -> MOAll_type (get_fresh_int ())
                | _ -> raise (InternalError 
                        "Gufo internal error: no deep for the given type.") 
                        (*we don't have deep for other types.*)
              )
        in
        determine_ref_with_index_ idx_deep ref_vtyp
    )

(*we only check the ref here, without worying about possible args.*)
and determine_type_ref fulloptiprog optiprog locScope ref = 

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
             (match IntMap.find_opt varname locScope with
               | None -> MORef_type (None, varname, 0, [])
               | Some v -> v
             )
          | Some i -> 
(*                    MOAll_type (get_fresh_int()) *)
                   (match ref.morv_index with
                     | None -> MORef_type (Some i, varname, 0, [] )
                     | Some idx -> MORef_type (Some i, varname, List.length idx, [] )
                   )
        )
      | lst_fields -> 
          determine_with_fields lst_fields ref.morv_index
  in 
(*
  let varname_typ = 
    match fields with 
      | [] -> typ 
      | firstfd::lst_fields -> 
          get_ownertype_from_field fulloptiprog optiprog firstfd
  in
   typ, add_in_scope varname varname_typ locScope  *)
   typ, locScope 

and precise_with_args ref_typ args_typ =
  match args_typ with
    | [] -> ref_typ
    | typ :: oargs_typ -> 
        (match ref_typ with 
          | MORef_type (_,_,_,_) -> 
              ref_typ
              (*TODO: there might be a better solution to find.*)
          | MOAll_type i ->
              (MOFun_type (typ::oargs_typ, MOAll_type i)) 
          | _ -> 
              ref_typ
        )
 
(*bd all type is a map containing the reduction of an 'alltype' into a more
precise type.*)
and refine_bd_alltype i typ bd_alltype = 
  let cur_typ = IntMap.find_opt i bd_alltype in
  match cur_typ with
    | None -> 
        typ , IntMap.add i typ bd_alltype
    | Some cur_typ ->
        let typ = (determine_refine_type cur_typ typ) in
        typ , IntMap.add i typ bd_alltype

(*
 * 
*)
and determine_refapplication_type ref_typ args_typ bd_alltype = 
  let rec apply ref_typ args_typ = 
    match args_typ with
      | [] -> ref_typ
      | typ :: args_typ -> 
          (match ref_typ with 
            | MOFun_type ([_refarg], refret) ->
                apply refret args_typ
              | MOFun_type (refarg:: refargs , refret) ->
                  apply (MOFun_type (refargs, refret)) args_typ
            | MORef_type (modi,i,deep,args) -> 
                apply (MORef_type (modi,i,deep,(List.rev (typ::(List.rev args))))) args_typ
                (*TODO: there might be a better solution to find.*)
            | MOAll_type i ->
                let typ, _ = refine_bd_alltype i ref_typ bd_alltype in typ 
(*                 let typ, _ = refine_bd_alltype i ref_typ bd_alltype in typ  *)
            | MOList_type st -> 
               MOList_type st 
            | _ -> 
                (*If we are reaching this part, it means that _ref_typ has been
resolved to something else than a function or something accepting argument, but still there are new arguments provided ..*)
                raise (TypeError "Too many arguments given.")
          )
  in
  match args_typ with
    | [] -> ref_typ
    | args_typ -> apply ref_typ  args_typ
        


(* 
 *
 * This function is the central type checking fonction.
 * It tries te determine a type for a expr e.
 *
 * This function will be run several time for the same expression depending on
 * the state of the analyse: it will be first used to retrieve a simple type
 * and later will be used against with an eventual constraint allowing to
 * refine the analyse.
 *
 * fulloptiprog is the representation of the full programs in optimised representation
 * optiprog is the current module in optimised representation
 * locScope is the type local scope (for a variable, give its type)
 * to what concrete type it is currently linked.
 *
 * const is an optional type contraint that must respect the value e.
 * e is the expression value we want to determine the type.
 * *)
and determine_type fulloptiprog optiprog locScope const e =
  let check_respect_constraint const deduced_type = 
    match const, deduced_type with
      | None, _ -> 
          deduced_type
      | Some (MOAll_type i), MOAll_type a -> 
          MOAll_type i
      | Some typ, deduced_type -> 
          determine_refine_type typ deduced_type
  in
  let rec add_args_to_scope argtyp_const args locScope = 
    match argtyp_const with 
      | None -> 
        List.fold_left
        (fun (locScope,newlst) arg -> 
          match arg with
            | MOBaseArg i -> 
                let ntype = (MOAll_type (get_fresh_int ()))
                in
                (IntMap.add i ntype locScope, ntype::newlst)
            | MOTupleArg args -> 
                let nlocScope, nlst = add_args_to_scope None args locScope in
                  (nlocScope, (MOTuple_type nlst)::newlst)
        )
        (locScope,[]) args
      | Some lstargtyp_const ->
        List.fold_left2
        (fun (locScope,newlst) arg arg_const -> 
          match arg with
            | MOBaseArg i -> 
                (IntMap.add i arg_const locScope, arg_const::newlst)
            | MOTupleArg args -> 
                (match arg_const with
                  | MOAll_type i ->
                      let nlocScope, nlst = add_args_to_scope None args locScope in
                      (nlocScope, (MOTuple_type nlst)::newlst)
                  | MOTuple_type lsttyp_const -> 
                      let nlocScope, nlst = add_args_to_scope (Some lsttyp_const) args locScope in
                      (nlocScope, (MOTuple_type nlst)::newlst)
                  | _ -> raise (TypeError "Impossible to infer type")
                )
        )
        (locScope,[]) args lstargtyp_const
  in
  let rec refine_args_from_scope locScope args =  
    List.map
      (fun arg -> 
        (match arg with
          | MOBaseArg i -> IntMap.find i locScope
          | MOTupleArg aargs -> 
              let nargs = refine_args_from_scope locScope aargs in
              MOTuple_type nargs
        )
      )
      args
  in

  let typ_res, locScope = 
    (match e with
      | MOComposed_val mct -> 
          MOComposed_type (determine_type_composed fulloptiprog optiprog mct),
          locScope
      | MOSimple_val st -> 
          (match st with 
            | MOBase_val bv -> 
                determine_type_base_val fulloptiprog optiprog locScope bv
            | MOTuple_val tv -> 
                let new_const = 
                  match const with
                    | None -> None
                    | Some (MOAll_type i) -> None
                    | Some (MOTuple_type lst) -> Some lst
                    | _ ->  raise (TypeError "Type checker was expecting a type tuple.")
                in
                let  nlst, locScope = 
                  (match new_const with
                    | None -> 
                              (List.fold_left 
                                    (fun (ntv, locScope) tvel -> 
                                      let (nel, locScope) =
                                        determine_type fulloptiprog optiprog locScope None tvel in
                                      ( nel::ntv, locScope)
                                      ) 
                                    ([], locScope) tv
                              )
                    | Some lst -> 
                              (List.fold_left2
                                    (fun (ntv, locScope) tvel const -> 
                                      let (nel, locScope) =
                                        determine_type fulloptiprog optiprog locScope (Some const) tvel in
                                      ( nel::ntv, locScope)
                                      ) 
                                    ([], locScope) tv lst
                              )
                   )
                   in
                    MOTuple_type (List.rev nlst), locScope
            | MOList_val lv ->
                let new_const = 
                  match const with
                    | None -> None
                    | Some (MOAll_type i) -> None
                    | Some (MOList_type typ) -> Some typ
                    | _ ->  raise (TypeError "Type checker was expecting a type list.")
                in

                (match lv with
                  | [] ->  MOAll_type (get_fresh_int ()), locScope
                  | lv -> 
                    let typ, locScope = 
                      determine_type fulloptiprog optiprog locScope new_const (List.hd lv) 
                    in
                    let typ, locScope = 
                      List.fold_left 
                        (fun (typ, locScope) lval -> 
                          let ntyp, locScope = determine_type fulloptiprog optiprog locScope new_const lval in 
                          determine_refine_type typ ntyp, locScope
                        )
                        (typ, locScope) (List.tl lv) 
                    in
                      MOList_type typ, locScope
                )
            | MONone_val -> 
                MOOption_type (MOAll_type (get_fresh_int ())), locScope
            | MOSome_val el -> 
                let new_const = 
                  match const with
                    | None -> None
                    | Some (MOAll_type i) -> None
                    | Some (MOOption_type typ)-> Some typ
                    | _ ->  raise (TypeError "Type checker was expecting a type option.")
                in
                let opt_typ, locScope = 
                  determine_type fulloptiprog optiprog locScope new_const el 
                in
                MOOption_type (opt_typ), locScope
            | MOSet_val sett -> 
                let new_const = 
                  match const with
                    | None -> None
                    | Some (MOAll_type i) -> None
                    | Some (MOSet_type typ)-> Some typ
                    | _ ->  raise (TypeError "Type checker was expecting a type set.")
                in
                let typ, locScope = 
                MSet.fold
                  (fun el (curtyp, locScope) -> 
                    let nel_type, locScope = 
                      determine_type fulloptiprog optiprog locScope
                        new_const (simple_to_core_val el)
                    in
                    determine_refine_type curtyp nel_type, locScope
                  )
                  sett
                  (MOAll_type (get_fresh_int ()), locScope)
                in
                MOSet_type typ, locScope
            | MOMap_val map -> 
                let new_const_key, new_const_val = 
                  match const with
                    | None -> None, None
                    | Some (MOAll_type i) -> None, None
                    | Some (MOMap_type (typkey, typval))-> Some typkey, Some typval
                    | _ ->  raise (TypeError "Type checker was expecting a type map.")
                in

                let keyt, elt,locScope = 
                MMap.fold
                  (fun key el (curkeyt, curelt, locScope) -> 
                    let keytyp, locScope = 
                      determine_type fulloptiprog optiprog locScope new_const_key
                        (simple_to_core_val key)
                    in 
                    let eltype, locScope = 
                      (determine_type fulloptiprog optiprog locScope new_const_val el)
                    in
                    (determine_refine_type curkeyt keytyp),
                    (determine_refine_type curelt eltype),
                    locScope
                  )
                  map (MOAll_type (get_fresh_int ()),
                       MOAll_type (get_fresh_int ()),locScope)
                in
                MOMap_type(keyt, elt), locScope
            | MOFun_val fv ->
                let lstargtyp_const, rettyp_const = 
                  match const with
                    | None -> None, None
                    | Some (MOAll_type i) -> None, None
                    | Some (MOFun_type(arglstTyp, rettyp))->
                        Some arglstTyp, Some rettyp
                    | _ ->  raise (TypeError "Type checker was expecting a type function.")
                in

                let locScope, nargs = add_args_to_scope lstargtyp_const fv.mofv_args_id locScope in  
                let body_type, locScope = 
                  determine_type fulloptiprog optiprog locScope rettyp_const 
                                 fv.mofv_body 
                in
                let nargs = refine_args_from_scope locScope fv.mofv_args_id in
                MOFun_type (nargs, body_type), locScope 
            | MOEmpty_val ->
              MOUnit_type, locScope
          )
      | MORef_val (ref, args) -> 
          (*we use locScope and only put to MOAll_type if not found in scope.*)
          (*we don't do intermodule type checking for now, so we only check ref
           * defined in this module.*)
          let typ_ref, locScope = 
            (*provide the raw type of ref (from the locScope) *)
          determine_type_ref fulloptiprog optiprog locScope ref 
          in
          (match args with 
            | [] -> 
                (match const with
                  | None -> 
                      determine_ref_with_index ref typ_ref, 
                      add_in_scope ref.morv_varname typ_ref locScope
                  | Some const ->
                      let refined_typ = determine_refine_type const typ_ref in
                      determine_ref_with_index ref refined_typ, 
                      add_in_scope ref.morv_varname refined_typ locScope
                )
            | args -> 
                determine_type_ref_function_call fulloptiprog optiprog const locScope 
                                            ref args typ_ref
          )
      | MOEnvRef_val _ -> 
        MOBase_type (MTypeString), locScope
      | MOBasicFunBody_val (op, arga, argb)
        ->
          determine_type_basic_fun fulloptiprog optiprog locScope const op arga argb
      | MOBind_val bd ->
          determine_type_binding fulloptiprog optiprog locScope const bd
      | MOIf_val (cond, thn, els) ->
          (*for now we do not worry about cond as it does not give info about
           * return type.*)
          let cond_typ, locScope = determine_type fulloptiprog optiprog locScope 
            (Some (MOBase_type MTypeBool)) cond in
          let thn_typ, locScope = determine_type fulloptiprog optiprog locScope const thn in
          let els_typ, locScope = (determine_type fulloptiprog optiprog locScope const els) in
          determine_refine_type thn_typ els_typ, locScope
      | MOComp_val (op, a, b ) ->
          (*a comparison can only return a boolean *)
          determine_type_comp fulloptiprog optiprog locScope const op a b
      | MOBody_val bdlst ->
          (*return the type of the last body*)
          let lastbd = List.nth bdlst ((List.length bdlst) -1) in
          determine_type fulloptiprog optiprog locScope const lastbd
    )
  in 
  let typ_res = check_respect_constraint const typ_res in
  typ_res, locScope



let prog_get_types fulloptiprog optiprog = 
  let determine_type_tupel (idtup, position) var_types = 
    let tup_typ = IntMap.find idtup var_types in
    get_type_from_tuple_el tup_typ position
  in
  (*In the first folding, we don't manage the MOTupEl_val *)
  let var_types = 
    let add_with_refine typmap i typ expr = 
      let refined = 
        match IntMap.find_opt i typmap with
          | None -> typ 
          | Some al_typ-> 
              (determine_refine_type ~id_var:(Some i) typ al_typ) 
      in 
      (* maybe usefill *)
(*       let typmap = constraint_type fulloptiprog optiprog typmap expr refined in *)
      IntMap.add i refined typmap
    in 
    IntMap.fold
      (fun id value typmap ->
        match value with 
          | MOTop_val e -> 
              let typ, locScope = determine_type fulloptiprog optiprog IntMap.empty None e in
              let typmap = IntMap.fold 
                (fun i typ typmap -> 
                  add_with_refine typmap i typ e
                ) 
                locScope typmap 
              in 
              add_with_refine typmap id typ e
          | MOTupEl_val _ -> typmap 
          (*for the moment, we don't add them as typed. They will get their type later.*)
      )
      optiprog.mopg_topvar IntMap.empty
  in
  (*we also run it for topcal *)
  let _typ, var_types = determine_type fulloptiprog optiprog var_types None optiprog.mopg_topcal in
  (*work over the MOTupEl_val *)
    IntMap.fold
      (fun id value typmap ->
        match value with 
          | MOTop_val _ -> typmap
          | MOTupEl_val (tup_id, position) ->
              let typ = determine_type_tupel (tup_id, position) var_types in
              IntMap.add id typ typmap 
      )
      optiprog.mopg_topvar var_types


let top_level_types fulloptiprog = 
  let toptypes_modules = IntMap.empty in 
  let toptypes_modules = 
    IntMap.fold 
      (fun modi el toptypes_modules-> 
        match el with 
          | MOSystemMod s -> 
              IntMap.add modi (GufoModules.get_types_map s) toptypes_modules 
          | MOUserMod optiprog -> (*TODO: if depend on nothing, gen type?*)
              IntMap.add modi (prog_get_types fulloptiprog optiprog) toptypes_modules
      )
    fulloptiprog.mofp_progmodules
    toptypes_modules
  in
  (*for main prog *)
  let toptypes_modules = IntMap.add 0 (prog_get_types fulloptiprog fulloptiprog.mofp_mainprog) toptypes_modules in

  toptypes_modules
(*
   * past_var_map is set to None when we are not in the console interactive. In
   * console interactive mode it contains a map which contains as key a new int
   * id for topvar and as value, the previous id value for the "same string"
   * var.
*)
let top_level_types_no_ref var_types past_var_map =

  let refine_bd_alltype req_typ real_typ bd_alltype = 
      match req_typ with 
        | MOAll_type(i) ->
          let cur_typ = IntMap.find_opt i bd_alltype in
          (match cur_typ with
              | None -> 
                  IntMap.add i real_typ bd_alltype, real_typ  
              | Some cur_typ ->
                  let real_typ = (determine_refine_type cur_typ real_typ) in
                  IntMap.add i real_typ bd_alltype, real_typ
          )
        | _ -> bd_alltype, real_typ
  in
    
  (*The function refine the rettyp of ref_typ according to the types of its arguments.*)
  let rec apply ref_typ args_typ bd_alltype = 
    match args_typ with
      | [] -> 
         let bd_alltype, ret_typ = refine_bd_alltype ref_typ ref_typ bd_alltype in
          bd_alltype, ret_typ 
      | typ :: args_typ -> 
          (match ref_typ with 
            | MOFun_type ([refarg], refret) ->
                let bd_alltype, _ = refine_bd_alltype typ refarg bd_alltype in
                apply refret args_typ bd_alltype
            | MOFun_type (refarg:: refargs , refret) ->
                let bd_alltype, _  = refine_bd_alltype refarg typ bd_alltype in
                apply (MOFun_type (refargs, refret)) args_typ bd_alltype
            | _ -> 
                bd_alltype, MOAll_type (get_fresh_int () )
          )
  in

  let refine_with_bd_alltyp args_typ bd_alltype =
    List.map
      (fun arg ->
       ( match arg with
        | MOAll_type(i) ->
          (match IntMap.find_opt i bd_alltype with
            | None -> arg
            | Some typ  -> typ
          )
        | _ -> arg
       )
      )
      args_typ
  in

  let get_module curmodulei refmodulei = 
    match refmodulei with 
      | None -> curmodulei 
      | Some mi -> mi
  in
  (* For a reference to modi.i, provide its type *)
  let rec find_and_get_type modi i deep bd_alltype = 
    let modtopvar_map = IntMap.find modi var_types in
    let typ = IntMap.find i modtopvar_map in
    let typ_with_deep = get_type_at_deep typ deep in 
    match typ_with_deep, past_var_map with 
      | MORef_type (nmodi, ni, ndeep, args), Some past_map ->
          (match nmodi, modi, ni, i with 
            | Some inmodi, modi, ni, i when (inmodi = modi && ni = i ) ->
                let past_i = IntMap.find i past_map in
                let modi = get_module modi nmodi in
                let bd_alltype, typ = find_and_get_type modi past_i ndeep bd_alltype in
                apply typ args bd_alltype 
            | None, modi, ni, i when (0 = modi && ni = i ) ->  
                let past_i = IntMap.find i past_map in
                let modi = get_module modi nmodi in
                let bd_alltype,typ  = find_and_get_type modi past_i ndeep bd_alltype in
                apply typ args bd_alltype
            | _ -> 
                let modi = get_module modi nmodi in
                let bd_alltype, typ = find_and_get_type modi ni ndeep bd_alltype in
                apply typ args bd_alltype
          )
      | MORef_type (nmodi, ni, ndeep, args), _  -> 
          let modi = get_module modi nmodi in
          let bd_alltype, typ = find_and_get_type modi ni ndeep bd_alltype in
          apply typ args bd_alltype
      | _,_ -> bd_alltype, typ_with_deep
  in

  let rec _recRemoveRef bd_alltype modi typ = 
    match typ with 
      | MOComposed_type ctyp ->
        let bd_alltype, new_moct_fields = 
            IntMap.fold
              (fun i tfield (bd_alltype, newtypmap) -> 
                let bd_alltype, motf_typ = 
                  _recRemoveRef bd_alltype modi tfield.motf_type 
                in
                bd_alltype, IntMap.add i {tfield with motf_type = motf_typ } newtypmap ) 
              ctyp.moct_fields (bd_alltype, IntMap.empty)
        in 
        bd_alltype, 
        MOComposed_type {ctyp with moct_fields = new_moct_fields}
      | MOBase_type _ -> bd_alltype, typ
      | MOTuple_type ttyp -> 
        let bd_alltype, tup_typ = 
          List.fold_left
          (fun (bd_alltype, newlst) typ ->
            let bd_alltype, el = _recRemoveRef bd_alltype modi typ in
            bd_alltype, el::newlst
           )
          (bd_alltype, []) ttyp 
        in bd_alltype, MOTuple_type (List.rev tup_typ)
      | MOList_type typ -> 
        let bd_alltype, typ = _recRemoveRef bd_alltype modi typ in
        bd_alltype, MOList_type typ
      | MOOption_type typ -> 
        let bd_alltype, typ = _recRemoveRef bd_alltype modi typ in
        bd_alltype, MOOption_type typ
      | MOSet_type typ -> 
        let bd_alltype, typ = _recRemoveRef bd_alltype modi typ in
        bd_alltype, MOSet_type typ
      | MOMap_type (typk, typv) -> 
          let bd_alltype, typk = _recRemoveRef bd_alltype modi typk in
          let bd_alltype, typv = _recRemoveRef bd_alltype modi typv in
          bd_alltype, MOMap_type (typk, typv)
      | MOFun_type (argst, rett) -> 
        let bd_alltype, new_lst_args = 
        List.fold_left
          (fun (bd_alltype, newlst) arg ->
            let bd_alltype, newel = _recRemoveRef bd_alltype modi arg in
            bd_alltype, newel::newlst
          )
          (bd_alltype, []) argst
        in
        
        let bd_alltype, newrett = _recRemoveRef bd_alltype modi rett in

        (*we can need to refine args type using the ret type.
          As in the following exemple
              let $af $i = $Int.toString $i
          For $af, rett is ref 1[0], for newrett, it is (fun int -> string) 
        *)
       let new_lst_args =  
          refine_with_bd_alltyp (List.rev new_lst_args) bd_alltype 
        in 

       (*let new_lst_args =  (List.rev new_lst_args) in*)
        bd_alltype, MOFun_type (new_lst_args, newrett)
      | MOAll_type _  -> bd_alltype , typ 
      | MOUnit_type  -> bd_alltype, typ 
      
      | MORef_type (refmodi, i , deep, args) -> 
         let modi = get_module modi refmodi in
         let bd_alltype, typ = find_and_get_type modi i deep bd_alltype in
            let bd_alltype, typ = apply typ args bd_alltype in
            bd_alltype, typ
      | MOTupel_type (refmodi , i , deep , args,  position) -> 
          let modi = get_module modi refmodi in
          let bd_alltype, typ = find_and_get_type modi i deep bd_alltype in
          let bd_alltype, typ = apply typ args bd_alltype in 
          bd_alltype, get_type_from_tuple_el typ position
    in

    (*IntMap.mapi
      (fun modi topvar_map ->
        IntMap.map (_recRemoveRef bd_alltype modi) topvar_map
      )
      var_types
    *)

    let new_topvar_types = 
    IntMap.fold
      (fun modi topvar_map newtopvar_map ->
        (*For every module in topvar map*)
        let newMap = 
          IntMap.fold
          (fun i typ  newMap -> 
            let _, newtyp = _recRemoveRef IntMap.empty modi typ in
            IntMap.add i newtyp newMap
          )
          topvar_map IntMap.empty
        in 
        IntMap.add modi newMap newtopvar_map
      )
      var_types (IntMap.empty)
    in 
    new_topvar_types



let get_type_from_topvar_types optiprog var_types (imod, idref, deep) args = 
  let modul = 
    (match imod with
      | None -> IntMap.find optiprog.mopg_name var_types
      | Some i -> IntMap.find i var_types 
    )
  in
  let base_typ = IntMap.find idref modul in
  let typ = 
    match deep with 
      | 0 -> base_typ
      | i -> get_type_at_deep base_typ deep
  in 
  determine_refapplication_type typ args IntMap.empty


(*Full type checking step *)


(*expr should have a type copatible with required_typ, else we would throw a
TypeError exception.*)
let type_check_has_type fulloptiprog optiprog var_types required_typ expr =
  debug_print (sprintf "checking expr '%s' has type '%s'\n" 
                        (moval_to_string expr) (type_to_string required_typ));
  let rec has_type required found = 
    let has_unique_type required found = 
    match required, found with
      | MOComposed_type ctyp, MOComposed_type ctyp2 ->
          ctyp.moct_name = ctyp2.moct_name
      | MOBase_type (MTypeString), MOBase_type (MTypeString) 
      | MOBase_type (MTypeBool), MOBase_type (MTypeBool) 
      | MOBase_type (MTypeInt), MOBase_type (MTypeInt) 
      | MOBase_type (MTypeFloat), MOBase_type (MTypeFloat) 
      | MOBase_type (MTypeCmd), MOBase_type (MTypeCmd) -> true 
      | MOTuple_type (typlst), MOTuple_type (typlst2) ->
          List.fold_left2 (fun b typa typb -> b && (has_type typa typb) ) true typlst typlst2
      | MOList_type (typa), MOList_type (typb) -> 
          has_type typa typb
      | MOOption_type (typa), MOOption_type (typb) -> 
          has_type typa typb
      | MOSet_type (typa), MOSet_type(typb) -> 
          has_type typa typb
      | MOMap_type (keya, vala), MOMap_type(keyb, valb) ->  
          (has_type keya keyb) && (has_type vala valb)
      | MOFun_type (argsa, reta), MOFun_type (argsb, retb) ->
          (has_type reta retb) && 
          List.fold_left2 (fun b arga argb -> b && (has_type arga argb)) true argsa argsb
      | MOAll_type i, MOAll_type i2 -> i = i2
      | MOUnit_type, MOUnit_type -> true
      | MOUnit_type, _ -> true (*TODO : this should be discussed. *)
      | MOAll_type i, _ -> true
      | _, MOAll_type i2 -> true
      | MORef_type (_,_,_,_), _ -> raise (InternalError "Gufo internal error (ref should not exist)") (*because we should have no more ref at this point *)
      | _, MORef_type(_,_,_,_) -> raise (InternalError "Gufo internal error (ref should not exist)")
      | MOTupel_type (_,_,_,_,_), _ -> raise (InternalError "unexpected tupel)")
      | _, MOTupel_type (_,_,_,_,_) ->  raise (InternalError "unexpected tupel)")
      | _, _ -> raise (TypeError (sprintf "Incompatible type: %s found while type %s required" 
                                  (type_to_string found) (type_to_string required)))
    in has_unique_type required found
  in
  let rec get_type_from_ref_ typ =
    match typ with 
      | MORef_type (imod, idref, deep, args) ->
          let typ = 
            get_type_from_topvar_types optiprog var_types (imod, idref, deep) args in 

            (*TODO P*)
          get_type_from_ref_ typ
      | MOTupel_type (imod, iref, deep,args, position) -> 
          let typ = 
            get_type_from_topvar_types optiprog var_types (imod, iref, deep) args in 
          get_type_from_ref_ (get_type_from_tuple_el typ position)
      | _ -> typ 
  in
  let typ_from_expr, _locScope = determine_type fulloptiprog optiprog 
                        (IntMap.find optiprog.mopg_name var_types) (Some required_typ) expr in
  let found_type = get_type_from_ref_ typ_from_expr in
  let _ = has_type required_typ found_type in
  found_type

(*This stuff is the stuff managing allTypeMap. 
  allTypeMap is a map describing the relation between types, especially with
  MOAll_type.
 *)
type allType_relation = {
  at_real_type : motype option;
  at_linked_type : IntSet.t
}

(*
 * debug an allType_relation, first argument is the id of the relation.
*)
let debug_print_allType_relation i rel = 
  (match rel.at_real_type with
    | None ->
      debug_print (sprintf "%d is not linked with real type " i) 
    | Some realtyp ->
      debug_print (sprintf "%d is linked with real type %s " i (type_to_string realtyp)) 
  );
  IntSet.iter 
    (fun linkedt -> 
      debug_print (sprintf "%d is linked with allType type %d " i  linkedt) 
    ) 
  rel.at_linked_type

let allType_getRealType_ t1 map = 
  match IntMap.find_opt t1 map with
    | None -> None
    | Some relation -> 
        relation.at_real_type 

let allType_get_linkedtype t1 map = 
  match IntMap.find_opt t1 map with
    | None -> IntSet.empty
    | Some relation -> 
        relation.at_linked_type

(*Try to get a real type either directly (because t1 is linked with a real type
or indirectly, because t1 is linked with a linked type linked with a real
one. If no type found, return t1.*)
let allType_getRealType t1 map =
  let search_though_linked_types idt1 map = 
    let rec search_though_linked_types_ idt1 map exploredSet = 
      let setToExplore = allType_get_linkedtype idt1 map in
        (*TODO: for now we just do find_first_opt, this means, we cannot have
          contradictory tipes, let's hope, it just never happen :) *)
        let possibleLinkedWithReal = 
        IntSet.find_first_opt
          (fun t -> match allType_getRealType_ t map with 
                      | None -> false
                      | Some _ -> true        
          ) 
          setToExplore
      in
      match possibleLinkedWithReal with
        | Some linkedt -> (allType_getRealType_ linkedt map), exploredSet
        | None -> (*no concrete stuff found*)
          let exploredSet = IntSet.union exploredSet setToExplore in
          (*we want the set of type linked with setToExplore with are not in exploredSet*)
          let toExplore = 
            IntSet.fold (fun t toExplore -> 
                          IntSet.union toExplore (allType_get_linkedtype t map) 
                        ) 
            setToExplore IntSet.empty
          in 
          let toExplore = IntSet.diff toExplore exploredSet in
          match IntSet.is_empty toExplore with
            | true -> None, exploredSet
            | false -> 
              IntSet.fold
                (fun linkedt (foundt,exploredSet) -> 
                  match foundt with
                    | Some t -> foundt, exploredSet (*do nothing, just return*)
                    | None -> 
                        search_though_linked_types_ linkedt map exploredSet 
                )
                toExplore (None, exploredSet)
    in
      search_though_linked_types_ idt1 map IntSet.empty
  in
  match t1 with 
    | MOAll_type idt1 -> 
      (match allType_getRealType_ idt1 map with
        | Some t -> t
        | None -> 
            let newt, _ = search_though_linked_types idt1 map in 
          (match newt with
            | None -> t1
            | Some newt -> newt
          )
      )
    | otherType -> t1

(*create the relation in the map if it does not exist.*)
let create_relation_if_not_exist t1 map = 
  let rel = 
    match IntMap.find_opt t1 map with
      | None -> {
                  at_real_type = None;
                  at_linked_type = IntSet.empty;
                }
      | Some rel -> rel 
    in 
    IntMap.add t1 rel map

let allType_addRelation t1 t2 map =
  debug_print (sprintf "Adding relation between %d and %d \n" t1 t2);
  (*we should create the relation in the map only we there is no conflict
    between t1 and tonly we there is no conflict between t1 and t2*)
  let map = create_relation_if_not_exist t1 map in 
  let map = create_relation_if_not_exist t2 map in 
  let relation1 = IntMap.find t1 map in
  let relation2 = IntMap.find t2 map in
  match allType_getRealType (MOAll_type t1) map , allType_getRealType (MOAll_type t2) map with
    | MOAll_type t1, MOAll_type t2 -> 
      debug_print (sprintf "Linking types");
      let relation1 = 
        { relation1 with at_linked_type = IntSet.add t2 relation1.at_linked_type;
        } in
      let relation2 = 
        { relation2 with at_linked_type = IntSet.add t2 relation2.at_linked_type;
        } in
      let map = IntMap.add t1 relation1 map in
      let map = IntMap.add t2 relation2 map in
      map
    | realType1, MOAll_type t2 -> 
      debug_print (sprintf "%d take type %s" t2 (type_to_string realType1));
      let relation2 =
        { relation2 with at_real_type = Some realType1} in 
        IntMap.add t2 relation2 map 
    | MOAll_type t1, realType2 -> 
      debug_print (sprintf "%d take type %s" t1 (type_to_string realType2));
      let relation1 =
        { relation1 with at_real_type = Some realType2} in 
        IntMap.add t1 relation1 map 
    | realType1, realType2 -> 
        let realType =  determine_refine_type realType1 realType2 in
        debug_print (sprintf "realtypes %s and %s are merged in %s"  (type_to_string realType1) (type_to_string realType2) (type_to_string realType) );
        let relation1 ={ relation1 with at_real_type = Some realType} in 
        let relation2 ={ relation2 with at_real_type = Some realType} in 
        let map = IntMap.add t1 relation1 map in
        let map = IntMap.add t2 relation2 map in
        map



let allType_addRealType t1 real_typ map = 
  let map = create_relation_if_not_exist t1 map in 
  let relation1 = IntMap.find t1 map in
  let relation1 = 
    match relation1.at_real_type with
      | None -> {relation1 with at_real_type = Some real_typ}
      | Some typ  ->
        {relation1 with 
          at_real_type = Some (determine_refine_type typ real_typ);
        }
  in 
  IntMap.add t1 relation1 map

(*Try to refine type using the information from the allTypeMap map.
  For exemple if your type is Option('9) and '9 is linked with type int, you
  will get Option(int).
*)
let rec refine_allTypeConstraint typ allTypeMap = 
  match typ with 
   | MOComposed_type mcomp -> 
    (*TODO : strangely moct_internal_val is a mtype_val and not an motype_val.
            for now, I left it, and comment the following code but it might to
            needed to investigate this.
      *)
(*
     let internal_val = 
      StringMap.map 
        (fun aval -> refine_allTypeConstraint aval allTypeMap)
        mcomp.moct_internal_val
     in
       MOComposed_type {mcomp with moct_internal_val = internal_val}
*)
       MOComposed_type mcomp  
   | MOBase_type bval -> MOBase_type bval
   | MOTuple_type tupTyp -> 
      MOTuple_type 
        (List.map (fun aval -> refine_allTypeConstraint aval allTypeMap) tupTyp)
   | MOList_type t -> MOList_type (refine_allTypeConstraint t allTypeMap)
   | MOOption_type t -> MOOption_type (refine_allTypeConstraint t allTypeMap)
   | MOSet_type t -> MOSet_type (refine_allTypeConstraint t allTypeMap)
   | MOMap_type (kt, vt) ->
       MOMap_type(refine_allTypeConstraint kt allTypeMap, 
                  refine_allTypeConstraint vt allTypeMap)
   | MOFun_type (argstyps, rettyp) ->
        let argstyps = 
          List.map (fun aval -> refine_allTypeConstraint aval allTypeMap) argstyps
        in
        let rettyp = refine_allTypeConstraint rettyp allTypeMap in
        MOFun_type (argstyps, rettyp)
   | MOAll_type i -> 
      allType_getRealType (MOAll_type i) allTypeMap
   | MOUnit_type -> MOUnit_type
   | MORef_type (_,_,_, _ ) -> raise (InternalError "type checker error.")
   | MOTupel_type _ -> raise (InternalError "type checker error.")


(*This function check an exepected type and an effectively found one, using it
to feed the allTypeMap map.
  For exemple if your real type is int while the expected one is '1, '1 will be
linked with te type int in the allTypeMap map.
*)
let type_check_has_type_with_allTypeConstraint 
      fulloptiprog optiprog var_types required_typ real_typ allTypeMap
  =
  let rec has_type required found allTypeMap = 
    let has_unique_type required found = 
    match required, found with
      | MOComposed_type ctyp, MOComposed_type ctyp2 ->
          (match ctyp.moct_name = ctyp2.moct_name with
            | true -> MOComposed_type ctyp, allTypeMap
            | false -> raise (TypeError 
                (sprintf "Incompatible type: %s found while type %s required"
                         (type_to_string found) (type_to_string required)))
          )
      | MOBase_type (MTypeString), MOBase_type (MTypeString) ->
          MOBase_type (MTypeString), allTypeMap
      | MOBase_type (MTypeBool), MOBase_type (MTypeBool) ->
          MOBase_type (MTypeBool), allTypeMap
      | MOBase_type (MTypeInt), MOBase_type (MTypeInt) ->
          MOBase_type (MTypeInt), allTypeMap
      | MOBase_type (MTypeFloat), MOBase_type (MTypeFloat) ->
          MOBase_type (MTypeFloat), allTypeMap
      | MOBase_type (MTypeCmd), MOBase_type (MTypeCmd) -> 
          MOBase_type (MTypeCmd), allTypeMap
      | MOTuple_type (typlst), MOTuple_type (typlst2) ->
          let new_typlst, allTypeMap = 
          List.fold_left2 
            (fun (new_typlst,allTypeMap) typa typb -> 
              let res, allTypeMap = (has_type typa typb allTypeMap) in
              res::new_typlst , allTypeMap
            )  
          ([],allTypeMap) typlst typlst2
          in 
            MOTuple_type (List.rev new_typlst), allTypeMap
      | MOList_type (typa), MOList_type (typb) -> 
          let newtyp, allTypeMap = has_type typa typb allTypeMap in
          MOList_type(newtyp), allTypeMap
      | MOOption_type (typa), MOOption_type (typb) -> 
          let newtyp, allTypeMap = has_type typa typb allTypeMap in
          MOOption_type newtyp, allTypeMap
      | MOSet_type (typa), MOSet_type(typb) -> 
          let newtyp, allTypeMap = has_type typa typb allTypeMap in
          MOSet_type (newtyp), allTypeMap
      | MOMap_type (keya, vala), MOMap_type(keyb, valb) ->  
          let resk, allTypeMap = (has_type keya keyb allTypeMap) in
          let resv, allTypeMap = (has_type vala valb allTypeMap) in
           MOMap_type(resk,resv), allTypeMap
      | MOFun_type (argsa, reta), MOFun_type (argsb, retb) ->
          let resret, allTypeMap = has_type reta retb allTypeMap in  
          let argstyps, allTypeMap =
          List.fold_left2 
            (fun (argstyps, allTypeMap) arga argb -> 
              let res, allTypeMap = (has_type arga argb allTypeMap) in
              res::argstyps , allTypeMap
            ) 
            ([], allTypeMap) argsa argsb
           in 
            MOFun_type(List.rev argstyps, resret), allTypeMap
      | MOAll_type i, MOAll_type i2 -> 
          MOAll_type i, allType_addRelation i i2 allTypeMap
      | MOUnit_type, MOUnit_type -> MOUnit_type, allTypeMap
      | MOUnit_type, _ -> MOUnit_type, allTypeMap (*TODO : this should be discussed. *)
      | MOAll_type i, typ -> typ, allType_addRealType i typ allTypeMap
      | typ, MOAll_type i2 -> typ, allType_addRealType i2 typ allTypeMap
      | MORef_type (mod1,idref1,deep1,arg1), _ -> 
          let required = 
            get_type_from_topvar_types optiprog var_types (mod1, idref1, deep1) arg1 
          in
          has_type required found allTypeMap 
      | _, MORef_type(_,_,_,_) -> raise (InternalError "Gufo internal error (ref should not exist2)")
      | MOTupel_type (_,_,_,_,_), _ -> raise (InternalError "unexpected tupel)")
      | _, MOTupel_type (_,_,_,_,_) ->  raise (InternalError "unexpected tupel)")
      | _, _ -> raise (TypeError (sprintf "Incompatible type: %s found while type %s required" 
                                  (type_to_string found) (type_to_string required)))
    in has_unique_type required found
  in
    let real_typ, allTypeMap = has_type required_typ real_typ allTypeMap in
    real_typ, allTypeMap

(*expr should have a type compatible with required_typ, else we would throw a
TypeError exception.*)
(*
  allTypeMap has type allType_relation IntMap.t
*)
let expr_check_has_type_with_allTypeConstraint 
      fulloptiprog optiprog var_types required_typ expr allTypeMap
  =
  let rec get_type_from_ref_ typ =
    match typ with 
      | MORef_type (imod, idref, deep, args) ->
          let typ = 
            get_type_from_topvar_types optiprog var_types (imod, idref, deep) args in 

            (*TODO P*)
          get_type_from_ref_ typ
      | MOTupel_type (imod, iref, deep,args, position) -> 
          let typ = 
            get_type_from_topvar_types optiprog var_types (imod, iref, deep) args in 
          get_type_from_ref_ (get_type_from_tuple_el typ position)
      | _ -> typ 
  in
  debug_print (sprintf "checking expr '%s' has type '%s'\n" (moval_to_string expr) (type_to_string required_typ));
  let typ_from_expr, _locScope = determine_type fulloptiprog optiprog 
                        (IntMap.find optiprog.mopg_name var_types) (Some required_typ) expr in
  let found_type = get_type_from_ref_ typ_from_expr in
  type_check_has_type_with_allTypeConstraint 
      fulloptiprog optiprog var_types required_typ found_type allTypeMap



    (*stringOrRef should always provide a string*)
let rec type_check_stringOrRef fulloptiprog optiprog typScope sor = 
  match sor with
    | MOSORString sor -> true
    | MOSORExpr aval -> 
        let _ = type_check_has_type fulloptiprog optiprog typScope (MOBase_type(MTypeString)) aval 
        in true



and type_check_cmdseq fulloptiprog optiprog typScope cmdseq = 
  let type_check_cmd_args args = 
    List.fold_left
      (fun b sor -> b && (type_check_stringOrRef fulloptiprog optiprog typScope sor ))
      true args
  in
  let type_check_cmd_output output = 
    match output with
      | MOCMDOStdOut
      | MOCMDOStdErr -> true
      | MOCMDOFile sor
      | MOCMDOFileAppend sor -> type_check_stringOrRef fulloptiprog optiprog typScope sor
  in
  let type_check_cmd_outputerr output = 
    match output with
      | MOCMDEStdOut
      | MOCMDEStdErr -> true
      | MOCMDEFile sor
      | MOCMDEFileAppend sor -> type_check_stringOrRef fulloptiprog optiprog typScope sor
  in
  let type_check_cmd_input input = 
    match input with
      | MOCMDIStdIn -> true
      | MOCMDIFile sor -> type_check_stringOrRef fulloptiprog optiprog typScope sor
  in
  let type_check_cmd cmd = 
    type_check_cmd_args cmd.mocm_args &&
    type_check_cmd_output cmd.mocm_output &&
    type_check_cmd_outputerr cmd.mocm_outputerr &&
    type_check_cmd_input cmd.mocm_input_src
  in
  match cmdseq with 
    | MOSimpleCmd cmd -> 
        type_check_cmd cmd
    | MOForkedCmd cmdseq -> type_check_cmdseq fulloptiprog optiprog typScope cmdseq
    | MOAndCmd (cmdseqa, cmdseqb) 
    | MOOrCmd (cmdseqa, cmdseqb)
    | MOSequenceCmd (cmdseqa, cmdseqb)
    | MOPipedCmd (cmdseqa, cmdseqb) -> type_check_cmdseq fulloptiprog optiprog typScope cmdseqa &&
                                       type_check_cmdseq fulloptiprog optiprog typScope cmdseqb


  (*This function returns true or fail with a TypeError exception *)
and type_check_val fulloptiprog optiprog typScope v = 
  let progScope = (IntMap.find optiprog.mopg_name typScope) in
  match v with 
    | MOSimple_val sv ->
        (match sv with 
          | MOBase_val MOTypeCmdVal cmdseq -> type_check_cmdseq fulloptiprog optiprog typScope cmdseq 
          | MOBase_val _ -> true
          | MOTuple_val tup -> 
              List.fold_left (fun b el -> b && type_check_val fulloptiprog optiprog typScope el) 
              true tup
          | MOList_val lst ->
              (match lst with
                | [] -> true
                | hd::lst -> 
                  let typ_hd,_ = determine_type fulloptiprog optiprog 
                            progScope None hd in
                  (*check that every list element has same type*)
                  List.fold_left 
                  (fun b el -> 
                    let _typ = (type_check_has_type fulloptiprog optiprog typScope typ_hd el) in
                    b && (type_check_val fulloptiprog optiprog typScope el)) 
                  true lst
              )
          | MONone_val -> true
          | MOSome_val v -> type_check_val fulloptiprog optiprog typScope v
          | MOSet_val set ->
              (try 
                let atyp,_ = determine_type fulloptiprog optiprog progScope
                  None (simple_to_core_val (MSet.choose set))
                in
                  MSet.fold
                    (fun el b -> 
                      let el_core = simple_to_core_val el in
                      let _typ = (type_check_has_type fulloptiprog optiprog typScope atyp el_core)
                      in
                      b && (type_check_val fulloptiprog optiprog typScope el_core))
                    set true 
              with Not_found -> true (*empty set is ok*)
              )
          | MOMap_val map -> 
              (try 
                let ex_key, ex_val = MMap.choose map in
                let (atypkey,_), (atypval,_) = 
                  determine_type fulloptiprog optiprog progScope None (simple_to_core_val ex_key), 
                  determine_type fulloptiprog optiprog progScope None ex_val
                in
                  MMap.fold
                    (fun k el b -> b &&
                      (
                        let k_core = simple_to_core_val k in
                        let _typ = (type_check_has_type fulloptiprog optiprog typScope atypkey (k_core)) in
                        let _typ = (type_check_has_type fulloptiprog optiprog typScope atypval el) in
                        (type_check_val fulloptiprog optiprog typScope k_core) && 
                        (type_check_val fulloptiprog optiprog typScope el)
                      )
                    )
                    map true 
              with | Not_found -> true (*empty map is ok*)
              )
          | MOFun_val fv -> 
              type_check_val fulloptiprog optiprog typScope fv.mofv_body 
          | MOEmpty_val -> true
        )
    | MOComposed_val ctypv ->
        let ctypv_modul, ctypv_id = ctypv.mocv_resolved_type in
        let ctyp = find_type_in_prog fulloptiprog ctypv_modul ctypv_id in
        (*we should check field are filled with correct types *)
        IntMap.fold 
          (fun i vf b -> 
            let tf = (IntMap.find i ctyp.moct_fields).motf_type in
            let _typ = (type_check_has_type fulloptiprog optiprog typScope tf vf) in
            b && (type_check_val fulloptiprog optiprog typScope vf)
          ) 
          ctypv.mocv_fields true 
    | MORef_val (ref, args) ->

        let check_with_lst =
          match ref.morv_index with
            | None -> true
            | Some lst -> 
                let _check_is_int =
                  List.iter
                  (fun v -> 
                    let _ = 
                      type_check_has_type fulloptiprog optiprog typScope 
                        (MOBase_type MTypeInt ) v in
                    ()
                  )
                   lst
                in
                  List.fold_left 
                    (fun b v -> type_check_val fulloptiprog optiprog typScope v)
                    true lst
        in

        (*if there is args, this is a function call, we have to check the
         * argument of the call.*)
        (match args with
          | [] -> check_with_lst
          | lst ->
            (**allTypeMap allows to check that the 'alltype' within the
            function call are coherent.
            For exemple if the function f has signature f: '1 -> '1 -> '2,
            allTypeMap will check that the first and the second argument are
            compatibles.
            *)
            let check_ref reft args = 
              (match reft with
                | MORef_type (modul, id_ref,deep,nargs) ->
                    (*There should be no ref type at this level.*)
                    true
                | MOFun_type(args_type, ret) ->
                    (*This is possible that there are more args than there
                      * are required argument. In that case the nth
                      * outbounds. 
                      * This should be possible only if the ret is itself a fun.
                      * *)
                    (match (Pervasives.compare (List.length args) (List.length args_type) )
                    with 
                      | i when i > 0 ->  
                          (*TODO*)
                          true
                      | _ -> 
                      let res,_, _allTypeMap = 
                        List.fold_left 
                        (fun (b,pos, allTypeMap) arg ->  
                            let arg_req_type = List.nth args_type pos in
                            let typ, allTypeMap = 
                                expr_check_has_type_with_allTypeConstraint 
                                  fulloptiprog optiprog typScope arg_req_type arg allTypeMap
                            in
                            (debug_print (sprintf "arg should have type %s \n" (type_to_string arg_req_type)));
                            (debug_print (sprintf "arg has type %s \n" (type_to_string typ)));
                            (b && (type_check_val fulloptiprog optiprog typScope arg)), pos+1, allTypeMap
                        ) (true,0, IntMap.empty) args 
                      in res
                    )
                | _ -> 
                    raise (TypeError "Invalid function call")
              )
            in
            let ftyp = get_type_from_ref fulloptiprog optiprog typScope ref in 
            check_ref ftyp args  
         )
    | MOEnvRef_val (_) -> 
      true
    | MOBasicFunBody_val (op, a, b) -> 
(*
        let typ =  determine_type_basic_fun fulloptiprog optiprog progScope None op a b in
        let _typa,_ = determine_type fulloptiprog optiprog progScope None a in
        let _typb,_ = determine_type fulloptiprog optiprog progScope None b in
*)
        (type_check_val fulloptiprog optiprog typScope a) && (type_check_val fulloptiprog optiprog typScope b)
    | MOBind_val bd ->
        type_check_val fulloptiprog optiprog typScope bd.mobd_value && type_check_val fulloptiprog optiprog typScope bd.mobd_body
    | MOIf_val (cond, thn, els) ->
        let _typ = type_check_has_type fulloptiprog optiprog typScope (MOBase_type MTypeBool) cond in
        (let thn_type,_ = determine_type fulloptiprog optiprog progScope None thn
         in 
         let _typ = type_check_has_type fulloptiprog optiprog typScope thn_type els in
            type_check_val fulloptiprog optiprog typScope cond && 
            type_check_val fulloptiprog optiprog typScope thn && 
            type_check_val fulloptiprog optiprog typScope els
      )
    | MOComp_val (op, a , b) ->
        let typa,_ = determine_type fulloptiprog optiprog progScope None a in
        let _typ = type_check_has_type fulloptiprog optiprog typScope typa b in
        type_check_val fulloptiprog optiprog typScope a && 
        type_check_val fulloptiprog optiprog typScope b
    | MOBody_val lstbody -> 
        let rev = List.rev lstbody in
        let main, _others = List.hd rev, List.rev (List.tl rev) in
        (*TODO check others against unit*)
(*         List.fold_left (fun b bd -> ) true others *)
        type_check_val fulloptiprog optiprog typScope main


let type_check_prog fulloptiprog optiprog top_types = 
  IntMap.iter
  (fun i v -> 
    match v with 
      | MOTop_val v -> let _res = type_check_val fulloptiprog optiprog top_types v in ()
      | MOTupEl_val _ -> ()
  )
  optiprog.mopg_topvar;
  let _ = type_check_val fulloptiprog optiprog top_types optiprog.mopg_topcal in ()


(*For every user module, valid correct typing or throw a TypeError exception.
*)
let type_check fulloptiprog top_types = 
  debug_info (debug_title3 "Doing final type check");
  let _ = 
    (*check every modules*)
    IntMap.iter
      (fun modi el -> 
        match el with 
          | MOSystemMod s -> ()
              (*We don't check them*)
          | MOUserMod optiprog -> 
              type_check_prog fulloptiprog optiprog top_types
      )
      fulloptiprog.mofp_progmodules
  in
  (*for main prog *)
  type_check_prog fulloptiprog fulloptiprog.mofp_mainprog top_types
(*
  ;
  let finalAlltypeMap = (!alltypeMap) in
  (*check the coherency of the alltypeMap *)
  IntMap.iteri 
    (fun i lstConstraint ->  
      (*We first check that there is no uncoherency within the list of constraint*)
      let res_typ = 
      List.fold_left 
        (fun res_typ const -> 
          determine_refine_type res_typ const
        )
        (MOAll_type (get_fresh_int ())) lstConstraint
    ) (finalAlltypeMap)
*)



  (*debug function: print for every top level, the type in order to control.*)
let debug_to_level_type fulloptiprog var_types = 

  debug_info (debug_title3 "toplevel var (and type)");
  (*if the iname come from a tuple reference, it will not be present in
   * mopg_topvar_debugname*)
  let find_debug_name prog iname = 
    try IntMap.find iname prog.mopg_topvar_debugname
    with Not_found  -> "_tuple_"

  in 
  IntMap.iter
    (
      (*for every modules*)
      fun modi toptypmap ->
        let mod_str = IntMap.find modi fulloptiprog.mofp_progmap_debug in
        IntMap.iter 
          (fun var typ ->
            let var_str = 
              match IntMap.find_opt modi fulloptiprog.mofp_progmodules with
                | None -> find_debug_name fulloptiprog.mofp_mainprog var
                | Some (MOUserMod mprog) -> find_debug_name mprog var
                | Some (MOSystemMod sysmod) -> (IntMap.find var sysmod.mosm_topvar).mosmv_name
            in
            debug_print (sprintf "from module %s: %d (%s) : %s\n" mod_str var var_str (type_to_string typ))
          )
          toptypmap
    )
    var_types

(*END Determining the type of every top level function *)
  (* END PART 3: FULL TYPE CHECKING *)

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

  let rec parsedToOpt_composed_val ?topvar:(topvar = None) optiprog locScope mct = 
    (*the field name allow to recognize the composed type, we then check every
     * field has name according to the same object.*)
    let parsedToOpt_field def_modul fd =
      let parsedToOpt_field_in_prog prog fd = 
          match StringMap.find_opt fd.mtfv_name prog.mopg_field2int with 
            | None -> raise (TypeError ("field "^fd.mtfv_name^" does not belong to a known type"))
            | Some i -> IntMap.find i prog.mopg_field_to_type
      in
      match def_modul with 
        | None -> parsedToOpt_field_in_prog optiprog fd
        | Some (MOUserMod userprog) -> parsedToOpt_field_in_prog userprog fd
        | Some (MOSystemMod sysmod) -> 
            (match StringMap.find_opt fd.mtfv_name sysmod.mosm_typstrfield2inttype with
              | None -> raise (TypeError ("field "^fd.mtfv_name^" does not belong to a known type"))
              | Some i -> i)
    in
    let check_valid_fields def_modul ctype field =
      let check_valid_fields_in_prog prog = 
        (*check the field belong to the given ctype *)
          match StringMap.find_opt field.mtfv_name prog.mopg_field2int with 
            | None -> raise (TypeError ("field "^field.mtfv_name^" does not belong to a known type"))
            | Some i -> ctype = IntMap.find i prog.mopg_field_to_type
      in
        match def_modul with
          | None -> check_valid_fields_in_prog optiprog 
          | Some (MOUserMod userprog) -> check_valid_fields_in_prog userprog
          | Some (MOSystemMod sysmod) -> 
            (match StringMap.find_opt field.mtfv_name sysmod.mosm_typstrfield2inttype with 
              | None ->raise  (TypeError ("field "^field.mtfv_name^" does not belong to a known type")) 
              | Some i -> ctype = i
            )
    in
    let add_to_field_map def_modul locScope fd fieldsmap = 
      let iname = 
        match def_modul with
          | None -> (StringMap.find fd.mtfv_name optiprog.mopg_field2int)
          | Some (MOUserMod userprog ) -> (StringMap.find fd.mtfv_name userprog.mopg_field2int)
          | Some (MOSystemMod sysmod) -> (StringMap.find fd.mtfv_name sysmod.mosm_typstrfield2int)
      in
      IntMap.add iname (parsedToOpt_expr ~topvar optiprog locScope fd.mtfv_val) fieldsmap
    in
  
    let def_modul, def_modul_i = 
      match mct.mcv_module_def with
        | None -> None , None
        | Some modstr -> 
          let modulint = 
            (match StringMap.find_opt modstr fulloptiprog.mofp_progmap with
              | None -> raise (VarError (modstr^" does not appear to be an available module."))
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
          match check_valid_fields def_modul ctype fd with
            | true -> ()
            | false -> raise (TypeError (fd.mtfv_name^" does not belong to the correct struct."))
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
            | Some mdstring -> Some (StringMap.find mdstring fulloptiprog.mofp_progmap ))
            ;
        mocv_fields = mo_fields;
        mocv_resolved_type = (def_modul_i, ctype);
      }
  
  and parsedToOpt_stringOrRef ?topvar:(topvar = None) optiprog locScope sor = 
    match sor with 
    | SORString s -> MOSORString s
    | SORExpr tval -> MOSORExpr (parsedToOpt_expr ~topvar optiprog locScope tval)
  
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
    {
      mocm_cmd = cseq.mcm_cmd;
      mocm_args = List.map (parsedToOpt_stringOrRef ~topvar optiprog locScope) cseq.mcm_args;
      mocm_output = parsedToOpt_cmd_output ~topvar optiprog locScope cseq.mcm_output;
      mocm_outputerr = parsedToOpt_cmd_outputerr ~topvar optiprog locScope cseq.mcm_outputerr;
      mocm_input_src = parsedToOpt_cmd_input_src ~topvar optiprog locScope cseq.mcm_input_src;
      mocm_res = None;
      mocm_input = None;
      mocm_print = None;
      mocm_print_error = None;
      mocm_print_std = None;
    }
  
  
  and parsedToOpt_cmd_seq_val ?topvar:(topvar = None) optiprog locScope cseq = 
    match cseq with 
      | SimpleCmd cval -> 
          MOSimpleCmd (parsedToOpt_cmd_val ~topvar optiprog locScope cval)
      | ForkedCmd fcmd -> 
          MOForkedCmd (parsedToOpt_cmd_seq_val ~topvar optiprog locScope fcmd)
      | AndCmd (cseq1, cseq2) -> 
          MOAndCmd (parsedToOpt_cmd_seq_val ~topvar optiprog locScope cseq1,
                    parsedToOpt_cmd_seq_val ~topvar optiprog locScope cseq2)
      | OrCmd (cseq1, cseq2)-> 
          MOOrCmd (parsedToOpt_cmd_seq_val ~topvar optiprog locScope cseq1,
                   parsedToOpt_cmd_seq_val ~topvar optiprog locScope cseq2)
      | SequenceCmd (cseq1, cseq2)-> 
          MOSequenceCmd (parsedToOpt_cmd_seq_val ~topvar optiprog locScope cseq1,
                         parsedToOpt_cmd_seq_val ~topvar optiprog locScope cseq2)
      | PipedCmd (cseq1, cseq2)-> 
          MOPipedCmd (parsedToOpt_cmd_seq_val ~topvar optiprog locScope cseq1,
                      parsedToOpt_cmd_seq_val ~topvar optiprog locScope cseq2)
  
  
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
        StringMap.add s i nameMap, MOBaseArg i, StringMap.add s i locScope
    | MTupleArg arglst ->
        let onames, locScope, newlst = 
          List.fold_left
            (fun (onames,locScope,newlst) arg -> 
              let nonames, el, locScope = 
                parsedToOpt_funarg nameMap locScope arg 
              in
              (StringMap.merge (fun s a b -> a) onames nonames, locScope, el::newlst)
            )
            (StringMap.empty,locScope,[]) arglst
        in
        onames, MOTupleArg newlst, locScope
    
  
  and parsedToOpt_simple_val ?topvar:(topvar = None) optiprog locScope ms =
    match ms with 
    | MEmpty_val -> MOEmpty_val
    | MBase_val bv -> 
        MOBase_val (parsedToOpt_base_val ~topvar optiprog locScope bv)
    | MTuple_val tup -> 
        MOTuple_val (List.map (parsedToOpt_expr ~topvar optiprog locScope) tup)
    | MList_val lst -> 
        MOList_val (List.map (parsedToOpt_expr ~topvar optiprog locScope) lst)
    | MNone_val -> MONone_val
    | MSome_val s -> 
        MOSome_val (parsedToOpt_expr ~topvar optiprog locScope s)
    | MSet_val slst ->
        let set = 
          List.fold_left 
            (fun set el -> 
              MSet.add (core_to_simple_val
                        (parsedToOpt_expr ~topvar optiprog locScope el)) set
            ) 
            MSet.empty slst
        in MOSet_val set
    | MMap_val mlst -> 
        let map = 
          List.fold_left 
            (fun map (k,el) -> 
              MMap.add (core_to_simple_val (parsedToOpt_expr ~topvar optiprog locScope k)) 
                       (parsedToOpt_expr ~topvar optiprog locScope el) map
            ) 
            MMap.empty mlst
        in MOMap_val map
    | MFun_val (args, body) -> 
      let onames,locScope, ofunargs = 
        List.fold_left 
          (fun (onames,locScope, newlst) arg -> 
            let nonames, oarg, locScope = parsedToOpt_funarg onames locScope arg in
            nonames,locScope, oarg::newlst
          ) 
          (StringMap.empty,locScope,[]) args 
      in 
      let obody = parsedToOpt_expr ~topvar optiprog locScope body in
      MOFun_val 
        {mofv_args_name = onames; mofv_args_id = ofunargs; mofv_body = obody}
  
  
  and parsedToOpt_ref ?topvar:(topvar = None) optiprog locScope ref  = 

    let resolve_fd prog fdname = 
      match StringMap.find_opt fdname prog.mopg_field2int with
        | None -> raise (TypeError (sprintf "field %s does not belong to a known field." fdname ))
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
      let vname = List.hd ref.mrv_varname in
      (*get the variable*)
      let ires = 
        match modul  with
          | None ->
            (match search_in_scope locScope vname with 
              | Some vint -> vint
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
          | None -> ires
          | Some (topvars, past_var_map) ->
              (
                match using_a_topvar topvars past_var_map ires with
                  | None -> ires
                  | Some topvar -> 
                      (*When topvar is the same id as the one found (we want to
                        take value from past_var_map if available in it.)*)
                      (match IntMap.find_opt topvar past_var_map with
                        | None -> raise (VarError (sprintf "Trying to use undefined variable %s ." vname))
                        | Some i -> i 
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
          ([], None) (List.tl ref.mrv_varname)
      in
        let newlst,_ = fields in 
       newlst 
    in
    let resolve_index ref = 
      (match ref.mrv_index with
                | None -> None
                | Some vlst -> Some (List.map (parsedToOpt_expr ~topvar optiprog locScope) vlst)
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
                "" ref.mrv_varname;
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
                (fun str curstr -> sprintf "%s.%s" str curstr) "" ref.mrv_varname;
          }
  and parsedToOpt_binding ?topvar:(topvar = None) optiprog locScope bd = 
    let add_in_bindlist bindScope bindList debugnames elname position = 
      match StringMap.find_opt elname bindScope with
        | None -> 
            let id = get_fresh_int () in 
            (StringMap.add elname id bindScope), 
            (id, position)::bindList,
            IntMap.add id elname debugnames,
            position
        | Some _ -> raise (VarError ("Cannot bind same variable name in same tuple: variable $"^elname^" ." ))
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
        (match bd.mbd_value with 
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
    match expr with
      | MComposed_val mct -> 
          parsedToOpt_composed_val ~topvar optiprog locScope mct
      | MSimple_val sv -> MOSimple_val (parsedToOpt_simple_val ~topvar optiprog locScope sv)
      | MRef_val (ref, args) -> 
          let oref = parsedToOpt_ref ~topvar optiprog locScope ref  in
          MORef_val (oref, List.map (parsedToOpt_expr ~topvar optiprog locScope) args)
      | MEnvRef_val (var) -> 
          MOEnvRef_val var
      | MBasicFunBody_val (op, args) ->
          (match args with 
          |  [a;b] -> 
              MOBasicFunBody_val (op, parsedToOpt_expr ~topvar optiprog locScope a, 
                                      parsedToOpt_expr ~topvar optiprog locScope b)
          |  _ -> raise (SyntaxError "basic operators (+,-,/,mod...) are binary.")
          )
      | MBind_val bd -> 
          MOBind_val (parsedToOpt_binding ~topvar optiprog locScope bd)
      | MIf_val (cond, thn, els) ->
          MOIf_val(parsedToOpt_expr ~topvar optiprog locScope cond,
                   parsedToOpt_expr ~topvar optiprog locScope thn, 
                   parsedToOpt_expr ~topvar optiprog locScope els)
      | MComp_val (op, left, right) -> 
          MOComp_val (op, parsedToOpt_expr ~topvar optiprog locScope left, 
                      parsedToOpt_expr ~topvar optiprog locScope right)
      | MBody_val dblst -> 
          MOBody_val (List.map (parsedToOpt_expr ~topvar optiprog locScope) dblst)
  
  
  in
  (*return the content optimised val of mvarlst and the information for mopg_topvar_bind_vars*)
  let mapping_mpg_topvar optiprog mvarlst = 
    List.fold_left
      (fun map mvar -> 
        match mvar.mva_name with
          | MBaseDecl aname ->
              let iname = (StringMap.find aname optiprog.mopg_topvar2int) in
              let res_expr = 
              match mvar.mva_value with
              (*the idea is that when the topvar is a function, we want it to
                 be potientially recursive and so it can use reference to
                itself to be recursive.

                When working on something other than a function, we want the a
                previous definition of the variable to be usable (if there was
                one).
                *)
                | MSimple_val (MFun_val (_, _)) ->
                  (parsedToOpt_expr  ~topvar:(None) optiprog (StringMap.add aname iname StringMap.empty) mvar.mva_value)
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
                      IntMap.add (StringMap.find aname optiprog.mopg_topvar2int)
                        (MOTupEl_val (tup_id, List.rev (new_position))) map, new_position
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
                                (IntSet.add (StringMap.find aname
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
        | None -> StringMap.add name (get_fresh_int()) map
        | Some _ -> raise (VarError ("Cannot redeclare toplevel variable: variable $"^name^" ." ))
    in
    let rec add_topel el map = 
      match el with 
        | MBaseDecl aname -> add_topvar aname map
        | MTupDecl nlst -> List.fold_left (fun map el -> add_topel el map) map nlst
    in
    List.fold_left
      (fun map mvar -> add_topel mvar.mva_name map) 
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
      match typ with
        |  MOUnit_type  
        |  MOAll_type _ 
        |  MOBase_type _ -> typ
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
          StringMap.add strel (get_fresh_int () ) newmap 
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
    let typname = List.hd ref.mrv_varname in
    let inttyp = StringMap.find_opt typname optiprog.mopg_ctype2int in
    (match inttyp, ref.mrv_varname with 
      | Some inttyp, [typname] -> 
          let modul = 
            (match ref.mrv_module with 
                              | None -> None 
                              | Some str -> Some (GufoModules.get_intname_from_modulestr str fulloptiprog)
            )
          in
          MORef_type (modul, inttyp, 0, [])
      | None, [typename] -> raise (TypeError ("Type not found: "^ typename))
      | _,  lst -> raise (TypeError 
                            ("unvalide type declaration"^(List.fold_left (fun str el -> str^" "^el)"" lst)))
      )
  in

  let rec parsedToOpt_type_simpletype st = 
    match st with 
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


  in



  let transform_composed_type_field fd = 
    let intname = get_fresh_int () in
    {
      motf_name  = intname;
      motf_type = parsedToOpt_type_simpletype fd.mtf_type;
      motf_debugname = fd.mtf_name;
    }
  in

  let check_fields optiprog mc_intname mc = 
       List.fold_left
       (fun (optiprog,map_res) fd -> 
         let ofd = transform_composed_type_field fd in
         (*we need to check there is no other field with the same name*)
         match StringMap.mem fd.mtf_name optiprog.mopg_field2int with
          | true ->
            raise (TypeError ("the field "^fd.mtf_name^" is used several times (possibly in different structs)"))
          | false  -> 
              (*we add it*)
            let optiprog = {optiprog with mopg_field2int = StringMap.add fd.mtf_name ofd.motf_name optiprog.mopg_field2int} in
            let optiprog = {optiprog with mopg_field_to_type = IntMap.add ofd.motf_name mc_intname optiprog.mopg_field_to_type} in
            (optiprog, (IntMap.add ofd.motf_name ofd map_res ))
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



(*PART 4 
 * During first part of part 3, we determine a type using the determine_type
function. However, they keep beiing unprecise because there is still
references.
  For exemple if you have funa(funb(anArg)), if funb type is deduced from the
anArg, the return type of funa might be unprecise.
  Here, we do a bottom to top analysis:
    We consider the type of anArg, to determine the type the call funb(anArg),
to finally get the one of this call of funa(funb(anArg).
type_check_val_to_top returns a new var_types and a current resulting type.
*
  For now this function use determine_refine_type but this function might not be enought; it does not update var_types. It does not link a MOAllType to a concrete type.
*)


let rec type_check_val_to_top fulloptiprog optiprog modi varval var_types = 
  (*var_types contains strictly more accurate information than a locScope (as
used in the determine_type* functions.). So to call some determine_type*
function we used it to not lost informations. *)
(*   let locScope = IntMap.find optiprog.mopg_name var_types in *)
  let get_type_from_var_types var_types idmodul id  =
    match IntMap.find_opt idmodul var_types with
      | None -> raise (InternalError "type checker error.")(*We hope this cannot happen.*)
      | Some modulmap -> 
          (match IntMap.find_opt id modulmap with
            | None -> raise (InternalError "type checker error.")(*We hope this cannot happen.*)
            | Some typ -> typ
          )
  in 

  let rec type_check_val_to_top_funargs args var_types = 
    List.map
    (fun funarg -> 
      match funarg with
        | MOBaseArg i -> get_type_from_var_types var_types modi i
        | MOTupleArg lstfunarg -> 
            (MOTuple_type (type_check_val_to_top_funargs lstfunarg var_types) )
    ) args
  in

  (*Specific for commands *)
  let rec do_analyse_cmd var_types cmdVal =
    let do_analyse_stringOrRef_cmd var_types sor =
      match sor with 
        | MOSORString _ -> var_types
        | MOSORExpr e -> 
            let var_types, _ = 
              type_check_val_to_top fulloptiprog optiprog modi e var_types in
            var_types
    in
    let do_analyse_output_cmd var_types out = 
      match out with
        | MOCMDOStdOut
        | MOCMDOStdErr -> var_types
        | MOCMDOFile sor   
        | MOCMDOFileAppend sor -> do_analyse_stringOrRef_cmd var_types sor
    in
    let do_analyse_outputerr_cmd var_types out = 
      match out with
        | MOCMDEStdOut
        | MOCMDEStdErr -> var_types
        | MOCMDEFile sor 
        | MOCMDEFileAppend sor -> do_analyse_stringOrRef_cmd var_types sor
    in
    let do_analyse_input var_types inp = 
      match inp with
        | MOCMDIStdIn -> var_types
        | MOCMDIFile sor -> do_analyse_stringOrRef_cmd var_types sor
    in
    match cmdVal with 
      | MOSimpleCmd cval ->
          let var_types = 
            List.fold_left 
            (fun var_types arg -> do_analyse_stringOrRef_cmd var_types arg 
            ) var_types cval.mocm_args in
          let var_types = do_analyse_output_cmd var_types cval.mocm_output in
          let var_types = do_analyse_outputerr_cmd var_types cval.mocm_outputerr in
          let var_types = do_analyse_input var_types cval.mocm_input_src in
          var_types
      | MOForkedCmd seq -> do_analyse_cmd var_types seq
      | MOAndCmd (cmd1, cmd2) ->
          do_analyse_cmd (do_analyse_cmd var_types cmd1) cmd2 
      | MOOrCmd (cmd1, cmd2) ->
          do_analyse_cmd (do_analyse_cmd var_types cmd1) cmd2 
      | MOSequenceCmd (cmd1, cmd2 )->
          do_analyse_cmd (do_analyse_cmd var_types cmd1) cmd2 
      | MOPipedCmd (cmd1, cmd2) -> 
          do_analyse_cmd (do_analyse_cmd var_types cmd1) cmd2 
  in
  (*Specific function for funcall*)
  let do_analyse_funcall var_types refval args args_typ =
    let allTypeMap = IntMap.empty in
    (*Check if a real arguments is compatible with the expected one and return the resulting type.*)
    let check_arg required real allTypeMap = 
      type_check_has_type_with_allTypeConstraint fulloptiprog optiprog var_types required real allTypeMap
    in

    (*For now this function just return an updated allTypeMap without worrying about var_types. TODO: it should update var_types. *)
    let rec do_analyse_funcall_ var_types ftyp args_types allTypeMap = 
      match ftyp with
        | MOFun_type(args_required_type, ret) ->
          (match (Pervasives.compare (List.length args_types) (List.length args_required_type) )
              with 
                | i when i > 0 ->  
                  (*This is possible that there are more args than there
                   * are required argument. In that case the nth
                   * outbounds. 
                   * This should be possible only if the ret is itself a fun.
                   * *)
                  (*We check every arg catched by the function, and rec call on
                   * the returned type + the additionnal arguments.*)
                  let cur_fun_args_types, ret_fun_args_types = 
                    list_split_at_idx args_types (List.length args_required_type) 
                  in
                  let allTypeMap =
                    List.fold_left2 (fun allTypeMap required real -> 
                                      let _,allTypeMap = 
                                        check_arg required real allTypeMap in
                                      allTypeMap
                                    ) 
                      allTypeMap
                      args_required_type 
                      cur_fun_args_types
                  in
                  let ret = allType_getRealType ret allTypeMap in
                  let ret, allTypeMap = 
                    do_analyse_funcall_ var_types ret ret_fun_args_types allTypeMap
                  in
                  ret, allTypeMap
                | _ -> 
                    let cur_fun_required_types, _ = 
                      list_split_at_idx args_required_type (List.length args_types) 
                    in
                    let allTypeMap,_ = 
                    List.fold_left2 
                      (fun (allTypeMap,args) required real -> 
                        let _,allTypeMap = 
                          check_arg required real allTypeMap in
                        let cur_arg, args =
                          match args with
                            | [] -> assert false
                            | [a] -> a, []
                            | a::b -> a, b
                        in
                        let required = refine_allTypeConstraint required allTypeMap
                        in
                        let _, allTypeMap =
                        expr_check_has_type_with_allTypeConstraint
                          fulloptiprog optiprog var_types required cur_arg allTypeMap
                        in
                        allTypeMap, args
                      ) 
                      (allTypeMap, args)
                      cur_fun_required_types
                      args_types
                    in
                    (*the types of the argument might impact the ret type*)
                    (*TODO: debug allTypeMap*)
                    let ret = allType_getRealType ret allTypeMap in
                    ret, allTypeMap
          )
          | _ -> 
              raise (TypeError "Invalid function call")
    in
    (*first we check the arguments *)
(*
    let var_types, args_types = 
      List.fold_left (fun (var_types, args_types) v -> 
        let var_types, next_type =
        type_check_val_to_top fulloptiprog optiprog v var_types
        in
        var_types, next_type::args_types
      ) (var_types, [] ) args
    in 
*)
    let funname, field = refval.morv_varname in
    (*then the function itself *)
    let modulId = 
      match refval.morv_module with
        | None -> optiprog.mopg_name 
            (*need to get the current module id*)
        | Some modulId -> modulId
    in 
    let funtype =  
      match (List.rev field) with 
        | [] -> get_type_from_var_types var_types modulId funname 
        | fd::lst -> (get_type_field_from_field fulloptiprog optiprog fd).motf_type
    in
    
      debug_print(sprintf "function %d ret type before analysis: %s \n"  funname (type_to_string funtype));
    match args_typ with
      | [] -> 
        (*no args, we return funtype*) (funtype, var_types)
      | lst -> 
          let funtyp, allTypeMap = do_analyse_funcall_ var_types funtype args_typ allTypeMap
          in  funtyp, var_types
  in

  match varval with 
    | MOSimple_val (MOBase_val (MOTypeCmdVal cmdVal)) -> 
        do_analyse_cmd var_types cmdVal , (MOBase_type MTypeCmd)
    | MOSimple_val (MOBase_val (MOTypeStringVal _)) -> 
        var_types, (MOBase_type MTypeString)
    | MOSimple_val (MOBase_val (MOTypeIntVal _)) -> 
        var_types, MOBase_type MTypeInt
    | MOSimple_val (MOBase_val (MOTypeBoolVal _)) -> 
        var_types, MOBase_type MTypeBool
    | MOSimple_val (MOBase_val (MOTypeFloatVal _)) -> 
        var_types, MOBase_type MTypeFloat
    | MOSimple_val (MOTuple_val vallist) -> 
      let var_types, rev_lstType = 
        List.fold_left 
          (fun (var_types, lstTyp ) aval -> 
            let var_types, res = 
              type_check_val_to_top fulloptiprog optiprog modi aval var_types in
            var_types, res::lstTyp
          ) 
          (var_types, [] ) vallist
      in var_types, MOTuple_type (List.rev(rev_lstType))
    | MOSimple_val (MOList_val vallist) -> 
      let var_types, res = List.fold_left 
        (fun (var_types, res_typ) aval -> 
          let var_types, res = 
            type_check_val_to_top fulloptiprog optiprog modi aval var_types 
          in
            var_types, determine_refine_type res_typ res
        )
        (var_types, (MOAll_type (get_fresh_int ()))) vallist
      in
      var_types, MOList_type res
    | MOSimple_val (MONone_val) -> 
      var_types, MOOption_type (MOAll_type (get_fresh_int ()))
    | MOSimple_val (MOSome_val v) -> 
      let var_types, res = type_check_val_to_top fulloptiprog optiprog modi v var_types 
      in
        var_types, MOOption_type (res)
    | MOSimple_val (MOSet_val setval) -> 
      let var_types, res = 
        MSet.fold
         (fun aval (var_types, res_typ) -> 
           let var_types, res = 
            type_check_val_to_top fulloptiprog optiprog modi (simple_to_core_val aval) var_types 
           in
             var_types, determine_refine_type res_typ res
         )
         setval (var_types, (MOAll_type (get_fresh_int ())))
      in var_types, MOSet_type res
    | MOSimple_val (MOMap_val mapval) -> 
      let var_types, key_res, res = 
        MMap.fold
        (fun key aval (var_types, key_res_typ , res_typ) -> 
          let var_types, key_res = type_check_val_to_top fulloptiprog optiprog
                                    modi (simple_to_core_val key) var_types 
          in
          let var_types, res = 
            type_check_val_to_top fulloptiprog optiprog modi aval var_types 
          in
            var_types, determine_refine_type key_res_typ  key_res ,
            determine_refine_type res_typ res
        )
        mapval (var_types, (MOAll_type (get_fresh_int ())), (MOAll_type (get_fresh_int ())))
      in
      var_types, MOMap_type (key_res, res)
    | MOSimple_val (MOFun_val fval) ->
      let lst_args = type_check_val_to_top_funargs fval.mofv_args_id var_types
      in
      let var_types, res = 
        type_check_val_to_top fulloptiprog optiprog modi fval.mofv_body var_types 
      in
        var_types, MOFun_type (lst_args, res)
    | MOSimple_val (MOEmpty_val) -> var_types, MOUnit_type
    | MOComposed_val comptype ->
        var_types, MOComposed_type (determine_type_composed fulloptiprog optiprog comptype)
    | MORef_val (refval, args) ->
      let var_types, rev_args_types = 
        List.fold_left 
          (fun (var_types, args_types) arg -> 
            let var_types, typ = 
              type_check_val_to_top fulloptiprog optiprog modi arg var_types
            in 
      debug_print(sprintf "function  has type args: %s \n"  (type_to_string typ));
            var_types, typ::args_types
          )
          (var_types, []) args
      in
      let ret,allTypeMap  = do_analyse_funcall var_types refval args (List.rev rev_args_types) in
      debug_print(sprintf "function  has type ret: %s \n"  (type_to_string ret));


      var_types, ret
    | MOEnvRef_val _ -> 
        var_types, MOBase_type (MTypeString)
    | MOBasicFunBody_val (op, arga, argb) -> 
        let var_types, typArga = 
          type_check_val_to_top fulloptiprog optiprog modi arga var_types in
        let var_types, typArgb = 
          type_check_val_to_top fulloptiprog optiprog modi argb var_types in
        var_types, determine_refine_type typArga typArgb 
    | MOBind_val bd -> 
      let var_types, bdval_typ = type_check_val_to_top fulloptiprog optiprog modi bd.mobd_value var_types in
      let var_types, bdbody_typ = type_check_val_to_top fulloptiprog optiprog modi bd.mobd_body var_types 
       in
      (*Enough?*)
      var_types, bdbody_typ
    | MOIf_val (cond, v1, v2) -> 
        let var_types, cond_typ = 
          type_check_val_to_top fulloptiprog optiprog modi cond var_types in 
        (*TODO: ok? *)
        let _typ = determine_refine_type cond_typ (MOBase_type (MTypeBool)) in
        let var_types, v1_typ = 
          type_check_val_to_top fulloptiprog optiprog modi v1 var_types in
        let var_types, v2_typ = 
          type_check_val_to_top fulloptiprog optiprog modi v2 var_types in
        var_types, determine_refine_type v1_typ v2_typ
    | MOComp_val (op, v1, v2) -> 
        let var_types, v1_typ = 
          type_check_val_to_top fulloptiprog optiprog modi v1 var_types in
        let var_types, v2_typ = 
          type_check_val_to_top fulloptiprog optiprog modi v2 var_types in
        let _typ = determine_refine_type v1_typ v2_typ in
        var_types, MOBase_type MTypeBool
    | MOBody_val (lst) -> 
        List.fold_left 
        (fun (var_types, _) elval -> 
          type_check_val_to_top fulloptiprog optiprog modi elval  var_types
        )
        (var_types, MOUnit_type) lst


let type_check_prog_to_top fulloptiprog optiprog modi var_types = 
  debug_info(debug_title3 (sprintf "Module %d\n" modi )) ;
  let var_types = 
    IntMap.fold
    (fun vari  varval var_types -> 
      match varval with 
        | MOTop_val aval -> 
            let var_types, typ = 
              type_check_val_to_top fulloptiprog optiprog modi aval var_types 
            in
            var_types
        | MOTupEl_val _  -> var_types
    )
    optiprog.mopg_topvar var_types
  in 
  let var_types, _  = 
    type_check_val_to_top fulloptiprog optiprog modi optiprog.mopg_topcal var_types
  in var_types

let type_check_bottom_to_top fulloptiprog var_types = 
  (*For every modules*)
  debug_info(debug_title2 "part4: bottom to top analysis");
  let var_types = 
  IntMap.fold
  (fun modi optimod var_types ->
    match optimod with
      | MOUserMod optiprog -> type_check_prog_to_top fulloptiprog optiprog modi var_types
      | MOSystemMod sysmod -> var_types
  )
   fulloptiprog.mofp_progmodules var_types 
  in
  type_check_prog_to_top fulloptiprog fulloptiprog.mofp_mainprog 0 var_types
    
    

(*END PART 4 *)





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
  (*First step, we compute toplevel var. It is done in one run (one
   * representation traversal) and sometimes the determined type is a reference
   * to another variable type.*)
  (*In fact : var_types does not only contains type of toplevel vars
  but of every vars: this is possible because each var (independantly from source
  file name, has an internal int id which is unique): this allow to have a global
  scope. *)
  debug_info (debug_title2 "part3: full type checking");
  let var_types = top_level_types fulloptiprog in 
  (*Second stop, we want to remove references type from the toplevels types.*)
  let var_types = top_level_types_no_ref var_types None in
  debug_to_level_type fulloptiprog var_types ;
  (*Third step, look inside the code*)
   type_check fulloptiprog var_types   ;
  (*PART 4 *)
  debug_info (debug_title2 "Part4: full type checking");
  let var_types = type_check_bottom_to_top fulloptiprog var_types in
  fulloptiprog, var_types

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
      mopg_topcal = empty_expr;
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
            (match StringMap.find_opt name map with
              | None -> StringMap.add name (get_fresh_int()) map
              | Some old_i -> 
                  let ni = get_fresh_int() in
                  past_map := IntMap.add ni old_i (!past_map); 
                  StringMap.add name ni map
            )
          in
          let rec add_topel el map = 
            match el with 
              | MBaseDecl aname -> add_topvar aname map
              | MTupDecl nlst -> List.fold_left (fun map el -> add_topel el map) map nlst
          in
          let topvar2int = 
            List.fold_left
                (fun map mvar -> add_topel mvar.mva_name map)
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
  let var_types = top_level_types nfulloptiprog in 
  (*PART 3 *)
  debug_print "dump var_types before top_level_types_no_ref\n";
  IntMap.iter
    (fun ivar typ -> debug_print (sprintf "\t\t %d : %s\n" ivar (type_to_string typ)))
    (IntMap.find 0 var_types);
  let var_types = top_level_types_no_ref var_types past_var_map in
  debug_print "dump var_types after top_level_types_no_ref\n";
  IntMap.iter
    (fun ivar typ -> debug_print (sprintf "\t\t %d : %s\n" ivar (type_to_string typ)))
    (IntMap.find 0 var_types);


  (*PART 3 *)
   debug_to_level_type nfulloptiprog var_types ;
  (*PART 3 *)
   type_check nfulloptiprog var_types   ;
  (*PART 4 *)
  debug_info(debug_title2 "PART 4");
  debug_print "dumping var_types at beginning of part 4";
  debug_var_type var_types;
  let var_types = type_check_bottom_to_top nfulloptiprog var_types in
  nfulloptiprog, var_types

  
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

(*   let  = search_modules moduleprog in *)
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
  let var_types = top_level_types fulloptiprog in 
  let var_types = top_level_types_no_ref var_types None in
  debug_to_level_type fulloptiprog var_types ;
   type_check fulloptiprog var_types   ;
  (*END PART 3*)
  (*PART 4 *)
  debug_info(debug_title2 "PART 4");
  let var_types = type_check_bottom_to_top fulloptiprog var_types in
  debug_print (sprintf "Dumping fulloptiprog modules after adding modules: %s \n"
          (fulloptiprogModules_to_string fulloptiprog));

  fulloptiprog, var_types

