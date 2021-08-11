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

(*TODO: Soit une variable de type fun refA -> refB, il faut que lorsque refA (et/ou refB) est résolu, elle soit elle même transformé en fun resTyp -> resTyp2 *)

open GufoParsed
open Gufo
open Gufo.MCore
open GufoParsedHelper
open Printf

open GenUtils
open GufoOptUtils
open GufoSolverUtils


let debug_typeset_poor set = 
        TypeSet.iter
        (fun atype -> 
          debug_info (Printf.sprintf "type %s (at %s)" (type_to_string atype) (type_to_location_string atype))
        )
        set



let debug_typeset imod ivar set = 
        debug_info (Printf.sprintf "%d : %d : " imod ivar);
        TypeSet.iter
        (fun atype -> 
          debug_info (Printf.sprintf "    %d : %d : %s (at %s)" imod ivar (type_to_string atype) (type_to_location_string atype))
        )
        set


let debug_type imod ivar atype = 
          debug_info (Printf.sprintf "res %d : %d : %s " imod ivar (type_to_string atype))


let debug_constraints constraints = 
  debug_info(debug_title2 "Dumping constraints\n");
  IntMap.iter
    (fun imod modul->
      IntMap.iter
      (fun ivar atypeSet ->debug_typeset imod ivar atypeSet
      )
      modul
  )
  constraints 

let debug_res_constraints constraints = 
  debug_info(debug_title2 "Dumping resolved constraints\n");
  IntMap.iter
    (fun imod modul->
      IntMap.iter
      (fun ivar atype -> debug_type imod ivar atype
      )
      modul
  )
  constraints 




let determine_refine_base_type epos i j = 
  let typ = 
    match i, j with 
      | MTypeString, MTypeString -> MTypeString
      | MTypeBool, MTypeBool -> MTypeBool
      | MTypeInt, MTypeInt -> MTypeInt
      | MTypeFloat, MTypeFloat -> MTypeFloat
      | MTypeCmd, MTypeCmd -> MTypeCmd
      | _, _ -> raise_typeError 
                  (sprintf "Cannot merge together type %s and type %s ." 
                    (basetype_to_str i) (basetype_to_str j)) epos
  in 
  TypeSet.singleton {loc_val = (MOBase_type typ); loc_pos = epos}


let determine_refine_type_composed epos i j = 
  match i.moct_name - j.moct_name with
    | 0 -> i
    | _ -> raise_typeError (sprintf "Cannot merge type %d and %d together (TODO: give source name) (heritage not implemented))" i.moct_name j.moct_name) epos



(*This function try to refine between two types:
  There is three possibility;
    - Types are clearly incompatible (ex: int and string) 
        --> we raise a type exception.
    - Types cannot be merged : we return a set of the two types
    - Types are clearly compatible : (a reference to an int and an int) 
        --> we return a set of 1 element (the merged type)

    Type should rarely (never) be equal because they come from a set, but we
    write this possibility.

*)
let rec determine_refine_type t1 t2 = 
  let compare_list ilst jlst = 
    (match (List.length ilst - List.length jlst ) with 
      | 0 -> 
        let lst_determine = 
          List.map2
            (fun i j -> 
               determine_refine_type i j
            )
          ilst jlst
        in
        let eq = 
           (List.for_all
           (fun det -> 
             (TypeSet.cardinal det) == 1
           )
           lst_determine)
        in
        (match eq with 
          | true ->
            Some (List.map2 
              (fun i j -> 
                TypeSet.choose (determine_refine_type i j)
              )
              ilst jlst);
          | _ -> None
         )
      | _ -> None
    )
  in
  match t1.loc_val, t2.loc_val with
    | _t1, MOAll_type i ->
        TypeSet.singleton t1 
    | MOAll_type i , _t2 -> 
        TypeSet.singleton t2 
    | MORef_type (imod, i, deep,args ), _ -> 
      TypeSet.add t1 (TypeSet.singleton t2)
    | _, MORef_type (imod, i, deep,args ) -> 
      TypeSet.add t1 (TypeSet.singleton t2)
    | MOUnit_type, MOUnit_type -> 
        TypeSet.singleton t1
    | MOUnit_type, _ -> 
        raise_typeError  "cannot refine unit with other type" t1.loc_pos
    | MOComposed_type i , MOComposed_type j -> 
        TypeSet.singleton 
          {loc_val = MOComposed_type 
                      (determine_refine_type_composed t1.loc_pos i j );
           loc_pos = t1.loc_pos
          }
    | MOBase_type i , MOBase_type j  -> 
        determine_refine_base_type t1.loc_pos i j 
    | MOTuple_type ilst , MOTuple_type jlst  ->
        (match (List.length ilst - List.length jlst ) with 
          | 0 -> 
            (match (compare_list ilst jlst) with
              | None -> TypeSet.add t1 (TypeSet.singleton t2)
              | Some mlst -> 
                 TypeSet.singleton {t1 with loc_val = (MOTuple_type mlst)}
            )
          | _ -> raise_typeError 
                  "Cannot merge together tuple types of different lengths."
                  t1.loc_pos
        )
    | MOList_type i , MOList_type j  -> 
        let resdet = (determine_refine_type i j) in
        (match (TypeSet.cardinal resdet ) == 1 with
          | true -> 
              TypeSet.singleton {
                t1 with loc_val = MOList_type (TypeSet.choose resdet)
              }
          | false -> TypeSet.add t1 (TypeSet.singleton t2)
        )
    | MOOption_type i, MOOption_type j -> 
        let resdet = (determine_refine_type i j) in
        (match (TypeSet.cardinal resdet ) == 1 with
          | true -> 
            TypeSet.singleton 
              {t1 with loc_val = MOOption_type(TypeSet.choose resdet)}
          | false -> TypeSet.add t1 (TypeSet.singleton t2)
        )
    | MOSet_type i, MOSet_type j ->
        let resdet = (determine_refine_type i j) in
        (match (TypeSet.cardinal resdet ) == 1 with
          | true -> 
            TypeSet.singleton 
              {t1 with loc_val = MOSet_type(TypeSet.choose resdet)}
          | false -> TypeSet.add t1 (TypeSet.singleton t2)
        )
    | MOMap_type (k,v), MOMap_type(kp,vp) ->
        let resk = (determine_refine_type k kp) in
        let resv = (determine_refine_type v vp) in
        (match ((TypeSet.cardinal resk ) == 1) &&  
              ((TypeSet.cardinal resv ) == 1)
        with
          | true -> 
            TypeSet.singleton {t1 with loc_val = MOMap_type((TypeSet.choose resk),
                                               ((TypeSet.choose resv)))}
          | false -> TypeSet.add t1 (TypeSet.singleton t2)
        )
    | MOFun_type(aargs, aret), MOFun_type(bargs, bret) -> 
        (match (List.length aargs - List.length bargs ) with 
          | 0 -> 
            (match (compare_list aargs bargs ) with
              | None -> TypeSet.add t1 (TypeSet.singleton t2)
              | Some mlst -> 
                  let resret = (determine_refine_type aret bret) in
                  (match (TypeSet.cardinal resret) == 1 with
                    | true -> 
                      TypeSet.singleton 
                        {t1 with loc_val = (MOFun_type (mlst, TypeSet.choose resret))}
                    | false -> TypeSet.add t1 (TypeSet.singleton t2)
                  )
            )
          | _ -> raise_typeError 
                  "Cannot merge together function types of different lengths."
                  t1.loc_pos
        )
    | _,_ ->
        let msg = (sprintf "Cannot infer correct type between %s and %s " 
                    (type_to_string t1) (type_to_string t2)) in
        raise_typeError msg t1.loc_pos 

  (*This function is triggered when the reference (rmodul, rvarid) has been
resolved to resolved_type and allow to transfort in the unresolved map
reference to this variable to its direct type.*)
let update_on_ref_resolution unresolved_map rmodul rvarid resolved_type =
  let rec refine_with_args typ args_ref = 
    match typ, args_ref with
      | MOFun_type([], ret_fun), [] ->
          ret_fun.loc_val
      | MOFun_type(cur_fun_arg::funargs, ret_fun), cur_arg::args_ref ->
          (*We have to check the args compatibility.*)
          let arg_type = determine_refine_type cur_arg  cur_fun_arg in
          refine_with_args (MOFun_type(funargs, ret_fun)) args_ref
      | _,_ -> typ 
  in
  
  let rec update_typ typ = 
    (match typ.loc_val with
     | MORef_type (refmod, refi, _,args) ->
        let refmod = match refmod with 
            | None -> 0
            | Some i -> i 
        in
        (match refmod - rmodul, refi - rvarid with
          | 0, 0 -> 
               (*we have to refine the type depending of the arguments*)  
              let base_type = refine_with_args resolved_type.loc_val args in
              {typ with loc_val = base_type} 
          | _ ->
                typ
        )
     | MOFun_type (args, ret) ->
          let newArgs = 
            List.map  
              (fun arg -> 
                update_typ arg
              )
              args
          in
          let newRet = update_typ ret in
          {typ with loc_val = MOFun_type (newArgs, newRet)}
     | _ -> typ
    )
  in
  IntMap.mapi
  (fun idmod modul ->
    IntMap.mapi
      (fun idvar typeSet ->
        TypeSet.map 
          (fun typeEl -> update_typ typeEl
          )
          typeSet 
      )
      modul
    )
    unresolved_map

(*extract resolved constraints from the set.
  A variable is resolved when:
    - There is a single element in the typeSet and this element is not a
    MORef_type.
    - There is a single 'concrete' type (neither MOAll, neither a ref) in the
    typeset and the eventuel ref types can be converted to this type.
*)
let select_resolved resolved_map unresolved_map =
  let change = ref false in
  let add_in_module imodul varId elToAdd map = 
    match IntMap.find_opt imodul map with
      | None -> 
          IntMap.add imodul (IntMap.add varId elToAdd IntMap.empty) map
      | Some mapEl ->
          IntMap.add imodul (IntMap.add varId elToAdd mapEl) map
  in

  let resolved_map, unresolved_map = 
    IntMap.fold
      (fun imodul modul (resolveds, unresolveds) ->
        IntMap.fold
          (fun varid typeSet (resolveds, unresolveds) ->
            match TypeSet.cardinal typeSet with
              | 1 ->  
                (match (TypeSet.choose typeSet).loc_val with
                  | MORef_type _ ->
                      (*We consider it not to be resolved.*)
                     (resolveds, 
                       add_in_module imodul varid typeSet unresolveds
                     )
                  | _ -> 
                    change := true;
                    (*When we resolve a ref, we should update every place where
                      it is used.*)
                    let resolved_type = (TypeSet.choose typeSet) in
                    let unresolveds = update_on_ref_resolution unresolveds
                                        imodul varid resolved_type in
                    (add_in_module imodul varid (TypeSet.choose typeSet) resolveds), 
                    unresolveds
                )
              | _ -> 
                      (resolveds, 
                       add_in_module imodul varid typeSet unresolveds
                     )
          )
          modul (resolveds, unresolveds )
      )
      unresolved_map (resolved_map, IntMap.empty)
  in !change, resolved_map, unresolved_map

let resolve modi vari resolved_map typeSet =

  let rec args_refine epos ref_typ args_typ = 
    match args_typ with
      | [] -> 
          ref_typ 
      | typ :: args_typ -> 
          (match ref_typ.loc_val with 
            | MOFun_type ([refarg], refret) ->
                args_refine epos refret [] 
            | MOFun_type (refarg:: refargs , refret) ->
                args_refine epos 
                            ({ref_typ with loc_val = MOFun_type (refargs, refret)}) 
                            args_typ 
            | _ -> 
                raise_typeError 
                  "Type checking error"
                  epos
          )
  in

  let ref_to_real_type refModul refId refDeep refArgs curTyp =
    let modul = 
      (match refModul with
        | None -> modi
        | Some modul -> modul 
      )
    in 
    let atyp = 
      match IntMap.find_opt modul resolved_map with
        | None -> None
        | Some modul -> IntMap.find_opt refId modul 
    in            
    (match atyp with 
      | None -> curTyp
      | Some typfound ->
          let typfound = get_type_at_deep typfound.loc_pos typfound refDeep in
          (*Check how to handle linked args*)  
          let new_typ = args_refine typfound.loc_pos typfound refArgs in
                  {new_typ with loc_pos = curTyp.loc_pos}
    )
  in

  (*First we check if there are ref to replace by real type*)
  let typeSet = 
    TypeSet.map
      (fun typ -> 
        match typ.loc_val with
          | MORef_type (modul, id, deep, args) ->
              ref_to_real_type modul id deep args typ
          | MOFun_type (argsList, ret) ->
              let newArgsList = 
                List.map
                  (fun arg -> 
                    (match arg.loc_val with 
                      | MORef_type (modul, id, deep, args) ->
                          ref_to_real_type modul id deep args arg
                      | _ -> arg
                    )
                  )
                  argsList
              in
              let newRet = 
                  (match ret.loc_val with 
                      | MORef_type (modul, id, deep, args) ->
                          ref_to_real_type modul id deep args ret
                      | _ -> ret
                  )
              in
                {typ with loc_val = MOFun_type(newArgsList, newRet)}
        | _ -> typ
      )
      typeSet
  in

  (*Then we see if we can merge type*)

  debug_info "*********** Type merger *********** \n";
  debug_info "Set is\n";
  debug_typeset_poor typeSet;
  
  let resSet = 
    let accTypeSet = TypeSet.singleton (TypeSet.choose typeSet) in
    TypeSet.fold
      (fun typ accTypeSet ->
        TypeSet.fold
          (fun typToComp accTypeSet -> 
            let res = (determine_refine_type typToComp typ) in
            TypeSet.fold
              (fun resEl accTypeSet ->
                TypeSet.add resEl accTypeSet
              )
              res accTypeSet
          )
          accTypeSet accTypeSet
      )
      typeSet accTypeSet
  in 
  debug_info "result set is:\n";
  debug_typeset_poor resSet; resSet



(*We trigger this function only when we are blocked (there are still variables
to resolved but we the last round did not allow to resolve variables.*)
let resolve_free_variables unresolved_map = 
  (*************** RULE 1 ***************
    Rule 1: Resolve MOAll singleton:
      - if a variable A has no constraint or a single constraint MOAll
      - A variable B has two or more constraint such as
          * B --> A
            B --> OtherType
            B --> OtherOtherType
      then 
        A can be resolved to OtherType
*)
  let rule1 unresolved_map =
    (*do_change track if this rule created at least one change. *)
    let do_change = ref false in
      (*We find every MOall singleton constraints *)
    let moallSingletonConsts = 
      let filter_MoAll idmodul idvar typeSet = 
        (match (TypeSet.cardinal typeSet = 1) with
          | false -> false
          | true -> 
              (match (TypeSet.choose typeSet).loc_val with
                | MORef_type (imod, ivar, _, _ ) -> 
                    let imod = match imod with
                      | None -> 0 
                      | Some i -> i
                    in
                    (match imod - idmodul, ivar - idvar with
                      | 0 , 0 -> true
                      | _  -> false
                    )
                | MOAll_type iall -> true
                | _ -> false
              )
        )
       in 
       filters_constraint_from_map filter_MoAll unresolved_map
    in
    let unresolved_map = 
    (*For each singleton constraint moallSingletonConsts *)
    List.fold_left
      (fun unresolved_map aSingletonConst ->
        (*We check if they are used in at least one other constraint *)
        let filter_use_singlConst _idmodul _idvar typeSet =
          (match (TypeSet.cardinal typeSet > 1) with
            | false -> false
            | true -> 
              let sing_idmodul, sing_idvar = aSingletonConst in
              TypeSet.exists 
                (fun loctype ->
                  match loctype.loc_val with
                    | MORef_type (modOpt, idref, deep, args) ->
                        let modRef = (match modOpt with 
                                       | None -> 0
                                       | Some i -> i
                                     )
                        in 
                        (match modRef - sing_idmodul, sing_idvar - idref with
                          | 0, 0 -> 
                              true
                          | _,_ -> false
                        )
                    | _ -> false
                ) 
                typeSet
          )
        in
        let lst_use_singl_const = 
          filters_constraint_from_map filter_use_singlConst unresolved_map in
        (*For every place where the constraint is used, retourne un nouveau type.*)
        let newTypeSing =
          List.fold_left 
            (fun newTypeSing modulvarid ->
              (match newTypeSing with
                | Some _ -> newTypeSing (*newTypeSing already found, we do not try more*)
                | None -> (*newTypeSing not found, we search*)
                  let typeset = get_el_from_modulVarId unresolved_map modulvarid in
                    let tryNewType = 
                      TypeSet.find_first_opt
                        (fun typeloc ->
                          match typeloc.loc_val with
                            | MORef_type (_,_,_,_) -> false 
                            | MOAll_type i  -> false 
                            | _ -> true
                        )
                        typeset
                    in 
                    tryNewType
              )
            )
            (None)
            lst_use_singl_const
        in 
          (*Replace in unresolved_map, aSingletonConst by newTypeSing*)
          match newTypeSing with
            | None -> unresolved_map
            | Some newTypeSing ->
              do_change:= true;
              replace_in_map unresolved_map aSingletonConst (TypeSet.singleton newTypeSing)
          
      )
      unresolved_map
      moallSingletonConsts
    in
      unresolved_map, !do_change
    
  in 

  (*************** RULE 2 ****************)

  let rule2 unresolved_map =
    (*transform unresolved ref to MOAll_type .*)
    (*do_change track if this rule created at least one change. 
      TODO: maybe it should be allowed only if ref is used in no other
      unresolved constraints.
    *)
    let do_change = ref false in
    IntMap.mapi
      (fun idmod modul ->
        IntMap.mapi
          (fun idvar typeSet ->
            TypeSet.map 
              (fun typeEl ->
                (match typeEl.loc_val with
                   | MORef_type (refmod, refi, _,_) ->
                        let refmod =
                          (match refmod with
                            | None -> 0
                            | Some i -> i
                          )
                        in
                       (match (refmod - idmod, refi - idvar) with
                        | 0, 0 -> 
                           (*We consider it not to be resolved.*)
                          do_change := true;
                          debug_info (Printf.sprintf "Rule 2 transformation for %d[%d]\n" idvar idmod);
                          {typeEl with loc_val = MOAll_type (get_fresh_int ())}
                        | _ -> typeEl
                       )
                    | _ -> typeEl
                 )
              )
              typeSet 
          )
          modul
      )
      unresolved_map, !do_change
  in 


  let unresolved_map, change = rule1 unresolved_map in
  match change with 
    | true ->  unresolved_map
    | false -> let unresolved_map, _ = rule2 unresolved_map in unresolved_map

(*Return a constraint map with only a single resolved type for a variable
 constraints: Gufo.MCore.TypeSet.t GenUtils.IntMap.t GenUtils.IntMap.t
*)
let solver constraints =
(*In a first step, we resolve every var which have a single constraint (so are resolved).
  We keep a map of thouse resolved vars.
*)
  let _, resolved_map, unresolved_map = select_resolved IntMap.empty constraints in

(*
  In a loop, we try to resolve every var, which can be solved using their
constraints and the map of resolved vars.
*)
  let rec try_resolve resolved_map unresolved_map = 
    match IntMap.is_empty unresolved_map with
      | true -> (*All variables resolved, the end :) *)
          debug_res_constraints resolved_map;
          resolved_map
      | false -> (*Still variables to be resolved*)
          let unresolved_map = 
            IntMap.mapi
              (fun modi modconstraints ->
                IntMap.mapi 
                (fun varid typeSet ->
                  resolve modi varid resolved_map typeSet
                )
                modconstraints
              )
              unresolved_map
          in
          let change, resolved_map, unresolved_map = 
            select_resolved resolved_map unresolved_map 
          in
          match change with 
            | true ->
              try_resolve resolved_map unresolved_map
            | false ->
                (*This case means, we have still unresolved vars but where not
                  able te resolve one in the last run.
                  This can happen when there are "free references", references
                  which are not linked to a type (for exemple, a function
                  argument ( fun $f $a = $a + 5 , in this case $a is not
                  explicitely resolved.))
                *)
                debug_constraints unresolved_map;
                let unresolved_map = resolve_free_variables unresolved_map in
                debug_constraints unresolved_map;
                try_resolve resolved_map unresolved_map
  in
  try_resolve resolved_map unresolved_map
