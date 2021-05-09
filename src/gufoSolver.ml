open GufoParsed
open Gufo
open Gufo.MCore
open GufoParsedHelper
open Printf

open GenUtils
open GufoOptUtils


let debug_typeset imod ivar set = 
        TypeSet.iter
        (fun atype -> 
          debug_info (Printf.sprintf "%d : %d : %s (at %s)" imod ivar (type_to_string atype) (type_to_location_string atype))
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
    | MOAll_type i , _t1 -> 
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

(*extract resolved constraints from the set.
  A variable is resolved when:
    There is a single element in the typeSet and this element is not a
    MORef_type.
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

  (*First we check if there are ref to replace by real type*)
  let typeSet = 
    TypeSet.map
      (fun typ -> 
        match typ.loc_val with
          | MORef_type (modul, id, deep, args) ->
            let modul = 
              (match modul with
                | None -> modi
                | Some modul -> modul 
              )
            in 
            let atyp = IntMap.find_opt id (IntMap.find modul resolved_map) in
            (match atyp with 
              | None -> typ
              | Some typfound ->             
                  let typfound = get_type_at_deep typfound.loc_pos typfound deep in
                  (*Check how to handle linked args*)  
                  let new_typ = args_refine typfound.loc_pos typfound args in
                  {new_typ with loc_pos = typ.loc_pos}
            )

        | _ -> typ
      )
      typeSet
  in

  (*Then we see if we can merge type*)
  TypeSet.fold
    (fun typ newTypeSet ->
      TypeSet.fold
        (fun typToComp newTypeSet -> 
            let res = (determine_refine_type typToComp typ) in
            res
(*
            let newTypeSet = TypeSet.union newTypeSet res in
            newTypeSet
*)
        )
        typeSet newTypeSet 
        
    )
    typeSet TypeSet.empty


let resolve_free_variables unresolved_map = 
  IntMap.map
    (fun modul ->
      IntMap.map
        (fun typeSet ->
          TypeSet.map 
            (fun typeEl ->
              (match typeEl.loc_val with
                 | MORef_type _ ->
                     (*We consider it not to be resolved.*)
                    {typeEl with loc_val = MOAll_type (get_fresh_int ())}
                  | _ -> typeEl
               )
            )
            typeSet 
        )
        modul
    )
    unresolved_map

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
      | true -> (*the end :) *)
          resolved_map
      | false -> 
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
                let unresolved_map = resolve_free_variables unresolved_map in
                try_resolve resolved_map unresolved_map
  in
  try_resolve resolved_map unresolved_map
