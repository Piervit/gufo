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

(* The main language parser. *)

(*The compilation give a few conflicts:

Warning: 3 states have shift/reduce conflicts.
Warning: one state has reduce/reduce conflicts.
Warning: 3 shift/reduce conflicts were arbitrarily resolved.
Warning: 47 reduce/reduce conflicts were arbitrarily resolved.

They should be all about the '*' and '.' symbol.

This is because '*' is used for both '5 * 5' and 'ls *'.
And because '.' is used for variables/modules separation and for 'ls .''

Commenting the cmd_arg usage of STAR and DOT will solve the conflict but create
issue for commands such as 'ls *'.

*)

%{

  open GufoParsed

%}

%token STRUCT 
%token LET
%token FUN
(* %token JOKER (*_*) *)
%token COLON (* : *)
%token AFFECTATION (* = *)
%token TRUE 
%token FALSE
(* pattern matching *)
%token ARROW (* -> *)
%token WITH
%token WITH_SET 
%token WITH_MAP 
%token WITHOUT_SET
%token WITHOUT_MAP
(* file/dir shortcut *)
%token TILDE(* userdir *)
(* command rilated *)
%token CLOSING_CHEVRON (* > *)
%token EQ_CLOSING_CHEVRON (* >= *)
%token MINUS_CLOSING_CHEVRON 
%token DOUBLE_CLOSING_CHEVRON (* >> *)
%token WRITE_ERROR_TO (* 2> *)
%token WRITE_ERROR_NEXT_TO (* 2>> *)
%token WRITE_ERROR_TO_STD (* 2>&1 *)
%token WRITE_ALL_TO (* >& *)
%token OPENING_CHEVRON 
%token EQ_OPENING_CHEVRON 
%token MINUS_OPENING_CHEVRON 
%token PIPE
%token SIMPLE_AND
%token AND
%token OR
%token <string> ARG 
%token <string> FILE
(* %token <string> CMDARG *)
(* mathematic *)
%token PLUS
%token PLUS_DOT
%token PLUS_STR
%token MINUS
%token MINUS_DOT
%token DIVISION
%token DIVISION_DOT
%token STAR
%token STAR_DOT
%token MODULO
%token MODULO_DOT
%token EQUALITY (* == *)
%token INEQUALITY (* != *)
(*
%token GREATER_THAN (* gt *)
%token GREATER_OR_EQUAL (* gte *)
%token LOWER_THAN (* lt *)
%token LOWER_OR_EQUAL (* lte *)
*)
(* array rilated *)
%token OPEN_SQRBRACKET (* [ *)
%token OPEN_SQRIDXBRACKET (* [ *)
%token CLOSE_SQRBRACKET  (* ] *)
(* condition *)
%token IF
%token THEN
%token ELSE
(* type *)
%token INTTYPE 
%token FLOATTYPE
%token STRINGTYPE
%token LISTTYPE
%token SETTYPE
%token MAPTYPE
%token OPTIONTYPE
%token EXTENDS
%token IN
%token SHAS
%token MHAS
%token BOOLTYPE
%token CMDTYPE
%token <int> INT
%token <float> FLOAT 
%token <string>STRING 
(* others *)
%token OPEN_BRACE
%token CLOSE_BRACE
%token OPEN_BRACKET
%token CLOSE_BRACKET
%token SEMICOLON (* ; *)
%token <string> FREETYPE
%token <string> VARNAME 
%token <string> ENVVAR
%token <string> VARFIELD
%token <string> MODULVAR
%token <string> MODUL
%token <string> WORD 
%token DOUBLE_MINUS

%token DOT
%token NONE
%token SOME
%token START
%token COMMA
%token EOF

%token DOUBLE_SEMICOLON (* ;; *)
%left DOUBLE_SEMICOLON

%left PLUS
%left MINUS
%left STAR, DIVISION
%left MODULO
%left WITH
%left WITHOUT
%left SHAS
%left MHAS
%left PLUS_STR
%left PLUS_DOT
%left STAR_DOT, DIVISION_DOT
%left MODULO_DOT
%left WITH_SET
%left WITH_MAP
%left WITHOUT_SET
%left WITHOUT_MAP

%left SEMICOLON
%left SIMPLE_AND
%left AND
%left OR
%left PIPE


%right ELSE
(*
%left LOWER_THAN
%left LOWER_OR_EQUAL
%left GREATER_THAN
%left GREATER_OR_EQUAL
*)
%left TILDE


%start <GufoParsed.mprogram option> prog
%start <GufoParsed.mprogram option> shell
%%

(**
 ##################### MAIN LANGUAGE PARSER #####################
*)

shell:
    |  EOF
    {   
      Some {mpg_types = GenUtils.StringMap.empty; mpg_topvar = []; 
            mpg_topcal = {
                           loc_pos = GufoParsedHelper.dummy_position ; 
                           loc_val = MSimple_val(MEmpty_val);
                         }
           }
    }
    |  main_expr = located(topvarassign); EOF
    {   
      Some {mpg_types = GenUtils.StringMap.empty; mpg_topvar = []; mpg_topcal = main_expr} 
    }
    | LET ; varnames = located(var_tuple_decl); argnames = located(funargs_top) ; AFFECTATION; funbody = located(topvarassign); EOF
	{
          let open GenUtils in 
          (match argnames.loc_val, varnames.loc_val with
            | [], _ -> 
              let mvar = {mva_name = varnames; mva_value = funbody} in
              Some ({mpg_types = GenUtils.StringMap.empty; mpg_topvar = [mvar];  
                     mpg_topcal = {
                           loc_pos = GufoParsedHelper.dummy_position ; 
                           loc_val = MSimple_val(MEmpty_val);
                         }
                   })
            | args, MBaseDecl varname ->
                let mvar = 
                  {
                    mva_name = varnames; 
                    mva_value = 
                    {
                      funbody with 
                      loc_val = MSimple_val (
                                  MFun_val ({argnames with loc_val = List.rev args},
                                             funbody))
                    }
                  } 
                in
                Some ({mpg_types = GenUtils.StringMap.empty; mpg_topvar = [mvar];  
                       mpg_topcal = {
                           loc_pos = GufoParsedHelper.dummy_position ; 
                           loc_val = MSimple_val(MEmpty_val);
                         }
                      })
            | argnames, MTupDecl _ -> 
                (*TODO: improve error message *)
                raise (VarError "Error, impossible to gives arguments to tuple element of a let declaration. ")
          )
      	}
    |  STRUCT; name = VARNAME; AFFECTATION; OPEN_BRACE ; fields_decl = fields_decl; CLOSE_BRACE
	{
          let open GenUtils in 
          let (typ, internal_val) = fields_decl in
          let name = rm_first_char name in
          let ctyp = 
                  (MComposed_type 
                              {mct_name = name; 
                               mct_fields = typ; 
                               mct_internal_val = internal_val})
          in
          Some {mpg_types = StringMap.singleton name ctyp; mpg_topvar = []; 
                mpg_topcal = {
                           loc_pos = GufoParsedHelper.dummy_position ; 
                           loc_val = MSimple_val(MEmpty_val);
                         }
                }
	}




prog:
    | p = mfile{ Some p }
;


mfile:
  |  topels = mtopels; EOF
    {
      let (types, variables) = topels in
      {mpg_types = types; mpg_topvar = variables;  
       mpg_topcal = {
         loc_pos = GufoParsedHelper.dummy_position ; 
         loc_val = MSimple_val(MEmpty_val);
         }
      }
    }
  |  topels = mtopels; START; main_expr = located(topvarassign);EOF
    {   
      let (types, variables) = topels in
      {mpg_types = types; mpg_topvar = variables; mpg_topcal = main_expr} 
    }


mtopels: 
  |topels = rev_mtypes_or_topvals {topels};


var_tuple_decl:
  | simple = base_var_tuple_decl;
    {simple}
  | left = base_var_tuple_decl; DOUBLE_MINUS ;right = var_tuple_decl ;
    {
     
      match left,right with
        | MBaseDecl lf, MBaseDecl rg -> MTupDecl [MBaseDecl lf;MBaseDecl rg]
        | MBaseDecl lf, MTupDecl rg -> MTupDecl ((MBaseDecl lf) :: rg)
        | MTupDecl lf,MBaseDecl rg ->  MTupDecl (List.rev(MBaseDecl rg :: (List.rev lf)))
        | MTupDecl lf, right ->  MTupDecl (List.rev(right :: (List.rev lf)))
    }
  | OPEN_BRACKET; decl = var_tuple_decl ; CLOSE_BRACKET
    {
     
     MTupDecl [decl]
    }

base_var_tuple_decl:
  | name = located(VARNAME); 
    {let open GenUtils in 
      MBaseDecl ({name with loc_val =  rm_first_char  name.loc_val })
    }

rev_mtypes_or_topvals:
  | { GenUtils.StringMap.empty,[] }
  | topels= rev_mtypes_or_topvals; STRUCT; name = VARNAME; AFFECTATION; OPEN_BRACE ; fields_decl = fields_decl; CLOSE_BRACE
	{
          let open GenUtils in 
	  let (types, topvals) = topels in
          let (typ, internal_val) = fields_decl in
          let name = rm_first_char name in
          match StringMap.mem name types with
            | true -> 
                raise (TypeError {loc_val = ("The type "^name^" is already declared."); 
                                  loc_pos = GufoParsedHelper.dummy_position;  
                                }
                      )
            | false ->
                ( GenUtils.StringMap.add name
                  (MComposed_type 
                              {mct_name = name; 
                               mct_fields = typ; 
                               mct_internal_val = internal_val})
                  types , topvals)
	}

  (*variables and function assignation*)

  | topels= rev_mtypes_or_topvals; LET ; varnames = located(var_tuple_decl); argnames = located(funargs_top) ; AFFECTATION; funbody = located(topvarassign);
	{
           
          let open GenUtils in 
      	  let (types, topvals) = topels in
          (match argnames.loc_val, varnames.loc_val with
            | [], _ -> 
              let mvar = {mva_name = varnames; mva_value = funbody} in
      	      (  types ,  mvar :: topvals )
            | args, MBaseDecl varname ->
                let mvar = 
                  {
                    mva_name = varnames; 
                    mva_value = {funbody with 
                                  loc_val = MSimple_val (
                                              MFun_val ({argnames with loc_val = List.rev args}, 
                                                         funbody))}
                  } in
      	      (  types ,  mvar :: topvals )
            | argnames, MTupDecl _ -> 
                (*TODO: improve error message *)
                raise (VarError "Error, impossible to gives arguments to tuple element of a let declaration. ")
          )
      	}

(* TYPE PARSING*)

fields_decl: 
  fields_internalfun = rev_fields_decl 
  {
  let (fields, internalfun) = fields_internalfun in
  (List.rev fields, List.rev internalfun )
  };

rev_fields_decl:
  | { [],[] }
  | vars = rev_fields_decl; varname = located(WORD) ; COLON; typename = toptypedecl ; COMMA
    { 
      let lst_field, lst_val = vars in
      {mtf_name = varname; mtf_type = typename; mtf_extend = None} :: lst_field, lst_val }
  | vars = rev_fields_decl; EXTENDS; fieldname= located(modulVar); COMMA
    { 
      let lst_field, lst_val = vars in
      let fieldnameStr = ref_to_string fieldname.loc_val in 
      { mtf_name = {loc_val = (String.concat "_" ["ext"; fieldnameStr]); 
                    loc_pos = fieldname.loc_pos};
        mtf_type = (MRef_type fieldname.loc_val);
        mtf_extend = Some fieldnameStr;
      } :: lst_field, 
      lst_val 
    }
  | vars = rev_fields_decl; EXTENDS; fieldname= located(modulVar); WITH; anofun = anonymousfun; COMMA
    { 
      let lst_field, lst_val = vars in
      let fieldnameStr = ref_to_string fieldname.loc_val in 
      {mtf_name = {fieldname with loc_val = String.concat "_" ["ext"; fieldnameStr];} ;
       mtf_type = (MRef_type fieldname.loc_val);
       mtf_extend = Some fieldnameStr
      } :: lst_field , 
     (String.concat "_" ["extfun"; fieldnameStr], anofun) ::lst_val }
  ;

toptypedecl:
  | ft = FREETYPE;
    {MAll_type ft }
  | STRINGTYPE
    { MBase_type MTypeString }
  | INTTYPE
    { MBase_type MTypeInt }
  | FLOATTYPE
    { MBase_type MTypeFloat }
  | BOOLTYPE
    { MBase_type MTypeBool }
  | CMDTYPE
    { MBase_type MTypeCmd }
  | internal_type = typedecl; LISTTYPE
    { MList_type internal_type }
  | args = funargdecl ; 
    { 
      let rev_args = List.rev args in
      let ret_type = List.hd rev_args in
      let args = List.rev (List.tl rev_args) in
      MFun_type (args, ret_type) }
  | name = modulVar
    { MRef_type name }
  | first_tupel = typedecl ; DOUBLE_MINUS; tupel_suite= typetupelseq;
    { MTuple_type (first_tupel:: tupel_suite)}
  | tdec = typedecl; OPTIONTYPE; 
    {MOption_type tdec}
  | tdec = typedecl ;SETTYPE
    { MSet_type tdec }
  | OPEN_BRACKET; tdec = typedecl; COMMA; tdec2 = typedecl; CLOSE_BRACKET ;MAPTYPE
    { MMap_type (tdec, tdec2)}


typedecl:
  | ft = FREETYPE;
    {MAll_type ft }
  | STRINGTYPE
    { MBase_type MTypeString }
  | INTTYPE
    { MBase_type MTypeInt }
  | FLOATTYPE
    { MBase_type MTypeFloat }
  | BOOLTYPE
    { MBase_type MTypeBool }
  | CMDTYPE
    { MBase_type MTypeCmd }
  | internal_type = typedecl; LISTTYPE
    { MList_type internal_type }
  | name = modulVar
    {MRef_type name}
  | OPEN_BRACKET; typ = toptypedecl ; CLOSE_BRACKET;
    { typ }
  ;

funargdecl:
  | arg_type=typedecl ; ARROW; args = rev_funarinnergdecl;
    {arg_type :: args }
  ; 

rev_funarinnergdecl:
  | arg_type = typedecl ; 
    {[ arg_type ] }
  | arg_type = typedecl ; ARROW ; args= rev_funarinnergdecl;
    {arg_type :: args }
  ;

typetupelseq :
  | el = typedecl;
    { [el] }
  | el = typedecl; DOUBLE_MINUS; seq = typetupelseq
    { el:: seq }


leaf_expr: 
  | NONE
    {MSimple_val (MNone_val)}
  | i = located(INT)
    {MSimple_val (MBase_val (MTypeIntVal i))}
  | s = located(STRING)
    {MSimple_val (MBase_val (MTypeStringVal s))}

  | f = located(FALSE)
    {MSimple_val (MBase_val (MTypeBoolVal 
        {
          loc_val = false;
          loc_pos = f.loc_pos;
        }))}
  | t = located (TRUE)
    {MSimple_val (MBase_val (MTypeBoolVal 
        {
          loc_val = true;
          loc_pos = t.loc_pos;
        }))}
  | f = located (FLOAT)
    {MSimple_val (MBase_val (MTypeFloatVal f))}
  | MINUS_OPENING_CHEVRON; set = located(listSetEl); MINUS_CLOSING_CHEVRON
    {
      MSimple_val (MSet_val set)
    }
  | start = located(MINUS_OPENING_CHEVRON);  MINUS_CLOSING_CHEVRON
    {
      MSimple_val (MSet_val {loc_val = []; loc_pos = start.loc_pos})
    }

  | start = located (MINUS_OPENING_CHEVRON); COLON;   MINUS_CLOSING_CHEVRON
    {
      MSimple_val (MMap_val {loc_val = []; loc_pos = start.loc_pos })
    }
  | start = located (MINUS_OPENING_CHEVRON); map = mapEl; fin = located (MINUS_CLOSING_CHEVRON)
    {
      MSimple_val (MMap_val ({loc_val = map; 
                              loc_pos = GufoLocHelper.pos_merging start.loc_pos 
                                                                  fin.loc_pos 
                             }))
    }
  |  cmdas = located(cmd_expr);
    { MSimple_val (MBase_val (MTypeCmdVal cmdas)) }
  |  anonf = anonymousfun ; 
    {anonf}

basic_expr:
  | res = leaf_expr 
    { res }
  | LET ; binding_name = var_tuple_decl ; argnames = located(funargs_top) ; AFFECTATION ; binding_value = located(topvarassign); IN ; OPEN_BRACKET; body = located(topvarassign); CLOSE_BRACKET;
    { 
     let open GenUtils in
      MBind_val {mbd_name = binding_name; 
                 mbd_value = 
                    (match argnames.loc_val with 
                      | [] -> binding_value
                      | lstargs -> 
                          { binding_value with 
                            loc_val = MSimple_val 
                                      (MFun_val (
                                        { argnames with loc_val = List.rev lstargs}, 
                                          binding_value))
                          }); 
                  mbd_body =  body;
                  }
    }
  | SOME;  varassign = located(varassign_in_expr);
    {MSimple_val (MSome_val varassign)}
  | IF ; cond = located(top_expr); THEN; thn = located(top_expr) ELSE; els = located(top_expr); 
  {MIf_val (cond, thn, els)}

  | OPEN_BRACE ;fds = fields_assign; CLOSE_BRACE
    {MComposed_val {mcv_module_def = None; mcv_fields=fds} }
  | md = located(MODUL);DOT ; OPEN_BRACE ;fds = fields_assign; CLOSE_BRACE
    {let open GenUtils in 
    MComposed_val 
      {mcv_module_def = Some ({md with loc_val = rm_first_char md.loc_val}); mcv_fields=fds} 
    }
  | comp = comp_expr; 
    {comp}
  |  envvar = located(ENVVAR); 
     {MEnvRef_val ({envvar with loc_val = GenUtils.rm_first_char envvar.loc_val })}
  |  funcall = located(modulVar); funargs = funcallargs ; 
     {MRef_val (funcall, funargs)}
  | op = operation ; 
    {op}
  | start = located(OPEN_SQRBRACKET) ; CLOSE_SQRBRACKET
      {MSimple_val (MList_val {loc_val = []; loc_pos = start.loc_pos})}
  | OPEN_SQRBRACKET ; lst = located(listSetEl); CLOSE_SQRBRACKET
    {
      MSimple_val (MList_val lst)
    }
  | OPEN_BRACKET; a = top_expr ;CLOSE_BRACKET;
    {a}
(*
  | OPEN_BRACKET; funcall = modulVar; funargs = funcallargs ;CLOSE_BRACKET; funargs2 = funcallargs
    {MRef_val (funcall, List.append funargs funargs2 )}

*)


(*top_expr is a "toplevel" expresion:
    it can be:
      - a basic expression
      - a tuple expression
      - a sequence of exprsssion
*)
top_expr : 
  | var = basic_expr
    {var}
  | var1 = located(varassign_in_expr) ; DOUBLE_MINUS; seq=in_tuple_assign
    {let first_pos = var1.loc_pos in
     let last_pos = (List.nth seq ((List.length seq) - 1)).loc_pos  in

    MSimple_val (MTuple_val {
                              loc_val = (var1 :: seq); 
                              loc_pos = GufoLocHelper.pos_merging first_pos last_pos}) 
    }

varassign_in_expr : 
  | a = leaf_expr
    {a}
  | OPEN_BRACKET; CLOSE_BRACKET;
    { MSimple_val (MEmpty_val) }
  | OPEN_BRACKET; a = top_expr ; CLOSE_BRACKET;
    {a}
    
  | OPEN_BRACE ;fds = fields_assign; CLOSE_BRACE
    {MComposed_val {mcv_module_def = None; mcv_fields=fds} }
  | md = located(MODUL);DOT ; OPEN_BRACE ;fds = fields_assign; CLOSE_BRACE
    {let open GenUtils in 
    MComposed_val 
      {mcv_module_def = Some ({md with loc_val = rm_first_char md.loc_val}); mcv_fields=fds} 
    }
  | start = located (OPEN_SQRBRACKET) ; CLOSE_SQRBRACKET
      {
        MSimple_val (MList_val ({loc_val = []; loc_pos = start.loc_pos }))
      }
  | OPEN_SQRBRACKET ; lst = located(listSetEl); CLOSE_SQRBRACKET
    {
      MSimple_val (MList_val lst)
    }
  | var = located(modulVar); 
     {MRef_val (var, [])}


comp_expr :
  | expr1 = located(varassign_in_expr) ; eq=located(EQUALITY); expr2 = located(varassign_in_expr)
    {MComp_val ({loc_val=Egal;loc_pos=eq.loc_pos}, expr1, expr2) }
  | expr1 = located(varassign_in_expr) ; eq=located(INEQUALITY); expr2 = located(varassign_in_expr)
    {MComp_val ({loc_val=NotEqual;loc_pos=eq.loc_pos}, expr1, expr2) }
  | expr1 = located(varassign_in_expr) ; eq=located(CLOSING_CHEVRON) ; expr2 = located(varassign_in_expr)
    {MComp_val ({loc_val=GreaterThan;loc_pos=eq.loc_pos}, expr1, expr2) }
  | expr1 = located(varassign_in_expr) ; eq=located(EQ_CLOSING_CHEVRON); expr2 = located(varassign_in_expr)
    {MComp_val ({loc_val=GreaterOrEq;loc_pos=eq.loc_pos}, expr1, expr2) }
  | expr1 = located(varassign_in_expr) ; eq=located(OPENING_CHEVRON) ; expr2 = located(varassign_in_expr)
    {MComp_val ({loc_val=LessThan;loc_pos=eq.loc_pos}, expr1, expr2) }
  | expr1 = located(varassign_in_expr) ; eq=located(EQ_OPENING_CHEVRON); expr2 = located(varassign_in_expr)
    {MComp_val ({loc_val=LessOrEq;loc_pos=eq.loc_pos}, expr1, expr2) }

anonymousfun: 
  | OPEN_BRACKET; FUN; args = located(funargs_top); ARROW;   body = located(topvarassign); CLOSE_BRACKET; 
    { 
    MSimple_val (MFun_val ({loc_val = List.rev args.loc_val; loc_pos = args.loc_pos}, 
                            body))
    }



topvarassign : 
  | assign = top_expr
    { assign }


operation : 
  | OPEN_BRACKET; CLOSE_BRACKET;
    { MSimple_val (MEmpty_val) }
  | i1 = located(top_expr); PLUS_STR; i2 = located(top_expr)
    {MBasicFunBody_val (MConcatenation, [i1; i2])}
  | i1 = located(top_expr); PLUS; i2 = located(top_expr)
    {MBasicFunBody_val (MAddition, [i1; i2])}
  | i1 = located(top_expr); PLUS_DOT; i2 = located(top_expr) 
    {MBasicFunBody_val (MAdditionFloat, [i1; i2])}
  | i1 = located(top_expr) ; MINUS ; i2 = located(top_expr)
    {MBasicFunBody_val (MSoustraction, [i1; i2])}
  | i1 = located(top_expr) ; MINUS_DOT ; i2 = located(top_expr)
    {MBasicFunBody_val (MSoustractionFloat, [i1; i2])}
  | i1 = located(top_expr) ; STAR ; i2 = located(top_expr)
    {MBasicFunBody_val (MMultiplication, [i1; i2])}
  | i1 = located(top_expr) ; STAR_DOT ; i2 = located(top_expr)
    {MBasicFunBody_val (MMultiplicationFLoat, [i1; i2])}
  | i1 = located(top_expr) ; DIVISION ; i2 = located(top_expr)
    {MBasicFunBody_val (MDivision , [i1; i2])}
  | i1 = located(top_expr) ; DIVISION_DOT ; i2 = located(top_expr)
    {MBasicFunBody_val (MDivisionFloat , [i1; i2])}
  | i1 = located(top_expr); MODULO ; i2 = located(top_expr)
    {MBasicFunBody_val (MModulo, [i1; i2])}
  | i1 = located(top_expr); MODULO_DOT ; i2 = located(top_expr)
    {MBasicFunBody_val (MModuloFloat, [i1; i2])}
  | i1 = located(top_expr); WITH ; i2 = located(top_expr)
    {MBasicFunBody_val (MWithList, [i1; i2])}
  | i1 = located(top_expr); WITH_SET ; i2 = located(top_expr)
    {MBasicFunBody_val (MWithSet, [i1; i2])}
  | i1 = located(top_expr); WITH_MAP ; i2 = located(top_expr)
    {MBasicFunBody_val (MWithMap, [i1; i2])}
  | i1 = located(top_expr); WITHOUT_SET ; i2 = located(top_expr)
    {MBasicFunBody_val (MWithoutSet, [i1; i2])}
  | i1 = located(top_expr); WITHOUT_MAP ; i2 = located(top_expr)
    {MBasicFunBody_val (MWithoutMap, [i1; i2])}
  | i1 = located(top_expr); SHAS ; i2 = located(top_expr)
    {MBasicFunBody_val (MHasSet, [i1; i2])}
  | i1 = located(top_expr); MHAS ; i2 = located(top_expr)
    {MBasicFunBody_val (MHasMap, [i1; i2])}
  | assign1 = located(top_expr); DOUBLE_SEMICOLON ;assign2 = exprseqeassign; 
    { MBody_val (assign1 ::assign2)}

cmd_expr:
  |  cmdas = located(simple_cmd);
    { SimpleCmd cmdas }
  | acmds = located(cmd_expr) ; SIMPLE_AND 
    {
      (ForkedCmd (acmds))
    }
  | acmds = located(cmd_expr); AND ; bcmds = located(cmd_expr)
    {
      (AndCmd (acmds, bcmds))
    }
  | acmds = located(cmd_expr); OR ; bcmds = located(cmd_expr)
    {
      (OrCmd (acmds, bcmds))
    }
  | acmds = located(cmd_expr); PIPE ; bcmds = located(cmd_expr)
    {
      (PipedCmd(acmds, bcmds))
    }
  | acmds = located(cmd_expr) ; SEMICOLON; bcmds = located(cmd_expr)
    {
      (SequenceCmd(acmds, bcmds))
    }



funcallargs :
  | {[]}
  | funarg = located(varassign_in_expr); funargs = funcallargs
    { funarg :: funargs }


funargs_top:
  |  { [] }
  | varname = located(VARNAME); funargs = funargs_top;
    { 
      let open GenUtils in 
      (MBaseArg ( { varname with loc_val =  rm_first_char varname.loc_val})) 
        ::funargs 
    }
  | OPEN_BRACKET ; subargs = located(funargs); CLOSE_BRACKET; funargs = funargs_top
    { (MTupleArg subargs) ::funargs }

funargs:
  | arg = located(VARNAME)
    { 
      let open GenUtils in 
      [MBaseArg ( { arg with loc_val = rm_first_char arg.loc_val}
        )] }

  |  OPEN_BRACKET; arg = located(funargs) ; CLOSE_BRACKET;
    { 
      [MTupleArg arg] }
  | arg = located(VARNAME) ; DOUBLE_MINUS ; funargs = funargs
    { 
      let open GenUtils in 
      (MBaseArg ( { arg with loc_val = rm_first_char arg.loc_val } )):: funargs 
    }
  | OPEN_BRACKET arg = located(funargs); CLOSE_BRACKET; DOUBLE_MINUS ; funargs = funargs
    { 
      (MTupleArg arg):: funargs }

exprseqeassign:
  | var1 =located(varassign_in_expr)
    {[var1]}
  | var1 = tupleassign; DOUBLE_SEMICOLON ; seq=exprseqeassign
    {var1 :: seq}

tupleassign:
  | var1 =located(varassign_in_expr)
    {var1}

in_tuple_assign:
  | var1 =located(varassign_in_expr)
    {[var1]}
  | var1 = located(varassign_in_expr); DOUBLE_MINUS; seq=in_tuple_assign
    {var1 :: seq}

(******* CMD PARSING ********)


simple_cmd:
  | str_cmd = located(WORD); args = cmd_args; redirs = redirs; 
  | str_cmd = located(FILE); args = cmd_args; redirs = redirs; 
    {
      let stdout,stdouterr, stdin = List.fold_left 
        (fun (stdout, stdouterr, stdin) redir -> 
          match redir with 
            | MRedirOFile file -> MCMDOFile file, stdouterr, stdin
            | MRedirOFileAppend file -> MCMDOFileAppend file, stdouterr, stdin
            | MRedirEFile file -> stdout, MCMDEFile file, stdin
            | MRedirEFileAppend file -> stdout, MCMDEFileAppend file, stdin
            | MRedirEStdOut -> stdout, MCMDEStdOut, stdin
            | MRedirIFile file -> stdout, stdouterr, MCMDIFile file
            | _ -> assert false

        ) 
        (MCMDOStdOut, 
         MCMDEStdErr, 
         MCMDIStdIn)
        redirs
      in
      let cmd = {mcm_cmd = str_cmd;
                mcm_args = args; 
                mcm_output = stdout; 
                mcm_outputerr = stdouterr; 
                mcm_input_src = stdin; 
               }
    in
     cmd
    }

cmd_args :
  | arg = cmd_arg; args = cmd_args ; 
    {arg :: args}
  | {[]}


cmd_arg :
  | arg = located(INT)
    {SORString  
      {
        loc_val = (string_of_int arg.loc_val) ;
        loc_pos = arg.loc_pos;
      }
    }
  | arg = located(FLOAT)
    {SORString 
      {
      loc_val = (string_of_float arg.loc_val) ;
      loc_pos = arg.loc_pos ;
      }
    }
  | t = located(TRUE)
    {SORString 
      {
        loc_val = "true" ;
        loc_pos = t.loc_pos;
      }
    }
  | f = located(FALSE)
    {SORString 
      {
        loc_val = "false" ;
        loc_pos = f.loc_pos
      }
    }
  | arg = located(STRING)
  | arg = located(WORD)
  | arg = located(ARG)
  | arg = located(FILE)
    { SORString arg
      
    }
  | t = located(TILDE)
    {SORString 
      {
        loc_val = "~" ;
        loc_pos = t.loc_pos;
      }
    }
  | arg = located(modulVarOrExpr)
    { 
      SORExpr arg;
    }
(** Commenting the STAR and DOT here will solve the states conflicts of * and .
but will make commands such as ls * to fail. *)
  | s = located(STAR)
    {SORString 
      {
        loc_val = "*" ;
        loc_pos = s.loc_pos;
      }
    }
  | d = located(DOT)
    {SORString 
      {
        loc_val = "." ;
        loc_pos = d.loc_pos;
      }
    }

redirs :
  | {[]}
  | redir = redir; redirs = redirs;  
    { List.concat [redir; redirs] }


redir : 
  | CLOSING_CHEVRON; file = located(WORD);
  | CLOSING_CHEVRON; file = located(FILE);
    { [MRedirOFile (SORString file)] }
  | CLOSING_CHEVRON; file = located (modulVarOrExpr);
    { [MRedirOFile (SORExpr file)] }
  | DOUBLE_CLOSING_CHEVRON; file = located(WORD)
  | DOUBLE_CLOSING_CHEVRON; file = located(FILE)
    { [MRedirOFileAppend (SORString file)] }
  | DOUBLE_CLOSING_CHEVRON; file = located (modulVarOrExpr)
    { [MRedirOFileAppend (SORExpr file)] }
  | WRITE_ERROR_TO; file = located(WORD)
  | WRITE_ERROR_TO; file = located(FILE)
    { [MRedirEFile (SORString file)] }
  | WRITE_ERROR_TO; file = located(modulVarOrExpr)
    { [MRedirEFile (SORExpr file)] }
  | WRITE_ERROR_NEXT_TO; file = located(WORD)
  | WRITE_ERROR_NEXT_TO; file = located(FILE)
    { [MRedirEFileAppend (SORString file)] }
  | WRITE_ERROR_NEXT_TO; file = located(modulVarOrExpr)
    { [MRedirEFileAppend (SORExpr file)] }
  | WRITE_ERROR_TO_STD
    { [MRedirEStdOut] }
  | WRITE_ALL_TO ; file  = located(WORD)
  | WRITE_ALL_TO ; file  = located(FILE)
    { [MRedirOFile (SORString file); MRedirEFile (SORString file)] }
  | WRITE_ALL_TO ; file  = located(modulVarOrExpr)
    { [MRedirOFile (SORExpr file); MRedirEFile (SORExpr file)] }
  | OPENING_CHEVRON; file = located(WORD)
  | OPENING_CHEVRON; file = located(FILE)
    { [MRedirIFile (SORString file)] }
  | OPENING_CHEVRON; file = located(modulVarOrExpr)
    { [MRedirIFile (SORExpr file)] }

(******* CMD PARSING END ********)
(******* FIELDS PARSING ********)

fields_assign:
  | { [] }
  | fields = fields_assign ;  sf = located(single_field); COMMA
    {  sf :: fields }
  ;

single_field:
  | fieldname = located(WORD) ; AFFECTATION; value=  located(topvarassign);
    { {mtfv_name = fieldname; mtfv_val = value} }

modulVar:
  | var = located(VARNAME); idx = lst_index; 
    {
      let open GenUtils in 
      {
        mrv_module = None; 
        mrv_varname= { loc_val = [( rm_first_char var.loc_val )] ; loc_pos = var.loc_pos}; 
        mrv_index = idx;
      }
    }
  |  varfield = located(VARFIELD); idx = lst_index; 
    {
     let open GenUtils in 
     let lst = Str.split (Str.regexp "\\.") (rm_first_char varfield.loc_val)  in
    {
      mrv_module = None; 
      mrv_varname= {loc_val = lst; loc_pos = varfield.loc_pos ;} ; 
      mrv_index = idx}
    }
  | var = located(MODULVAR); idx = lst_index; 
    {
    let open GenUtils in 
    let lst = Str.split (Str.regexp "\\.") (rm_first_char var.loc_val) in
    let (modul, lst) = (List.hd lst, List.tl lst) in
      {
        mrv_module = Some modul; 
        mrv_varname= {loc_val = lst; loc_pos = var.loc_pos} ; 
        mrv_index = idx
      }
    }

lst_index:
  | {None}
  | prev_idx = lst_index; OPEN_SQRIDXBRACKET;elkey = located(top_expr); CLOSE_SQRBRACKET;
    {match prev_idx with
      | None -> Some [elkey]
      | Some lst -> Some (elkey::lst)
    }



listSetEl:
  | el = located(top_expr);
    {[el]}
  | el = located(top_expr); COMMA; lst = listSetEl;
    {el::lst}

mapEl:
  | key = located(top_expr); COLON ; el = located(top_expr);
    {[key, el]}
  | key = located(top_expr); COLON ; el = located(top_expr); COMMA; lst = mapEl;
    {(key,el)::lst}



modulVarOrExpr:
  | a = located(ENVVAR)
    {MEnvRef_val ({ a with loc_val = GenUtils.rm_first_char a.loc_val })}
  | a = located(modulVar)
    {MRef_val (a,[])}
  | OPEN_BRACKET ; varassign = top_expr; CLOSE_BRACKET;
    { varassign }


(**
 ##################### END MAIN LANGUAGE PARSER #####################
*)

%inline located(X): x=X {
  GufoParsedHelper.with_poss $startpos $endpos x
}

