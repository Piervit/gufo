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

%token STRUCT 
%token LET
%token FUN
%token JOKER (*_*)
%token COLON (* : *)
%token AFFECTATION (* = *)
%token TRUE 
%token FALSE
(* pattern matching *)
%token MATCH
%token PERCENT 
%token ARROW (* -> *)
%token WITH (* also array rilated *)
%token WITHOUT (* also array rilated *)
(* file/dir shortcut *)
%token DBL_STAR (* ** *)
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
(* file operation *)
%token FILE_EXIST  (* exist *)
(* mathematic *)
%token PLUS
%token DOUBLE_MINUS
%token MINUS
%token DIVISION
%token STAR
%token MODULO
%token EQUALITY (* == *)
%token INEQUALITY (* != *)
(*
%token GREATER_THAN (* gt *)
%token GREATER_OR_EQUAL (* gte *)
%token LOWER_THAN (* lt *)
%token LOWER_OR_EQUAL (* lte *)
*)
(* array rilated *)
%token LIST_OPEN (* [[ *)
%token LIST_CLOSE (* ]] *)
%token OPEN_SQRBRACKET (* [ *)
%token OPEN_SQRIDXBRACKET (* [ *)
%token CLOSE_SQRBRACKET  (* ] *)
%token LIST_LENGTH 
%token LIST_APPEND 
%token LIST_PREPEND
%token LIST_RM_LAST
%token LIST_RM_FIRST
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
%token DOUBLE_SEMICOLON (* ;; *)
%token <string> FREETYPE
%token <string> VARNAME 
%token <string> VARFIELD
%token <string> MODULVAR
%token <string> MODUL
%token <string> WORD 

%token DOT
%token NONE
%token SOME
%token START
%token END
%token COMMA
%token EOF


%left PLUS
%left MINUS
%right STAR
%right DIVISION
%left MODULO
%left WITH



%left SEMICOLON
%left SIMPLE_AND
%left AND
%left OR
%left PIPE


%right ELSE
%left DOUBLE_MINUS
%left EQUALITY
%left INEQUALITY
(*
%left LOWER_THAN
%left LOWER_OR_EQUAL
%left GREATER_THAN
%left GREATER_OR_EQUAL
*)
%right DOUBLE_SEMICOLON
%left TILDE
%right OPEN_BRACKET
%left CLOSE_BRACKET


%start <GufoParsed.mprogram option> prog
%start <GufoParsed.mprogram option> shell
%%

shell:
    |  EOF
    {   
      Some GufoParsed.{mpg_types = GenUtils.StringMap.empty; mpg_topvar = []; mpg_topcal = MSimple_val(MEmpty_val)} 
    }
    |  main_expr = topvarassign; EOF
    {   
      Some GufoParsed.{mpg_types = GenUtils.StringMap.empty; mpg_topvar = []; mpg_topcal = main_expr} 
    }
    | LET ; varnames = var_tuple_decl; argnames = funargs_top ; AFFECTATION; funbody = topvarassign; EOF
	{
          let open GufoParsed in 
          let open GenUtils in 
          (match argnames, varnames with
            | [], _ -> 
              let mvar = {mva_name = varnames; mva_value = funbody} in
              Some (GufoParsed.{mpg_types = GenUtils.StringMap.empty; mpg_topvar = [mvar];  mpg_topcal = MSimple_val(MEmpty_val)} )
            | argnames, MBaseDecl varname ->
                let mvar = {mva_name = MBaseDecl varname; mva_value = MSimple_val (MFun_val (List.rev argnames, funbody))} in
                Some (GufoParsed.{mpg_types = GenUtils.StringMap.empty; mpg_topvar = [mvar];  mpg_topcal = MSimple_val(MEmpty_val)} )
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
                  (GufoParsed.MComposed_type 
                              {GufoParsed.mct_name = name; 
                               GufoParsed.mct_fields = typ; 
                               GufoParsed.mct_internal_val = internal_val})
          in
          Some GufoParsed.{mpg_types = StringMap.singleton name ctyp; mpg_topvar = []; mpg_topcal = MSimple_val(MEmpty_val) } 
	}




prog:
    | p = mfile{ Some p }
;


mfile:
  |  topels = mtopels; EOF
    {
      let (types, variables) = topels in
      GufoParsed.{mpg_types = types; mpg_topvar = variables;  mpg_topcal = MSimple_val(MEmpty_val)} 
    }
    ;
  |  topels = mtopels; START; main_expr = topvarassign;EOF
    {   
      let (types, variables) = topels in
      GufoParsed.{mpg_types = types; mpg_topvar = variables; mpg_topcal = main_expr} 
    }


mtopels: 
  |topels = rev_mtypes_or_topvals {topels};


var_tuple_decl:
  | name = VARNAME; 
    {let open GenUtils in 
     let open GufoParsed in
      let name = rm_first_char name in
      MBaseDecl (name)
    }
  | left = var_tuple_decl ; DOUBLE_MINUS ;right = var_tuple_decl ;
    {
     let open GufoParsed in
      match left,right with
        | MBaseDecl lf, MBaseDecl rg -> MTupDecl [MBaseDecl lf;MBaseDecl rg]
        | MBaseDecl lf, MTupDecl rg -> MTupDecl ((MBaseDecl lf) :: rg)
        | MTupDecl lf,MBaseDecl rg ->  MTupDecl (List.rev(MBaseDecl rg :: (List.rev lf)))
        | MTupDecl lf, right ->  MTupDecl (List.rev(right :: (List.rev lf)))
    }
  | OPEN_BRACKET; decl = var_tuple_decl ; CLOSE_BRACKET
    {
     let open GufoParsed in
     MTupDecl [decl]
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
                raise (GufoParsed.TypeError ("The type "^name^" is already declared."))
            | false ->
                ( GenUtils.StringMap.add name
                  (GufoParsed.MComposed_type 
                              {GufoParsed.mct_name = name; 
                               GufoParsed.mct_fields = typ; 
                               GufoParsed.mct_internal_val = internal_val})
                  types , topvals)
	}

  (*variables and function assignation*)

  | topels= rev_mtypes_or_topvals; LET ; varnames = var_tuple_decl; argnames = funargs_top ; AFFECTATION; funbody = topvarassign;
	{
          let open GufoParsed in 
          let open GenUtils in 
      	  let (types, topvals) = topels in
          (match argnames, varnames with
            | [], _ -> 
              let mvar = {mva_name = varnames; mva_value = funbody} in
      	      (  types ,  mvar :: topvals )
            | argnames, MBaseDecl varname ->
                let mvar = {mva_name = MBaseDecl varname; mva_value = MSimple_val (MFun_val (List.rev argnames, funbody))} in
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
  | vars = rev_fields_decl; varname = WORD ; COLON; typename = toptypedecl ; COMMA
    { 
      let lst_field, lst_val = vars in
      GufoParsed.{mtf_name = varname; mtf_type = typename; mtf_extend = None} :: lst_field, lst_val }
  | vars = rev_fields_decl; EXTENDS; fieldname= modulVar; COMMA
    {let open GufoParsed in 
      let lst_field, lst_val = vars in
      let fieldnameStr = ref_to_string fieldname in 
      { mtf_name = String.concat "_" ["ext"; fieldnameStr]; 
        mtf_type = (MRef_type fieldname);
        mtf_extend = Some fieldnameStr;
      } :: lst_field, 
      lst_val 
    }
  | vars = rev_fields_decl; EXTENDS; fieldname= modulVar; WITH; anofun = anonymousfun; COMMA
    {let open GufoParsed in 
      let lst_field, lst_val = vars in
      let fieldnameStr = ref_to_string fieldname in 
      {mtf_name = String.concat "_" ["ext"; fieldnameStr]; 
       mtf_type = (GufoParsed.MRef_type fieldname);
       mtf_extend = Some fieldnameStr
      } :: lst_field , 
     (String.concat "_" ["extfun"; fieldnameStr], anofun) ::lst_val }
  ;

toptypedecl:
  | ft = FREETYPE;
    {GufoParsed.MAll_type ft }
  | STRINGTYPE
    { GufoParsed.MBase_type GufoParsed.MTypeString }
  | INTTYPE
    { GufoParsed.MBase_type GufoParsed.MTypeInt }
  | FLOATTYPE
    { GufoParsed.MBase_type GufoParsed.MTypeFloat }
  | BOOLTYPE
    { GufoParsed.MBase_type GufoParsed.MTypeBool }
  | CMDTYPE
    { GufoParsed.MBase_type GufoParsed.MTypeCmd }
  | internal_type = typedecl; LISTTYPE
    { GufoParsed.MList_type internal_type }
  | args = funargdecl ; 
    { 
      let rev_args = List.rev args in
      let ret_type = List.hd rev_args in
      let args = List.rev (List.tl rev_args) in
      GufoParsed.MFun_type (args, ret_type) }
  | name = modulVar
    { GufoParsed.MRef_type name }
  | first_tupel = typedecl ; DOUBLE_MINUS; tupel_suite= typetupelseq;
    { GufoParsed.MTuple_type (first_tupel:: tupel_suite)}
    ;
  | tdec = typedecl; OPTIONTYPE; 
    {GufoParsed.MOption_type tdec}
  | tdec = typedecl ;SETTYPE
    { GufoParsed.MSet_type tdec }
  | OPEN_BRACKET; tdec = typedecl; COMMA; tdec2 = typedecl; CLOSE_BRACKET ;MAPTYPE
    { GufoParsed.MMap_type (tdec, tdec2)}


typedecl:
  | ft = FREETYPE;
    {GufoParsed.MAll_type ft }
  | STRINGTYPE
    { GufoParsed.MBase_type GufoParsed.MTypeString }
  | INTTYPE
    { GufoParsed.MBase_type GufoParsed.MTypeInt }
  | FLOATTYPE
    { GufoParsed.MBase_type GufoParsed.MTypeFloat }
  | BOOLTYPE
    { GufoParsed.MBase_type GufoParsed.MTypeBool }
  | CMDTYPE
    { GufoParsed.MBase_type GufoParsed.MTypeCmd }
  | internal_type = typedecl; LISTTYPE
    { GufoParsed.MList_type internal_type }
  | name = modulVar
    {GufoParsed.MRef_type name}
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


varassign_one_term: 
  | NONE
    {GufoParsed.MSimple_val (GufoParsed.MNone_val)}
  | i = INT
    {GufoParsed.MSimple_val (GufoParsed.MBase_val (GufoParsed.MTypeIntVal i))}
  | s = STRING 
    {GufoParsed.MSimple_val (GufoParsed.MBase_val (GufoParsed.MTypeStringVal s))}
  | FALSE
    {GufoParsed.MSimple_val (GufoParsed.MBase_val (GufoParsed.MTypeBoolVal false))}
  | TRUE
    {GufoParsed.MSimple_val (GufoParsed.MBase_val (GufoParsed.MTypeBoolVal true))}
  | f = FLOAT
    {GufoParsed.MSimple_val (GufoParsed.MBase_val (GufoParsed.MTypeFloatVal f))}
  |  anonf = anonymousfun ; 
    {anonf}

varassign_internal_no_bracket:
  | res = varassign_one_term 
    { res }
  | LET ; binding_name = var_tuple_decl ; argnames = funargs_top ; AFFECTATION ; binding_value = topvarassign; IN ; OPEN_BRACKET; body = topvarassign ; CLOSE_BRACKET;
    {let open GufoParsed in 
     let open GenUtils in
      MBind_val {mbd_name = binding_name; 
                  mbd_value = 
                    (match argnames with 
                      | [] -> binding_value
                      | lstargs -> MSimple_val (MFun_val (List.rev lstargs, binding_value)))
                  ; 
                  mbd_body =  body;
                  }
    }
  | SOME;  varassign = varassign_in_expr;
    {GufoParsed.MSimple_val (GufoParsed.MSome_val varassign)}
  | IF ; cond = varassign_no_bracket ; THEN; thn = varassign_no_bracket ELSE; els = varassign_no_bracket; 
  {GufoParsed.MIf_val (cond, thn, els)}

  | OPEN_BRACE ;fds = fields_assign; CLOSE_BRACE
    {GufoParsed.MComposed_val GufoParsed.{mcv_module_def = None; mcv_fields=fds} }
  | md = MODUL;DOT ; OPEN_BRACE ;fds = fields_assign; CLOSE_BRACE
    {let open GenUtils in 
    GufoParsed.MComposed_val 
      GufoParsed.{mcv_module_def = Some (rm_first_char md); mcv_fields=fds} 
    }
  | comp = comp_expr; 
    {comp}
  |  funcall = modulVar; funargs = funcallargs ; 
     {GufoParsed.MRef_val (funcall, funargs)}
  | op = operation ; 
    {op}
  | OPEN_SQRBRACKET ; CLOSE_SQRBRACKET
      {GufoParsed.MSimple_val (GufoParsed.MList_val [])}
  | OPEN_SQRBRACKET ; lst = listSetEl; CLOSE_SQRBRACKET
    {let open GufoParsed in
      MSimple_val (MList_val lst)
    }
  | OPEN_BRACKET; a = varassign_no_bracket ;CLOSE_BRACKET;
    {a}


varassign_no_bracket : 
  | var = varassign_internal_no_bracket
    {var}
  | var1 = varassign_in_expr ; DOUBLE_MINUS; seq=tupleassign
    { GufoParsed.MSimple_val (GufoParsed.MTuple_val  (var1 :: seq)) }
  | assign1 = varassign_in_expr; DOUBLE_SEMICOLON ;assign2 = exprseqeassign; 
    { GufoParsed.MBody_val (assign1 ::assign2) }
  | MINUS_OPENING_CHEVRON;  MINUS_CLOSING_CHEVRON
    {
      GufoParsed.MSimple_val (GufoParsed.MSet_val [])
    }
  | MINUS_OPENING_CHEVRON; set = listSetEl; MINUS_CLOSING_CHEVRON
    {let open GufoParsed in
      MSimple_val (MSet_val set)
    }
  | MINUS_OPENING_CHEVRON; COLON;   MINUS_CLOSING_CHEVRON
    {
      GufoParsed.MSimple_val (GufoParsed.MMap_val[])
    }
  | MINUS_OPENING_CHEVRON; map = mapEl; MINUS_CLOSING_CHEVRON
    {let open GufoParsed in
      MSimple_val (MMap_val map)
    }


varassign_in_expr : 
  | a = varassign_one_term
    {a}
  | OPEN_BRACKET; a = varassign_no_bracket ; CLOSE_BRACKET;
    {a}
    
  | OPEN_BRACE ;fds = fields_assign; CLOSE_BRACE
    {GufoParsed.MComposed_val GufoParsed.{mcv_module_def = None; mcv_fields=fds} }
  | md = MODUL;DOT ; OPEN_BRACE ;fds = fields_assign; CLOSE_BRACE
    {let open GenUtils in 
    GufoParsed.MComposed_val 
      GufoParsed.{mcv_module_def = Some (rm_first_char md); mcv_fields=fds} 
    }
  | OPEN_SQRBRACKET ; CLOSE_SQRBRACKET
      {GufoParsed.MSimple_val (GufoParsed.MList_val [])}
  | OPEN_SQRBRACKET ; lst = listSetEl; CLOSE_SQRBRACKET
    {let open GufoParsed in
      MSimple_val (MList_val lst)
    }
  | var = modulVar; 
     {GufoParsed.MRef_val (var, [])}


comp_expr :
  | expr1 = varassign_in_expr ; EQUALITY; expr2 = varassign_in_expr
    {GufoParsed.MComp_val (GufoParsed.Egal, expr1, expr2) }
  | expr1 = varassign_in_expr ; INEQUALITY; expr2 = varassign_in_expr
    {GufoParsed.MComp_val (GufoParsed.NotEqual, expr1, expr2) }
  | expr1 = varassign_in_expr ; CLOSING_CHEVRON ; expr2 = varassign_in_expr
    {GufoParsed.MComp_val (GufoParsed.GreaterThan, expr1, expr2) }
  | expr1 = varassign_in_expr ; EQ_CLOSING_CHEVRON; expr2 = varassign_in_expr
    {GufoParsed.MComp_val (GufoParsed.GreaterOrEq, expr1, expr2) }
  | expr1 = varassign_in_expr ; OPENING_CHEVRON ; expr2 = varassign_in_expr
    {GufoParsed.MComp_val (GufoParsed.LessThan, expr1, expr2) }
  | expr1 = varassign_in_expr ; EQ_OPENING_CHEVRON; expr2 = varassign_in_expr
    {GufoParsed.MComp_val (GufoParsed.LessOrEq, expr1, expr2) }

anonymousfun: 
  | OPEN_BRACKET; FUN; args = funargs_top; ARROW;   body =topvarassign; CLOSE_BRACKET;
    {let open GufoParsed in 
    MSimple_val (MFun_val (List.rev args, body))
    }



topvarassign : 
  | assign = varassign_no_bracket
    { assign }


operation : 
  | i1 = varassign_no_bracket; PLUS; i2 = varassign_no_bracket
    {GufoParsed.MBasicFunBody_val (GufoParsed.MAddition, [i1; i2])}
  | i1 = varassign_no_bracket ; MINUS ; i2 = varassign_no_bracket
    {GufoParsed.MBasicFunBody_val (GufoParsed.MSoustraction, [i1; i2])}
  | i1 = varassign_no_bracket ; STAR ; i2 = varassign_no_bracket
    {GufoParsed.MBasicFunBody_val (GufoParsed.MMultiplication, [i1; i2])}
  | i1 = varassign_no_bracket ; DIVISION ; i2 = varassign_no_bracket
    {GufoParsed.MBasicFunBody_val (GufoParsed.MDivision , [i1; i2])}
  | i1 = varassign_no_bracket; MODULO ; i2 = varassign_no_bracket
    {GufoParsed.MBasicFunBody_val (GufoParsed.MModulo, [i1; i2])}
  | i1 = varassign_no_bracket; WITH ; i2 = varassign_no_bracket
    {GufoParsed.MBasicFunBody_val (GufoParsed.MWith, [i1; i2])}
  | i1 = varassign_no_bracket; WITHOUT ; i2 = varassign_no_bracket
    {GufoParsed.MBasicFunBody_val (GufoParsed.MWithout, [i1; i2])}
  |  cmdas = simple_cmd;
    { GufoParsed.MSimple_val (GufoParsed.MBase_val (GufoParsed.MTypeCmdVal cmdas)) }
  | acmds = varassign_no_bracket ; SIMPLE_AND 
    {let open GufoParsed in
      MSimple_val (MBase_val 
      (MTypeCmdVal (ForkedCmd (mval_to_cmd acmds))))}
  | acmds = varassign_no_bracket ; AND ; bcmds = varassign_no_bracket
    {
      let open GufoParsed in
      MSimple_val (MBase_val (MTypeCmdVal (AndCmd (mval_to_cmd acmds, mval_to_cmd bcmds))))
    }
  | acmds = varassign_no_bracket ; OR ; bcmds = varassign_no_bracket
    {
      let open GufoParsed in
      MSimple_val (MBase_val (MTypeCmdVal (OrCmd (mval_to_cmd acmds, mval_to_cmd bcmds))))
    }
  | acmds = varassign_no_bracket ; PIPE ; bcmds = varassign_no_bracket
    {
      let open GufoParsed in
      MSimple_val (MBase_val (MTypeCmdVal (PipedCmd(mval_to_cmd acmds, mval_to_cmd bcmds))))
    }
  | acmds = varassign_no_bracket ; SEMICOLON; bcmds = varassign_no_bracket
    {
      let open GufoParsed in
      MSimple_val (MBase_val (MTypeCmdVal (SequenceCmd(mval_to_cmd acmds, mval_to_cmd bcmds))))
    }


funcallargs :
  | {[]}
  | funarg = varassign_in_expr; funargs = funcallargs
    { funarg :: funargs }



  funargs_top:
  |  { [] }
  | varname = VARNAME; funargs = funargs_top;
    { 
      let open GenUtils in 
      (GufoParsed.MBaseArg (rm_first_char varname)) ::funargs }
  | OPEN_BRACKET ; subargs = funargs; CLOSE_BRACKET; funargs = funargs_top
    { (GufoParsed.MTupleArg subargs) ::funargs }

funargs:
  | arg = VARNAME
    { 
      let open GenUtils in 
      [GufoParsed.MBaseArg (rm_first_char arg)] }
  |  OPEN_BRACKET; arg = funargs ; CLOSE_BRACKET;
    { 
      [GufoParsed.MTupleArg arg] }
  | arg = VARNAME ; DOUBLE_MINUS ; funargs = funargs
    { 
      let open GenUtils in 
      (GufoParsed.MBaseArg (rm_first_char arg)):: funargs }
  | OPEN_BRACKET arg = funargs; CLOSE_BRACKET; DOUBLE_MINUS ; funargs = funargs
    { 
      (GufoParsed.MTupleArg arg):: funargs }

exprseqeassign:
  | var1 =varassign_in_expr
    {[var1]}
  | var1 = varassign_in_expr; DOUBLE_SEMICOLON ; seq=tupleassign
    {var1 :: seq}

tupleassign:
  | var1 =varassign_in_expr
    {[var1]}
  | var1 = varassign_in_expr; DOUBLE_MINUS; seq=tupleassign
    {var1 :: seq}

(******* CMD PARSING ********)


simple_cmd:
  | str_cmd = WORD; args = cmd_args; redirs = redirs; 
  | str_cmd = FILE; args = cmd_args; redirs = redirs; 
    {
      let open GufoParsed in
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
     GufoParsed.SimpleCmd cmd
    }

cmd_args :
  | arg = cmd_arg; args = cmd_args ; 
    {arg :: args}
  | {[]}

cmd_arg :
  | arg = INT
    {GufoParsed.SORString  (string_of_int arg) }
  | arg = FLOAT
    {GufoParsed.SORString (string_of_float arg) }
  | TRUE 
    {GufoParsed.SORString "true" }
  | FALSE 
    {GufoParsed.SORString "false" }
  | arg = STRING
  | arg = WORD 
  | arg = ARG
  | arg = FILE
    { GufoParsed.SORString arg}
  | arg = modulVarOrExpr
    { GufoParsed.SORExpr arg}
(*
  | arg = DOT
    {GufoParsed.SORString "." }
  | arg = STAR
    {GufoParsed.SORString "*" }
*)

redirs :
  | {[]}
  | redir = redir; redirs = redirs;  
    { List.concat [redir; redirs] }

redir : 
  | CLOSING_CHEVRON; file = WORD;
  | CLOSING_CHEVRON; file = FILE;
    { [GufoParsed.MRedirOFile (GufoParsed.SORString file)] }
  | CLOSING_CHEVRON; file = modulVarOrExpr;
    { [GufoParsed.MRedirOFile (GufoParsed.SORExpr file)] }
  | DOUBLE_CLOSING_CHEVRON; file = WORD
  | DOUBLE_CLOSING_CHEVRON; file = FILE
    { [GufoParsed.MRedirOFileAppend (GufoParsed.SORString file)] }
  | DOUBLE_CLOSING_CHEVRON; file = modulVarOrExpr
    { [GufoParsed.MRedirOFileAppend (GufoParsed.SORExpr file)] }
  | WRITE_ERROR_TO; file = WORD
  | WRITE_ERROR_TO; file = FILE
    { [GufoParsed.MRedirEFile (GufoParsed.SORString file)] }
  | WRITE_ERROR_TO; file = modulVarOrExpr
    { [GufoParsed.MRedirEFile (GufoParsed.SORExpr file)] }
  | WRITE_ERROR_NEXT_TO; file = WORD
  | WRITE_ERROR_NEXT_TO; file = FILE
    { [GufoParsed.MRedirEFileAppend (GufoParsed.SORString file)] }
  | WRITE_ERROR_NEXT_TO; file = modulVarOrExpr
    { [GufoParsed.MRedirEFileAppend (GufoParsed.SORExpr file)] }
  | WRITE_ERROR_TO_STD
    { [GufoParsed.MRedirEStdOut] }
  | WRITE_ALL_TO ; file  = WORD
  | WRITE_ALL_TO ; file  = FILE
    { [GufoParsed.MRedirOFile (GufoParsed.SORString file); GufoParsed.MRedirEFile (GufoParsed.SORString file)] }
  | WRITE_ALL_TO ; file  = modulVarOrExpr
    { [GufoParsed.MRedirOFile (GufoParsed.SORExpr file); GufoParsed.MRedirEFile (GufoParsed.SORExpr file)] }
  | OPENING_CHEVRON; file = WORD
  | OPENING_CHEVRON; file = FILE
    { [GufoParsed.MRedirIFile (GufoParsed.SORString file)] }
  | OPENING_CHEVRON; file = modulVarOrExpr
    { [GufoParsed.MRedirIFile (GufoParsed.SORExpr file)] }

(******* CMD PARSING END ********)
(******* FIELDS PARSING ********)

fields_assign:
  | { [] }
  | fields = fields_assign ; fieldname = WORD ; AFFECTATION; value=  topvarassign; COMMA
    { GufoParsed.{mtfv_name = fieldname; mtfv_val = value} :: fields }
  ;


modulVar:
  | var = VARNAME; idx = lst_index; 
    {
      let open GenUtils in 
      GufoParsed.{mrv_module = None; mrv_varname= [(rm_first_char var)]; mrv_index = idx}}
    |  varfield = VARFIELD; idx = lst_index; 

    {
     let open GenUtils in 
     let lst = Str.split (Str.regexp "\\.") (rm_first_char varfield)  in
    GufoParsed.{mrv_module = None; mrv_varname= lst ; mrv_index = idx}
    }
  | var = MODULVAR; idx = lst_index; 
    {
    let open GenUtils in 
    let lst = Str.split (Str.regexp "\\.") (rm_first_char var) in
    let (modul, lst) = (List.hd lst, List.tl lst) in
      GufoParsed.{mrv_module = Some modul; mrv_varname= lst ; mrv_index = idx}
    }

lst_index:
  | {None};
  | prev_idx = lst_index; OPEN_SQRIDXBRACKET;elkey = varassign_no_bracket; CLOSE_SQRBRACKET;
    {match prev_idx with
      | None -> Some [elkey]
      | Some lst -> Some (elkey::lst)
    }



listSetEl:
  | el = varassign_no_bracket;
    {[el]}
  | el = varassign_no_bracket; COMMA; lst = listSetEl;
    {el::lst}

mapEl:
  | key = varassign_no_bracket; COLON ; el = varassign_no_bracket;
    {[key, el]}
      | key = varassign_no_bracket; COLON ; el = varassign_no_bracket; COMMA; lst = mapEl;
    {(key,el)::lst}



modulVarOrExpr:
  | a = modulVar
    {GufoParsed.MRef_val (a,[])}
  | OPEN_BRACKET ; varassign = varassign_no_bracket; CLOSE_BRACKET;
    { varassign }
