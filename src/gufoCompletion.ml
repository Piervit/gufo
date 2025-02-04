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

(* Completion system for the interactive console. *)


open GenUtils
open GufoExpression
open Gufo.MCore 

(*a comp_type is a type for a kind of completion.
For exemple, a file will not be completed on the same way than a var.
*)
type comp_type =  (*a type of completion*)
  | CompVar   (*completing a variable*)
  | CompModul (*completing a module*)
  | CompVarOrModul (*completing a variable or a module*)
  | CompFile  (*completing a file*)
  | CompArg   (*completing an argument*)
  | CompCmdOrKeyword (*completing a keyword or a cmd*)
  | CompInt (*Completing an integer*)
  | CompString (*Completing an integer*)
  | CompFloat (*Completing a float*)
  | CompNone (*when we don't know what kind of completion to apply.*)

let comp_type_to_string cmp_typ =
  match cmp_typ with
  | CompVar -> "var"
  | CompModul -> "modul"
  | CompVarOrModul -> "var or modul"
  | CompFile -> "file"
  | CompArg -> "arg"
  | CompCmdOrKeyword -> "cmd or keywork"
  | CompInt -> "int"
  | CompString -> "string"
  | CompFloat -> "float"
  | CompNone -> "none type"


let analyser expr pos cur_word = 
  (*les règles sont les suivantes: 
      - Si cur_word commence par $ suivi d'une minuscule -> on veut compléter une variable.
      - Si cur_word commence par $ suivi d'une majuscule -> on veut compléter un nom de module.
      - Si cur_word commence par "./" , il s'agit d'un fichier. 
      - Si cur_word commence par "\"" , il s'agit d'une chaine de caractère. 
      - Si cur_word commence par "-" suivi d'un autre caractère, il s'agit d'un arguemnt. 
      - soit red_expr, la reduction de expr au scope courant pour une position donné.
        Par exemple pour l'expression "let $res = (fun $a $b -> ls -l $a $b ) in $res" et la position "l" de "ls", red_expr, serait "ls -l $a $b"
      - Si cur_word est le premier mot de red_expr, il s'agit soit d'une commande soit d'un mot clef.
      - si cur_word est précédé dans son expression réduite par une commande ou par un nom de fichier, il s'agit d'un nom de fichier.


      Cette fonction retourne donc un type (expr_type * string) . la chaine étant le début du nom (de variable, module, fichier...).
  *)


  (*function to find the completion type using the two first char of cur_word*)
  let get_type_using_two_chars first_char second_char = 
  match first_char , second_char with
    | '$', _ -> 
        (match (is_lowercase second_char) with
          | true -> (CompVar, String.sub cur_word 1 ((String.length cur_word) - 1))
          | false -> (*if it doesn 't start with a lowercase, it start with uppercase*)
                    (CompModul, String.sub cur_word 1 ((String.length cur_word) - 1))
        )
    | '.', '/' -> 
        CompFile, cur_word
    | '-', _ ->
        (match (String.length cur_word) with
          | 0 -> assert false
          | 1 -> CompNone, ""
          | _ -> CompArg, cur_word
        )
    | '"', _ ->
      CompString, String.sub cur_word 1 ((String.length cur_word) - 1)
    | _, _ -> 
      let red_expr, red_expr_offset = get_partial_expr expr pos in
      let first_space_pos = String.rindex_from_opt red_expr (pos - red_expr_offset) ' ' in
      (match first_space_pos with
        | None -> (*this is first word*)
            CompCmdOrKeyword , cur_word
        | Some i -> 
            (*for this to be a cmd or keyword, 
             * there should be only spaces in the i first chars *)
            (match String.length (String.trim (String.sub red_expr 0 i)) with
              | 0 -> CompCmdOrKeyword, cur_word
              | _ -> CompFile, cur_word
            )
      )  
  in
  (*getting completion type when cur_word is only composed of a single char: single_char *)
  let get_type_single_char single_char = 
    match single_char with
      | i when ((Char.code i) > 47) &&  ((Char.code i) < 58)(*when i is numeric*) 
          -> CompInt, (Char.escaped i) 
      | '$' -> CompVarOrModul, ""
      | '.' -> CompFile, "."
      | '-' -> CompArg, ""
      | _ -> 
        let red_expr, red_expr_offset = get_partial_expr expr pos in
        let first_space_pos = String.rindex_from_opt red_expr (pos - red_expr_offset) ' ' in
        (match first_space_pos with
          | None -> (*this is first word*)
              CompCmdOrKeyword , cur_word
          | Some i -> 
              (*for this to be a cmd or keyword, 
               * there should be only spaces in the i first chars *)
              (match String.length (String.trim (String.sub red_expr 0 i)) with
                | 0 -> CompCmdOrKeyword, cur_word
                | _ -> CompFile, cur_word
              )
        )  
  in
  try 
    let first_char = String.get cur_word 0 in
    try 
      let second_char = String.get cur_word 1 in 
      let ty, res = get_type_using_two_chars first_char second_char in
      debug_print (Printf.sprintf "completion type: %s \n" (comp_type_to_string ty));
      ty,res

    with Invalid_argument _ -> 
      (*cur_word has only a single char*)
      get_type_single_char first_char
  with Invalid_argument _ ->
    (*TODO*)
    (*for now, if cur_char is empty, we consider that we are completing a file
(so it show the files of the current dir. 
    In the futur, we want this choice to depend of the predecessing word:
      - if nothing: a command
      - else a file
     *)
    CompFile, ""

(*utility function:
  return true if word start like cur_word, else return false. 
*)
let comp_word_to_cur_word cur_word word =
  let cur_word_size = String.length cur_word in
    match ((String.length word) >= cur_word_size) with
      | false -> false
      | true -> 
          let red_el = String.sub word 0 cur_word_size in
          (String.compare red_el cur_word) = 0




(*command or keywork completion*)
(*return a set of possible completion for a completion type cmd_or_keyword and
the start of expression cur_word.*)
let cmd_or_keyword_completion cur_word =
  (*we want to retrieve every keywork wich start bu cur_word *)
  let possibles = 
    StringSet.filter 
      (fun el -> comp_word_to_cur_word cur_word el) 
      GufoUtils.gufoKeywords 
  in
  (*it can also be a cmd, so we need to have a list of available commands.*)
  let path = (Unix.getenv "PATH") in
  let path_as_list = String.split_on_char ':' path in 
  List.fold_left
    (fun possibles folder ->
      (*in folder, we recover executable and compare their name.*)
      try
      Array.fold_left
      (fun possibles file -> 
        try 
          let full_file = (String.concat "" [folder; "/"; file]) in
          let _ = Unix.access full_file [Unix.X_OK] in
          match (comp_word_to_cur_word cur_word file) with
            | true -> StringSet.add file possibles
            | false -> possibles
        with (Unix.Unix_error _) -> possibles
      ) 
      possibles (Sys.readdir folder)
      with Sys_error _ -> possibles
    )
    possibles path_as_list


let var_completion fulloprog cur_word =
  (*we want to return a StringSet*)
  IntMap.fold
    (fun _ current_var possibles -> 
      let full_current_var = ("$"^current_var) in
      match comp_word_to_cur_word cur_word  full_current_var with
        | false -> possibles
        | true -> StringSet.add full_current_var possibles 
    )
  (*retrieve every variables of the program.*)
  fulloprog.mofp_mainprog.mopg_topvar_debugname
  StringSet.empty

(*Xan complet a module or a module variable.*)
let module_completion fulloprog cur_word =
  match String.index_opt cur_word '.' with
    | None -> 
        (*We are completing a module*)
        IntMap.fold
          (fun _ modname possibles -> 
            let full_current_mod = ("$"^modname) in
            match comp_word_to_cur_word cur_word full_current_mod with
              | false -> possibles
              | true -> 
                  StringSet.add full_current_mod possibles
          )
          fulloprog.mofp_progmap_debug
          StringSet.empty
    | Some idx_dot -> 
        (*$We are completing a module variable*)
        let modulName = String.sub cur_word 1 (idx_dot - 1) in
        let partial_var_name = String.sub cur_word (idx_dot + 1)
                                ((String.length cur_word) - idx_dot - 1) in
        (*does it match a known program module : *)
        match StringMap.find_opt modulName fulloprog.mofp_progmap with
          | Some imod -> (*yes*)
              (match IntMap.find imod fulloprog.mofp_progmodules with
                | MOUserMod umod -> 
                  (*User module*)
                  IntMap.fold 
                    (fun _ modvar possibles -> 
                      match comp_word_to_cur_word partial_var_name modvar with
                        | false -> possibles
                        | true -> StringSet.add ("$"^modulName^"."^modvar) possibles
                    )
                    umod.mopg_topvar_debugname
                    StringSet.empty
                  | MOSystemMod smod ->
                  (*System module*)
                   IntMap.fold 
                    (fun _ modvar possibles -> 
                      match comp_word_to_cur_word partial_var_name modvar.mosmv_name with
                        | false -> possibles
                        | true -> StringSet.add 
                                    ("$"^modulName^"."^modvar.mosmv_name) possibles
                    )
                    smod.mosm_topvar
                    StringSet.empty   
              )
          | None -> (*no*) 
              StringSet.empty
          


let file_completion shell_env cur_word =
  (*we complete relatively to the file present in the current dir.*)
  let syntaxic_dir_part, syntaxic_file_part = 
    match String.rindex_from_opt cur_word ((String.length cur_word)-1) '/' with
      | None -> None, cur_word
      | Some idx ->
          Some (String.sub cur_word 0 (idx+1) ),
          String.sub cur_word (idx+1) (String.length cur_word - (idx+1))
  in 
  let dir_of_cur_word = 
    match syntaxic_dir_part with
      | None -> 
        (* cur_dir is current working directory *)
        shell_env.mose_curdir 
      | Some dir_part -> dir_part
  in 
  try 
    match (Sys.is_directory dir_of_cur_word) with
      | true ->
          (*a valid dir, we can explore completion possiblities*)
          Array.fold_left
          (fun possibles file -> 
            match comp_word_to_cur_word syntaxic_file_part file with
            | true -> 
              (match syntaxic_dir_part with
                | None -> StringSet.add file possibles 
                | Some dir_part -> StringSet.add (dir_part ^ file) possibles 
              )
            | false -> possibles
          )
          StringSet.empty (Sys.readdir dir_of_cur_word)

      | false ->
          (*a file, not a dir, no completion posibilities*)
          StringSet.empty
  with Sys_error _ -> 
        (*we are not in a valid file, no completion*)
        StringSet.empty
  

(*for a comp_type and a word to complete, return the set of possible
completion and the minimal extension of cur_word.*)
let completion shell_env fulloprog cmp_type cur_word =
  (*string_common_start : string -> string -> int -> string 
    return a string containing the 'base' first char of str1 and then the
    common chars with str2.
    For exemple:
    string_common_start "lsmem" "lsmod" 2 will return "lsm". The base just
    allow to compare less caracters.
  *)
  let string_common_start str1 str2 base = 
    let rec cmp str1 str2 curpos curretstr = 
      match (String.length str1, String.length str2) with
        | i, j when i > curpos && j > curpos -> 
          let curCharStr1 = String.get str1 curpos in 
          let curCharStr2 = String.get str2 curpos in 
          if((Char.compare curCharStr1 curCharStr2) = 0) then
            cmp str1 str2 (curpos + 1) (curretstr ^ (String.make 1 curCharStr1))
          else 
            curretstr
        | _,_  -> curretstr
    in
    cmp str1 str2 (String.length base) base
  in
  (*get the set of possible values*)
  let posibilities = 
    match cmp_type with
    | CompVar -> var_completion fulloprog cur_word 
    | CompModul -> module_completion fulloprog cur_word 
    | CompVarOrModul -> 
        let possibles_vars = var_completion fulloprog cur_word in
        let possibles_completion = module_completion fulloprog cur_word in
        StringSet.union possibles_vars possibles_completion
    | CompFile -> file_completion shell_env cur_word 
    | CompArg -> StringSet.empty (*TODO*)
    | CompCmdOrKeyword -> cmd_or_keyword_completion cur_word
    | CompInt -> StringSet.empty (*TODO*)
    | CompString -> StringSet.empty (*TODO*)
    | CompFloat -> StringSet.empty (*TODO*)
    | CompNone -> StringSet.empty (*TODO*)
  in
  (*alphabetically order the set.*)
  let sorted_posibilities = (StringSet.elements posibilities) in
  (*Retrieve the best possible extension.*)
  match List.length sorted_posibilities with
    | 0 -> 
      (*case when there is 0 completion possibilities, default return*)
      sorted_posibilities, cur_word
    | _ -> 
      (*case when there are completions opportunities *)
        let deeper_extension = List.hd sorted_posibilities in
        let deeper_extension = 
          List.fold_left 
    (fun deeper_extension cur_possible ->
      string_common_start deeper_extension cur_possible cur_word
    )
    deeper_extension (List.tl sorted_posibilities)
        in
    sorted_posibilities, deeper_extension


