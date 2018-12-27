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

  (*get_red_expr: string -> int -> (string, int) 
   * for an expression and a position in this expression, return a reduced expr
   * and the offset at which the reduced expr starts within the initial
   * expression.
   *)
  let get_red_expr expr pos =
    (*we first exploit parenthesis*)
    let end_parenthesis_pos = String.index_from_opt expr pos ')' in 
    let first_parenthesis_pos = String.rindex_from_opt expr pos '(' in 
    let new_expr,pos = 
      match end_parenthesis_pos, first_parenthesis_pos with
        | Some end_pos, Some first_pos -> 
            String.sub expr first_pos (end_pos - first_pos), first_pos
        | None, _ 
        | _, None -> expr, 0
    in
    (*we check if we are within a function scope*)
    try 
      let first_fun_scope = Str.search_backward (Str.regexp "->") new_expr pos in
      String.sub new_expr (first_fun_scope + 1) (String.length new_expr - first_fun_scope - 1), first_fun_scope + 1
      with Not_found ->
        expr , 0 
  in
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
      let red_expr, red_expr_offset = get_red_expr expr pos in
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
      | _ -> CompCmdOrKeyword, cur_word
  in
  try 
    let first_char = String.get cur_word 0 in
    try 
      let second_char = String.get cur_word 1 in 
      get_type_using_two_chars first_char second_char
    with Invalid_argument _ -> 
      (*cur_word has only a single char*)
      get_type_single_char first_char
  with Invalid_argument _ ->
    CompNone, cur_word



