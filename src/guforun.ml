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

(*The execution launcher.*)

open Lexing
open Printf
open GufoParsed
open Gufo_lexer
open Gufo_lexing
open GufoEngine
open GufoParsedToOpt
open GenUtils


let parse_program filename = 
  let open Format in
  let open Gufo in
  match (GufoStart.parse_file filename) with
    | None -> printf "No prog!\n"; assert false
    | Some p -> GufoStart.handle_program p (GufoStart.filename_to_module filename)

(*previous basic interactive mod.*)
let _run_interactive () =
  printf "## Ilo interactive shell ##\n";
  let rec run_interactive_ () = 
    printf "# ";
    let res = read_line () in
    match (GufoStart.parse_shell  res) with
      | None -> printf "No prog!\n"; assert false
      | Some p -> 
          let prog = GufoStart.handle_program p "Shell_prog" in
          let opt_prog,_ = GufoParsedToOpt.parsedToOpt prog in
          printf "%s\n" (Gufo.MCore.moval_to_string (GufoEngine.exec opt_prog));

    run_interactive_()
  in
  run_interactive_()

let () =
  match Array.length Sys.argv with
    | 2 -> 
        let prog = parse_program Sys.argv.(1) in
        let opt_prog,_ = GufoParsedToOpt.parsedToOpt prog in
        printf "%s\n" (Gufo.MCore.moval_to_string (GufoEngine.exec opt_prog))
    | 1 -> 
          (*run_interactive ()*)
        GufoConsole.run ()

    | _ -> printf "bad number of arguments"

