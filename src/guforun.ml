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

(*The main execution launcher.*)

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
    | None -> Printf.eprintf "No program provided !\n"; assert false
    | Some p -> GufoStart.handle_program p (GufoModuleUtils.filename_to_module filename)

(*previous basic interactive mod.
  deprecated but conserved in case we have to go back to it.
*)
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
          let shell_env = Gufo.MCore.get_env (Sys.getcwd ()) in
          let red_prog, shell_env = (GufoEngine.exec opt_prog shell_env) in
          printf "%s\n" (Gufo.MCore.moval_to_string red_prog.mofp_mainprog.mopg_topcal.loc_val);
          run_interactive_()
  in
  run_interactive_()

let verbose = ref false
let full_verbose = ref false
let help = ref false
let prog = ref None

let init_backtrace _ = Printexc.record_backtrace true

let main () = 
  let speclist = 
    [("-v", Arg.Set verbose, " Enables basic information verbosity.");
     ("--verbose", Arg.Set verbose, " Enables basic information verbosity.");
     ("-V", Arg.Set full_verbose, " Enables full verbosity.");
     ("--Verbose", Arg.Set full_verbose, " Enables full verbosity.");
     ("-h", Arg.Set help, " Print help and exit. ");
     ("--help", Arg.Set help, " Print help and exit. ");
    ]
  in
  try
    let usage_msg = "usage: gufo [option] ... [file]\nIf called without file, will be set to run in interactive mode, else will execute the file.\nList of options:" in 
    Arg.parse speclist (fun afile-> prog := Some afile; print_endline "" ) usage_msg;
    (match !full_verbose with
      | true -> init_backtrace (); GufoConfig.set_debug GufoConfig.DBG_FULL
      | false -> match !verbose with
                  | true -> GufoConfig.set_debug GufoConfig.DBG_INFO
                  | _ -> GufoConfig.set_debug GufoConfig.DBG_NO_DEBUG
    );
    match !help with
      | true -> 
        Printf.printf "%s" (Arg.usage_string speclist usage_msg)
      | false -> 
          (match !prog with
            | Some prog -> 
              let parsedProg = parse_program prog in
              let opt_prog,_ = GufoParsedToOpt.parsedToOpt parsedProg in
              let shell_env = Gufo.MCore.get_env (Sys.getcwd ()) in
              let red_prog, shell_env = (GufoEngine.exec opt_prog shell_env) in
              printf "%s\n" (Gufo.MCore.moval_to_string red_prog.mofp_mainprog.mopg_topcal.loc_val)
            | None -> 
              GufoConsole.run ()
          )
  with 
(*     | TypeError e  *)
    | SyntaxError e ->
        printf "%s\n" e.loc_val
  ;;

main ()
