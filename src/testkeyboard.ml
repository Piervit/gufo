open Lwt
open Printf
open LTerm_key

exception ExitTerm of (LTerm.t * LTerm.mode)

let rec loop term history tmod =
  Lwt.catch (fun () -> (LTerm.read_event term)
    >>= fun event -> 
      match event with 
        | Key akey ->
            (** A key has been pressed. *)
                  (match akey.LTerm_key.code with
                    | Escape -> Lwt.fail (ExitTerm (term,tmod))
                    | _ -> 
                        LTerm.fprints term (LTerm_text.eval [S (sprintf "%s\n" (LTerm_key.to_string akey))]) >>= 
                        (fun () -> loop term history tmod )
                  )
        | _ -> 
                loop term history tmod 
  )
    (function
      | exn -> Lwt.fail exn)
 

let main () =
  LTerm_inputrc.load ()
  >>= fun () ->
  (Lwt.catch
    (fun () ->
       Lazy.force LTerm.stdout
       >>= fun term ->
       LTerm.enter_raw_mode term >>=
         (fun tmod -> loop term (LTerm_history.create []) tmod )
    )
    (function
      | ExitTerm (term, tmod) -> LTerm.leave_raw_mode term tmod 
      | exn -> Lwt.fail exn)
  )

let () = Lwt_main.run (main ())
