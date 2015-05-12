(* open Krobot_bus *)
(* open Krobot_message *)
(* open Krobot_geom *)
(* open Krobot_world_update *)

(* type world = Krobot_world_update.world *)

(* type state = Idle *)

(* type message = *)
(*   | CAN of Krobot_message.t *)
(*   | Msg of Krobot_bus.message *)

(* type output = { *)
(*   timeout : float; (\* in seconds *\) *)
(*   messages : message list; *)
(*   state : state; *)
(*   world : world; *)
(* } *)

(* type input = *)
(*   | Message of Krobot_bus.message *)
(*   | World_updated of world_update *)
(*   | Timeout *)

(* let init_state = Idle *)

(* let update_world world msg = *)
(*   match Krobot_world_update.update_world world msg with *)
(*   | Some (w, u) -> w, World_updated u *)
(*   | None -> world, Message msg *)

(* let step (input:input) (world:world) (state:state) : output = *)
(*   { timeout = 1.; *)
(*     messages = []; *)
(*     state = state; *)
(*     world = world } *)

(* let send_msg bus time = function *)
(*   | CAN c -> Krobot_bus.send bus (time, CAN (Info, Krobot_message.encode c)) *)
(*   | Msg m -> Krobot_bus.send bus (time, m) *)

(* let main_loop bus iter = *)
(*   let rec aux world state timeout = *)
(*     lwt msg = Krobot_entry_point.next ~timeout:Krobot_date.(time_to_wait timeout) iter  in *)
(*     let world, input = *)
(*       match msg with *)
(*       | Krobot_entry_point.Timeout -> *)
(*         (\* Lwt_log.ign_info_f "timeout"; *\) *)
(*         world, Timeout *)
(*       | Krobot_entry_point.Message(t,m) -> *)
(*         update_world world m *)
(*     in *)
(*     let output = step input world state in *)
(*     let time = Unix.gettimeofday () in *)
(*     lwt () = Lwt_list.iter_s (fun m -> send_msg bus time m) output.messages in *)
(*     let timeout_date = Krobot_date.(add (now ()) output.timeout) in *)
(*     aux output.world output.state timeout_date in *)
(*   aux init_world init_state (Krobot_date.now ()) *)

(* module S : Krobot_entry_point.S = struct *)
(*   let name = "control" *)
(*   let options = [] *)
(*   let main bus ev = *)
(*     let iter = Krobot_entry_point.iterator_from_react ev in *)
(*     main_loop bus iter *)
(* end *)

(* module Run = Krobot_entry_point.Make(S) *)


open Krobot_geom;;
open Krobot_mover_control;;

let retry_goto ~state ~destination =
  let rec aux ~state ~destination =
    Format.printf "Start %a@." Krobot_date.print (Krobot_date.now ());
    match_lwt goto ~state ~destination with
    | state, Goto_success ->
      Format.printf "Cool %a@." Krobot_date.print (Krobot_date.now ());
      Lwt.return state
    | state, Goto_failure ->
      Format.printf "Fail %a@." Krobot_date.print (Krobot_date.now ());
      lwt () = Lwt_unix.sleep 0.1 in
      aux ~state ~destination
    | state, Goto_unreachable ->
      Format.printf "Unreachable %a@." Krobot_date.print (Krobot_date.now ());
      lwt () = Lwt_unix.sleep 0.1 in
      aux ~state ~destination
      (* Lwt.fail (Invalid_argument "unreachable") *)
  in
  Format.printf "try go %a@." Krobot_date.print (Krobot_date.now ());
  aux ~state ~destination;;

(* lwt state = retry_goto ~state ~destination:{ x = 0.44; y = 1.6 };; *)
(* lwt state = retry_goto ~state ~destination:{ x = 0.7; y = 1. };; *)

(* lwt state = *)
(*   lwt () = Lwt_unix.sleep 2. in *)
(*   retry_goto ~state ~destination:{ x = 2.1; y = 1. };; *)

(* let rec loop state = *)
(*   Printf.printf "goto 1\n%!"; *)
(*   lwt state = retry_goto ~state ~destination:{ x = 0.44; y = 1.6 } in *)
(*   Printf.printf "goto 2\n%!"; *)
(*   lwt state = retry_goto ~state ~destination:{ x = 2.1; y = 1. } in *)
(*   Printf.printf "restart\n%!"; *)
(*   loop state *)
(* ;; *)

(* lwt () = *)
(*   lwt state = make () in *)
(*   loop state *)


let x = float_of_string (Sys.argv.(1))
let y = float_of_string (Sys.argv.(2))

lwt () =
  lwt state = make () in
  lwt state = retry_goto ~state ~destination:{ x; y } in
  Lwt.return ()
