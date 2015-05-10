#use "topfind";;
#camlp4o;;
#require "lwt.syntax";;
#require "krobot";;
open Krobot_bus;;
open Krobot_message;;

(* le hub et driver doivent être lancés *)
let bus = Lwt_unix.run (Krobot_bus.get ())
let send m = Lwt_unix.run (Krobot_message.send bus (Unix.gettimeofday (), m))
let sends m =
  let t = Lwt_list.iter_s (fun m -> Krobot_message.send bus (Unix.gettimeofday (), m)) m in
  Lwt_unix.run t
let sendt m = Krobot_message.send bus (Unix.gettimeofday (), m)

let move_ax12 id position speed =
  send (Ax12_Goto (id, position, speed))
let set_torque_ax12 id b =
  send (Ax12_Set_Torque_Enable (id, b))

(* let () = *)
(*   move_ax12 3 380 300 *)
(* let () = *)
(*   move_ax12 3 600 500 *)
(* let () = *)
(*   set_torque_ax12 3 false *)

let do_stuff () =
  lwt () = sendt (Ax12_Goto (3, 380, 200)) in
  lwt () = Lwt_unix.sleep 2. in
  lwt () = sendt (Ax12_Goto (3, 600, 400)) in
  lwt () = Lwt_unix.sleep 2. in
  lwt () = sendt (Ax12_Set_Torque_Enable (3, false)) in
  Lwt.return ()

let () = Lwt_unix.run (do_stuff ())
