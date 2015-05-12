#use "topfind";;
#camlp4o;;
#require "lwt.syntax";;
#require "krobot";;
open Krobot_bus;;
open Krobot_message;;
open Krobot_config;;

(* le hub et driver doivent être lancés *)
let bus = Lwt_unix.run (Krobot_bus.get ())
let send m = Krobot_message.send bus (Unix.gettimeofday (), m)
let sends m =
  let t = Lwt_list.iter_s (fun m -> Krobot_message.send bus (Unix.gettimeofday (), m)) m in
  Lwt_unix.run t
let sendt m = Krobot_message.send bus (Unix.gettimeofday (), m)

let lcd_clear () =
  Krobot_lcd.clear bus

let send_line num text =
  Krobot_lcd.send_line bus num text

let move_ax12 id position speed =
  send (Ax12_Goto (id, position, speed))
let set_torque_ax12 id b =
  send (Ax12_Set_Torque_Enable (id, b))
let move_ax12_reg id position speed =
  send (Ax12_Goto_Reg (id, position, speed))
let ax12_action () =
  send (Ax12_Action (254))
let silence_ax12 id =
  send (Ax12_Status_Return_Level (id, 1))

let do_stuff_simple () =
  let (left_in, _, _, left_out) = left_arm_positions in
  let (right_in, _, _, right_out) = right_arm_positions in

  lwt () = move_ax12 left_arm_idx left_out 500 in
  lwt () = Lwt_unix.sleep 0.01 in
  lwt () = move_ax12 right_arm_idx right_out 500 in
  lwt () = Lwt_unix.sleep 1. in
  lwt () = move_ax12_reg left_arm_idx left_in 500 in
  lwt () = Lwt_unix.sleep 0.01 in
  lwt () = move_ax12_reg right_arm_idx right_in 500 in
  lwt () = Lwt_unix.sleep 0.01 in
  lwt () = ax12_action () in
  Lwt.return ()

let () = Lwt_unix.run (do_stuff_simple ())

let do_stuff () =
  let (left_in, _, _, left_out) = left_arm_positions in
  let (right_in, _, _, right_out) = right_arm_positions in

  lwt () = send_line 4 "Setting up arms...  " in
  lwt () = Lwt_unix.sleep 0.02 in
  let _ = send (Ax12_Goto (left_arm_idx, left_in, 200)) in
  lwt () = Lwt_unix.sleep 0.2 in
  let _ = send (Ax12_Goto (right_arm_idx, right_in, 200)) in
  lwt () = Lwt_unix.sleep 1. in

  lwt () = send_line 4 "Moving arms out...  " in
  lwt () = Lwt_unix.sleep 0.02 in
  let _ = send (Ax12_Goto (left_arm_idx, left_out, 500)) in
  lwt () = Lwt_unix.sleep 0.2 in
  let _ = send (Ax12_Goto (right_arm_idx, right_out, 500)) in
  lwt () = Lwt_unix.sleep 2. in

  lwt () = send_line 4 "Moving arms back in " in
  lwt () = Lwt_unix.sleep 0.02 in
  let _ = send (Ax12_Goto (left_arm_idx, left_in, 500)) in
  lwt () = Lwt_unix.sleep 0.2 in
  let _ = send (Ax12_Goto (right_arm_idx, right_in, 500)) in
  lwt () = Lwt_unix.sleep 2. in

  lwt () = send_line 4 "Releasing arms...   " in
  let _ = send (Ax12_Set_Torque_Enable (left_arm_idx, false)) in
  lwt () = Lwt_unix.sleep 0.2 in
  let _ = send (Ax12_Set_Torque_Enable (right_arm_idx, false)) in
  Lwt.return ()

let () = Lwt_unix.run (do_stuff ())

(* let () = Lwt_unix.run (do_stuff 4) *)
