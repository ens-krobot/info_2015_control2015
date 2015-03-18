#use "topfind";;
#camlp4o;;
#require "lwt.syntax";;
#require "krobot";;
open Krobot_bus;;
open Krobot_message;;
open Lwt;;

let moving_x = ref false
let moving_y = ref false
let rotating = ref false

(* le hub et driver doivent être lancés *)
let bus = Lwt_unix.run (Krobot_bus.get ())
let send m = Lwt_unix.run (Krobot_message.send bus (Unix.gettimeofday (), m))

let handle_message (timestamp, message) =
  match message with
    | CAN(_, frame) -> begin
        match decode frame with

          | Motor_status(mx,my,r,_) ->
            moving_x := mx;
            moving_y := my;
            rotating := r

          | _ ->
              ()
      end
    | _ ->
        ()

(* a n'executer qu'une fois *)
let () = Lwt_react.E.keep (Lwt_react.E.map handle_message (Krobot_bus.recv bus))


let set_dynamics v_max omega_max acc_lin_max acc_rot_max =
  send (Motor_bezier_limits(v_max,omega_max,acc_lin_max,acc_rot_max))

let goto x y theta =
  send (Motor_bezier(x,y,0.,0.,theta,0.))


let move_x distance speed acceleration =
  send (Motor_move_x(distance,speed,acceleration))
(** [Motor_move_x(distance, speed, acceleration)] command to make
    the robot to move.
    - [distance] is in m
    - [speed] is in m/s
    - [acceleration] is in m/s^2
*)

let move_y distance speed acceleration =
  send (Motor_move_y(distance,speed,acceleration))
(** [Motor_move_y(distance, speed, acceleration)] command to make
    the robot to move.
    - [distance] is in m
    - [speed] is in m/s
    - [acceleration] is in m/s^2
*)

let turn angle speed acceleration =
  send (Motor_turn(angle,speed,acceleration))
      (** [Motor_turn(angle, speed, acceleration)] command to make the
          robot to turn.
          - [angle] is in rad
          - [speed] is in rad/s
          - [acceleration] is in rad/s^2
      *)

let motor_stop lin_acc rot_acc = send (Motor_stop(lin_acc, rot_acc))
      (** [Motor_stop(lin_acc, rot_acc)] command to stop following the
          current Bezier Spline and the queued ones.
          - [lin_acc] in m/s^2
          - [rot_acc] in rad/s^2
      *)

let reset_odometry () =
  send (Krobot_message.Set_odometry(0.,0.,0.))

let wait t = Lwt_unix.run (Lwt_unix.sleep 0. >>= fun () -> Lwt_unix.sleep t)
(* en secondes *)

let wait_ref_false r =
  while !r do
    wait 0.01;
  done

let wait_ref_true r =
  while (not !r) do
    wait 0.01;
  done

let wait_for_motors () =
  while (!moving_x || !moving_y || !rotating) do
    wait 0.01;
  done

let pi = 4. *. atan 1.

let() =
  set_dynamics 0.2 0.5 (pi/.4.) (pi/.8.);
  reset_odometry ();
  wait 0.1;
  goto 0.5 0. (pi/.4.);
  wait 0.1;
  wait_for_motors ();

  goto 0.5 0.5 (pi/.2.);
  wait 0.1;
  wait_for_motors ();

  goto 0. 0.5 (pi/.4.);
  wait 0.1;
  wait_for_motors ();

  goto 0. 0. 0.;
  wait 0.1;
  wait_for_motors ();

  Printf.printf "ok\n%!"


(* let () = *)
(*       reset_odometry (); *)
(*       wait 0.1; *)

(*       move_x 0.4 0.2 0.5; *)
(*       turn (pi/.2.) (pi/.2. /. 2.) (pi/.2. /. 4. /. 1.); *)
(*       wait 0.1; *)
(*       wait_ref_false moving_x; *)
(*       wait_ref_false rotating; *)

(*       move_y 0.4 0.2 0.5; *)
(*       turn (-.pi/.2.) (pi/.2. /. 2.) (pi/.2. /. 4. /. 1.); *)
(*       wait 0.1; *)
(*       wait_ref_false moving_y; *)
(*       wait_ref_false rotating; *)

(*       move_x (-.0.4) 0.2 0.5; *)
(*       turn (pi/.2.) (pi/.2. /. 2.) (pi/.2. /. 4. /. 1.); *)
(*       wait 0.1; *)
(*       wait_ref_false moving_x; *)
(*       wait_ref_false rotating; *)

(*       move_y (-.0.4) 0.2 0.5; *)
(*       turn (-.pi/.2.) (pi/.2. /. 2.) (pi/.2. /. 4. /. 1.); *)
(*       wait 0.1; *)
(*       wait_ref_false moving_y; *)
(*       wait_ref_false rotating; *)

(*       Printf.printf "ok\n%!" *)



(* let() = *)
(*       reset_odometry (); *)
(*       wait 0.1; *)
(*       move_x 1. 0.2 0.5; *)
(*       turn pi (pi/.2. /. 2.) (pi/.2. /. 4. /. 1.); *)
(*       wait 0.1; *)
(*       wait_ref_false rotating; *)
(*       wait_ref_false moving_x; *)

(*       move_x (-.1.) 0.2 0.5; *)
(*       turn (-.pi) (pi/.2. /. 2.) (pi/.2. /. 4. /. 1.); *)
(*       wait 0.1; *)
(*       wait_ref_false rotating; *)
(*       wait_ref_false moving_x; *)
(*       wait 1.; *)
(*       Printf.printf "ok\n%!" *)


(* let() = *)
(*       reset_odometry (); *)
(*       wait 0.1; *)
(*       move_x 0.5 0.2 0.5; *)
(*       wait 0.1; *)
(*       wait_ref_false moving_x; *)
(*       turn (pi/.2.) (pi/.2. /. 2.) (pi/.2. /. 4. /. 1.); *)
(*       wait 0.1; *)
(*       wait_ref_false rotating; *)
(*       move_x 0.5 0.2 0.5; *)
(*       wait 0.1; *)
(*       wait_ref_false moving_x; *)
(*       wait 0.1; *)
(*       move_x (-.1.) 0.2 0.5; *)
(*       turn (-.pi/.2.) (pi/.2. /. 2.) (pi/.2. /. 4. /. 1.); *)
(*       wait 0.1; *)
(*       wait_ref_false moving_x; *)
(*       wait_ref_false rotating; *)
(*       Printf.printf "ok\n%!" *)
