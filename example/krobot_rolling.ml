#use "topfind";;
#camlp4o;;
#require "lwt.syntax";;
#require "krobot";;
open Krobot_bus;;
open Krobot_message;;

let moving = ref false
let roting = ref false

(* le hub et driver doivent être lancés *)
let bus = Lwt_unix.run (Krobot_bus.get ())
let send m = Lwt_unix.run (Krobot_message.send bus (Unix.gettimeofday (), m))

let handle_message (timestamp, message) =
  match message with
    | CAN(_, frame) -> begin
        match decode frame with

          | Motor_status(m,t,_,_) ->
            moving := m;
            roting := t

          | _ ->
              ()
      end
    | _ ->
        ()

(* a n'executer qu'une fois *)
let () = Lwt_react.E.keep (Lwt_react.E.map handle_message (Krobot_bus.recv bus))

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

let wait t = Lwt_unix.run (lwt () = Lwt_unix.sleep 0. in Lwt_unix.sleep t)
(* en secondes *)

let wait_ref_false r =
  while !r do
    wait 0.01;
  done

let pi = 4. *. atan 1.

let () = move_x (-.0.05) 0.2 0.5

let () = move_x (-.0.5) 0.2 0.5
let () = move_y 0.5 0.2 0.5

let () =
  move_y (-.0.5) 0.2 0.5;
  move_x 0.5 0.2 0.5

let () = reset_odometry ()

let () = turn (-.2.*.pi) (pi/.2. /. 2.) (pi/.2. /. 4. /. 1.)
let () = motor_stop 0.5 0.5


(*
(* commande de la vm / planner *)
open Krobot_geom

let send_bus m = Lwt_unix.run (Krobot_bus.send bus (Unix.gettimeofday (), m))
(* let stop () = send_bus Strategy_stop *)
(* let clear () = send_bus (Trajectory_set_vertices []) *)

(* nettoie la trajectoire enregistree *)
let set_vertice x y = send_bus (Trajectory_set_vertices [{x;y}])
(* enregistre la trajectoire *)
let go () = send_bus Trajectory_go
*)
