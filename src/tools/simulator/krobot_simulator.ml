(*
 * krobot_simulator.ml
 * -----------------------
 * Copyright : (c) 2013, Xavier Lagorce <Xavier.Lagorce@crans.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Simulate the robot. *)

open Lwt
open Lwt_react
open Krobot_config
open Krobot_bus
open Krobot_message
open Krobot_geom

let section = Lwt_log.Section.make "krobot(simulator)"
let time_step = 0.001

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true
let hil = ref false
let sensors_emu = ref false
let robot_sim = ref true
let go_normal = ref false
let go_simu = ref false
let go_hil = ref false

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
  "-sensors", Arg.Set sensors_emu, " Don't emulate sensor inputs";
  "-no-simulation", Arg.Clear robot_sim, " Don't simulate the robot";
  "-hil", Arg.Set hil, " Run in hardware in the loop mode";
  "-go-normal", Arg.Set go_normal, " Put the card in normal mode and exit";
  "-go-simulation", Arg.Set go_simu, " Put the cards in simulation mode and exit";
  "-go-normal", Arg.Set go_normal, " Put the cards in normal mode and exit";
  "-go-hil", Arg.Set go_hil, " Put the cards in hardware in the loop mode and exit";
]

let usage = "\
Usage: krobot-simulator [options]
options are:"

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

(* State of the robot. *)
type state = {
  x : float;
  y : float;
  theta : float;
}

type internal_state = {
  theta_f : float;
  theta_bl : float;
  theta_br : float;
}

type command =
  | Idle
      (* The axis is doing nothing. *)
  | Trapezoid of float * float * float * float
      (* [Trapezoid(t_start, t_end, t_acc, velocity)] *)
  | Speed of float * float
      (* [Speed(t_end, velocity)] *)
  | Lock_theta of float * float * float

(* Type of simulators. *)
type simulator = {
  mutable state : state;
  (* The state of the robot. *)
  (*mutable state_indep : state;*)
  (* The state of the robot for second set of encoders. *)
  mutable internal_state : internal_state;
  (* The state of the wheels. *)
  mutable velocity_f : float;
  (* Velocity of the front motor. *)
  mutable velocity_bl : float;
  (* Velocity of the rear left motor. *)
  mutable velocity_br : float;
  (* Velocity of the rear right motor. *)
  mutable time : float;
  (* The current time. *)
  mutable command_x : command;
  (* The current command for X axis. *)
  mutable command_y : command;
  (* The current command for Y axis. *)
  mutable lock_ref : float;
  (* reference for the lock_target control loop *)
  mutable lock_error : float;
  (* latest error in theta control *)
  mutable command_theta : command;
  (* The current command for orientation axis. *)
  mutable omni_limits : float * float * float * float;
}

(* +-----------------------------------------------------------------+
   | Utility functions                                               |
   +-----------------------------------------------------------------+ *)

let print sim =
  Lwt_log.debug_f "
time = %f
state:
  x = %f
  y = %f
  theta = %f
internal_state:
  theta_f = %f
  theta_bl = %f
  theta_br = %f
velocities:
  front = %f
  rear-left = %f
  rear-right = %f
"
    sim.time
    sim.state.x
    sim.state.y
    sim.state.theta
    sim.internal_state.theta_f
    sim.internal_state.theta_bl
    sim.internal_state.theta_br
    sim.velocity_f
    sim.velocity_bl
    sim.velocity_br


(* +-----------------------------------------------------------------+
   | Trajectory generation                                           |
   +-----------------------------------------------------------------+ *)

let lock_target sim target_x target_y angle =
  (* Compute orientation to target *)
  let target_ori = (atan2 (target_y -. sim.state.y) (target_x -. sim.state.x)) +. angle in
  let target_ori =
    if target_ori > pi then
      target_ori -. 2. *. pi
    else if target_ori < (-.pi) then
      target_ori +. 2.*. pi
    else
      target_ori
  in
  let error = target_ori -. sim.state.theta in
  (* let error = *)
  (*   if (abs_float error) < (abs_float (error -. 2.*.pi)) then *)
  (*     error *)
  (*   else *)
  (*     error -. 2.*.pi *)
  (* in *)
  (* let error = *)
  (*   if (abs_float error) < (abs_float (error +. 2.*.pi)) then *)
  (*     error *)
  (*   else *)
  (*     error +. 2.*.pi *)
  (* in *)
  let dtheta = time_step *. pi /. 4. in
  sim.lock_error <- error;
  sim.lock_ref <-
    if (abs_float error) <= dtheta then
      target_ori
    else if error >= 0. then
      sim.lock_ref +. dtheta
    else
      sim.lock_ref -. dtheta ;
  1. *. (sim.lock_ref -. sim.state.theta)

let eval_command sim command =
  let new_cmd = match command with
    | Idle ->
      Idle
    | Speed (t_end, _)
    | Trapezoid (_, t_end, _, _) ->
      if sim.time > t_end then Idle else command
    | Lock_theta _ ->
      command
  in
  let u = match command with
    | Idle ->
      0.
    | Speed (_, vel) ->
      vel
    | Trapezoid (t_start, t_end, t_acc, vel) ->
      if sim.time < (t_start +. t_acc) then
        vel *. (sim.time -. t_start) /. t_acc
      else if sim.time < (t_end -. t_acc) then
        vel
      else
        vel *. (t_end -. sim.time) /. t_acc
    | Lock_theta (x, y, angle) ->
      lock_target sim x y angle
  in
  (u, new_cmd)

let velocities sim dt =
  (* Put the robot into idle if the last command is terminated. *)
  let u_x, new_cmd_x = eval_command sim sim.command_x in
  let u_y, new_cmd_y = eval_command sim sim.command_y in
  let w, new_cmd_theta = eval_command sim sim.command_theta in
  sim.command_x <- new_cmd_x;
  sim.command_y <- new_cmd_y;
  sim.command_theta <- new_cmd_theta;
  (u_x, u_y, w)

let move_x sim distance velocity acceleration =
  if distance <> 0. && velocity > 0. && acceleration > 0. then begin
    let t_acc = velocity /. acceleration in
    let t_end = (velocity *. velocity +. (abs_float distance) *. acceleration) /. (velocity *. acceleration) in
    let sign = if distance >= 0. then 1. else -1. in
    if t_end > 2. *. t_acc then begin
      if t_acc <> 0. then
        sim.command_x <- Trapezoid(sim.time, sim.time +. t_end, t_acc, sign *. velocity)
    end else begin
      if t_acc <> 0. then begin
        let t_acc = sqrt (abs_float (distance) /. acceleration) in
        let t_end = 2. *. t_acc in
        let velocity = sign *. acceleration *. t_acc in
        sim.command_x <- Trapezoid(sim.time, sim.time +. t_end, t_acc, velocity)
      end
    end
  end

let move_y sim distance velocity acceleration =
  if distance <> 0. && velocity > 0. && acceleration > 0. then begin
    let t_acc = velocity /. acceleration in
    let t_end = (velocity *. velocity +. (abs_float distance) *. acceleration) /. (velocity *. acceleration) in
    let sign = if distance >= 0. then 1. else -1. in
    if t_end > 2. *. t_acc then begin
      if t_acc <> 0. then
        sim.command_y <- Trapezoid(sim.time, sim.time +. t_end, t_acc, sign *. velocity)
    end else begin
      if t_acc <> 0. then begin
        let t_acc = sqrt (abs_float (distance) /. acceleration) in
        let t_end = 2. *. t_acc in
        let velocity = sign *. acceleration *. t_acc in
        sim.command_y <- Trapezoid(sim.time, sim.time +. t_end, t_acc, velocity)
      end
    end
  end

let turn sim angle velocity acceleration =
  if angle <> 0. && velocity > 0. && acceleration > 0. then begin
    let t_acc = velocity /. acceleration in
    let t_end = (velocity *. velocity +. (abs_float angle) *. acceleration) /. (velocity *. acceleration) in
    let sign = if angle >= 0. then 1. else -1. in
    if t_end > 2. *. t_acc then begin
      if t_acc <> 0. then begin
        sim.command_theta <- Trapezoid(sim.time, sim.time +. t_end, t_acc, sign *. velocity)
      end
    end else begin
      let t_acc = sqrt (abs_float (angle) /. acceleration) in
      let t_end = 2. *. t_acc in
      let velocity = sign *. acceleration *. t_acc in
      if t_acc <> 0. then begin
        sim.command_theta <- Trapezoid(sim.time, sim.time +. t_end, t_acc, velocity)
      end
    end
  end

let goto sim x_end y_end theta_end =
  let dx = x_end -. sim.state.x in
  let dy = y_end -. sim.state.y in
  let dtheta = theta_end -. sim.state.theta in
  let (v_lin_max, v_rot_max, a_lin_max, a_rot_max) = sim.omni_limits in
  let adx = if dx >= 0. then dx else -. dx in
  let ady = if dy >= 0. then dy else -. dy in
  let d = sqrt (adx*.adx+.ady*.ady) in
  if d > 0. then begin
    let vx = v_lin_max *. adx /. d in
    let vy = v_lin_max *. ady /. d in
    let ax = a_lin_max *. adx /. d in
    let ay = a_lin_max *. ady /. d in
    move_x sim dx vx ax;
    move_y sim dy vy ay;
  end;
  begin
    match sim.command_theta with
    | Lock_theta _ -> ()
    | _ -> turn sim dtheta v_rot_max a_rot_max
  end


let set_velocities sim u_x u_y w duration =
  sim.command_x <- Speed(sim.time +. duration, u_x);
  sim.command_y <- Speed(sim.time +. duration, u_y);
  sim.command_theta <- Speed(sim.time +. duration, w)

let get_velocities sim (u_x, u_y, w) =
  let theta = sim.state.theta in
  (* Transform speed from world coordinates to robot coordinates *)
  let (u_x, u_y) = (u_x *. (cos theta) +. u_y *. (sin theta),
                    -. u_x *. (sin theta) +. u_y *. (cos theta)) in
  let v_f = (-. u_x +. w *. wheels_distance) /. (wheels_diameter /. 2.) in
  let v_bl = (-. u_x *. (cos(2.*.pi/.3.)) -. u_y *. (sin(2.*.pi/.3.)) +. w *. wheels_distance) /. (wheels_diameter /. 2.) in
  let v_br = (-. u_x *. (cos(2.*.pi/.3.)) +. u_y *. (sin(2.*.pi/.3.)) +. w *. wheels_distance) /. (wheels_diameter /. 2.) in
  (v_f, v_bl, v_br)

let get_state sim =
  sim.state

let get_encoders sim =
  let (theta_f, theta_bl, theta_br) = (sim.internal_state.theta_f,
                                       sim.internal_state.theta_bl,
                                       sim.internal_state.theta_br) in
  (theta_f *. wheels_diameter /. 2.,
   theta_bl *. wheels_diameter /. 2.,
   theta_br *. wheels_diameter /. 2.)


(* +-----------------------------------------------------------------+
   | Main loops                                                      |
   +-----------------------------------------------------------------+ *)

let sim = ref None

let loop bus sim =
  let rec aux () =
    let time = Unix.gettimeofday () in
    let delta = time -. sim.time in
    sim.time <- time;

    lwt () = print sim in

    let v_f, v_bl, v_br, u_x, u_y, w = if !hil then
        let cos_fact = 1. /. (1. -. (cos(2.*.pi/.3.))) in
        let sin_fact = 1. /. (2. *. (sin(2.*.pi/.3.))) in
        (* Speeds in robot coordinates *)
        let lx = (-. sim.velocity_f +. sim.velocity_bl /. 2. +. sim.velocity_br /. 2.)
                 *. (wheels_diameter /. 2.) *. cos_fact in
        let ly = (-. sim.velocity_bl +. sim.velocity_br) *. (wheels_diameter /. 2.) *. sin_fact in
        (* Speeds in world coordinates *)
        let theta = sim.state.theta in
        let vx = lx *. (cos theta) -. ly *. (sin theta) in
        let vy = lx *. (sin theta) +. ly *. (cos theta) in
        let w = (sim.velocity_f +. sim.velocity_bl +. sim.velocity_br)
                /. (2. *. wheels_distance) *. (wheels_diameter /. 2.) *. cos_fact in
        (sim.velocity_f, sim.velocity_bl, sim.velocity_br, vx, vy, w)
    else
      let (u_x, u_y, w) = velocities sim delta in
      let (v_f, v_bl, v_br) = get_velocities sim (u_x, u_y, w) in
      (v_f, v_bl, v_br, u_x, u_y, w)
    in
    let theta = sim.state.theta +. w *. delta in
    let theta =
      if theta > pi then
        theta -. 2. *. pi
      else if theta < -.pi then
        theta +. 2. *. pi
      else
        theta
    in
    sim.state <- {
      x = sim.state.x +. u_x *. delta;
      y = sim.state.y +. u_y *. delta;
      theta = theta;
    };
    sim.internal_state <- {
      theta_f = sim.internal_state.theta_f +. delta *. v_f /. (2. *. wheels_diameter);
      theta_bl = sim.internal_state.theta_bl +. delta *. v_bl /. (2. *. wheels_diameter);
      theta_br = sim.internal_state.theta_br +. delta *. v_br /. (2. *. wheels_diameter);
    };
    lwt () = Lwt_unix.sleep time_step in
    aux () in
  aux ()

let send_CAN_messages sim bus =
  let rec aux () =
    (* Sends the state of the robot. *)
    lwt () = Krobot_message.send bus (sim.time, Odometry(sim.state.x, sim.state.y, sim.state.theta)) in
    (*lwt () = Krobot_message.send bus (sim.time, Odometry_indep(sim.state.x, sim.state.y, sim.state.theta)) in*)
    (* Wait before next batch of packets (emulate the electronic board behavior) *)
    let lock_error, lock_status = match sim.command_theta with
      | Lock_theta _ -> sim.lock_error, true
      | _ -> 0., false
    in
    lwt () = Krobot_message.send bus (sim.time, Lock_status(lock_error, lock_status)) in
    lwt () = Lwt_unix.sleep 0.005 in
    (* Sends the state of the motors. *)
    let tc_x = match sim.command_x with Idle -> false | _ -> true in
    let tc_y = match sim.command_y with Idle -> false | _ -> true in
    let tc_theta = match sim.command_theta with Idle | Lock_theta _ -> false | _ -> true in
    lwt () = Krobot_message.send bus (Unix.gettimeofday (), Motor_status(tc_x, tc_y, tc_theta, false)) in
    lwt () = Lwt_unix.sleep 0.005 in
    aux () in
  aux ()

(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

let handle_message bus (timestamp, message) =
  match message with
    | Kill "simulator" ->
      exit 0
    | CAN(_, frame) -> begin
      match !sim with
        | Some sim -> begin
          (* Generic messages *)
          (match decode frame with
            | Set_odometry(x, y, theta) ->
              sim.state <- { x; y; theta }
            | Lock_target(x, y, theta) ->
              sim.lock_ref <- sim.state.theta;
              sim.command_theta <- Lock_theta (x, y, theta)
            | Unlock_target ->
              sim.command_theta <- Idle
            | _ ->
              ());
          (* Messages related to HIL mode *)
          (if !hil then begin
            match decode frame with
              | Encoder_position_speed_2(pos, speed) ->
                sim.velocity_bl <- speed
              | Encoder_position_speed_3(pos, speed) ->
                sim.velocity_br <- speed
              | Encoder_position_speed_4(pos, speed) ->
                sim.velocity_f <- speed
              | _ ->
                () end
           (* Message related to full software simulation mode *)
           else begin
             match decode frame with
              | Motor_move_x(dist, speed, acc) ->
                ignore (Lwt_log.info_f "received: move_x(%f, %f, %f)" dist speed acc);
                move_x sim dist speed acc
              | Motor_move_y(dist, speed, acc) ->
                ignore (Lwt_log.info_f "received: move_y(%f, %f, %f)" dist speed acc);
                move_y sim dist speed acc
              | Motor_turn(angle, speed, acc) ->
                ignore (Lwt_log.info_f "received: turn(%f, %f, %f)" angle speed acc);
                turn sim angle speed acc
              | Motor_stop(lin_acc, rot_acc) ->
                sim.command_x <- Idle;
                sim.command_y <- Idle;
                sim.command_theta <- Idle;
              | Set_odometry(x, y, theta) ->
                sim.state <- { x; y; theta }
              (*| Set_odometry_indep(x, y, theta) ->
                sim.state <- { x; y; theta }*)
              | Motor_omni_limits(v_lin_max, v_rot_max, a_lin_max, a_rot_max) ->
                sim.omni_limits <- (v_lin_max, v_rot_max, a_lin_max, a_rot_max)
              | Motor_omni_goto(x_end, y_end, theta_end) ->
                goto sim x_end y_end theta_end
              | _ ->
                () end);
          Lwt.return () end
        | None ->
          Lwt.return ()
      end
    | _ -> Lwt.return ()

(* +-----------------------------------------------------------------+
   | Hokuyo emulation                                                |
   +-----------------------------------------------------------------+ *)

let dist_x x y theta obstacle =
  let y_int = y +. (obstacle -. x) *. (tan theta) in
  if (y_int > 0. && y_int < Krobot_config.world_height) then
    let d = (obstacle -. x) /. (cos theta) in
    if d >= 0. then
      Some d
    else
      None
  else
    None

let dist_y x y theta obstacle =
  let x_int = x +. (obstacle -. y) /. (tan theta) in
  if (x_int > 0. && x_int < Krobot_config.world_width) then
    let d = (obstacle -. y) /. (sin theta) in
    if d >= 0. then
      Some d
    else
      None
  else
    None

let min_border_distance x y theta =
  let l = match dist_x x y theta 0. with Some v -> [v] | None -> [] in
  let l = match dist_x x y theta Krobot_config.world_width with Some v -> v::l | None -> l in
  let l = match dist_y x y theta 0. with Some v -> v::l | None -> l in
  let l = match dist_y x y theta Krobot_config.world_height with Some v -> v::l | None -> l in
  match l with
    | [] -> None
    | l -> Some (List.fold_left min max_float l)

let sq x = x *. x

let distance_to_obj x y (sin_theta, cos_theta) obj =
  let x2, y2 = x +. cos_theta, y +. sin_theta in
  let x3, y3, r = obj.pos.Krobot_geom.x, obj.pos.Krobot_geom.y, obj.size in
  let a = sq (x2 -. x) +. sq (y2 -. y) in
  let b = 2. *. ( (x2-.x)*.(x-.x3)+.(y2-.y)*.(y-.y3) ) in
  let c = sq x3 +. sq y3 +. sq x +. sq y -. 2. *. (x*.x3+.y*.y3) -. sq r in
  let delta = sq b -. 4. *.a*.c in
  if abs_float delta < 0.01 then
    Some (-.b/.(2.*.a))
  else if delta > 0. then
    let d1 = (-.b -. sqrt (delta)) /. (2.*.a) in
    let d2 = (-.b +. sqrt (delta)) /. (2.*.a) in
    if d1 < 0. && d2 < 0. then
      None
    else if d1 < 0. && d2 > 0. then
      Some d2
    else if d1 > 0. && d2 < 0. then
      Some d1
    else
      Some (min d1 d2)
  else
    None

let closest_obstacle x y theta objs =
  let border = min_border_distance x y theta in
  let sin_theta = sin theta in
  let cos_theta = cos theta in
  let dist_to_objs = List.map (distance_to_obj x y (sin_theta, cos_theta)) objs in
  List.fold_left
    (fun min_dist dist -> match dist with
       | Some dist ->
         (match min_dist with
            | Some min_dist -> Some (min min_dist dist)
            | None -> Some dist)
       | None -> min_dist)
    border
    dist_to_objs

(*let gen_data robot =
  let dim = Array.length Krobot_config.urg_angles in
  let {Krobot_geom.x=urg_rel_x;
       Krobot_geom.y=urg_rel_y } = Krobot_config.urg_position in
  let urg_pos = [| urg_rel_x; urg_rel_y; 1. |] in
  let rot = rot_mat robot.theta in
  let urg_pos = mult rot urg_pos in
  let pos = Krobot_geom.translate
    { Krobot_geom.x=robot.x; Krobot_geom.y=robot.y }
    { Krobot_geom.vx = urg_pos.(0); Krobot_geom.vy = urg_pos.(1) } in
  let {Krobot_geom.x=cen_x;
       Krobot_geom.y=cen_y } = pos in
  let l = ref [] in
  for i = 0 to dim - 1 do
    let angle = Krobot_config.urg_angles.(i) in
    match min_border_distance cen_x cen_y (robot.theta +. angle) with
      | Some dist ->
        let x = dist *. cos angle +. urg_pos.(0) in
        let y = dist *. sin angle +. urg_pos.(1) in
        l := {Krobot_geom.x;Krobot_geom.y} :: !l
      | None ->
        ()
  done;
  Array.of_list !l*)

let circle_obstacle r { pos = { Krobot_geom.x; y }; size } =
  let t = Unix.gettimeofday () in
  let t = 0.3 *. t in
  let x' = x +. r *. cos t in
  let y' = y +. r *. sin t in
  { pos = { Krobot_geom.x = x'; y = y' }; size }

let gen_data robot =
  let dim = Array.length Krobot_config.urg_down_angles in
  let l = ref [] in
  for i = 0 to dim - 1 do
    let angle = Krobot_config.urg_down_angles.(i) in
    (*let test_obstacles = List.map (circle_obstacle 0.2)
        Krobot_config.test_obstacles in
      let obstacles = test_obstacles @ Krobot_config.fixed_obstacles in*)
    let obstacles = [] in
    match closest_obstacle robot.x robot.y (robot.theta +. angle) obstacles with
      | Some dist ->
        let x = dist *. cos angle in
        let y = dist *. sin angle  in
        l := {Krobot_geom.x=x;Krobot_geom.y=y} :: !l
      | None ->
        ()
  done;
  Array.of_list !l

let loop_urg sim bus =
  let rec aux () =
    let time = Unix.gettimeofday () in
    let msg = Urg (Krobot_bus.Down, gen_data sim.state) in
    lwt () = Krobot_bus.send bus (time, msg) in
    lwt () = Lwt_unix.sleep 0.1 in
    aux () in
  aux ()

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

lwt () =
  Arg.parse options ignore usage;

  (* Display all informative messages. *)
  Lwt_log.append_rule "*" Lwt_log.Info;

  (* Open the krobot bus. *)
  lwt bus = Krobot_bus.get () in

  if (!go_hil || !go_simu || !go_normal) then begin
    if !go_normal then
      ignore(Krobot_message.send bus (Unix.gettimeofday (), Set_simulation_mode Sim_no))
    else if !go_hil then
      ignore(Krobot_message.send bus (Unix.gettimeofday (), Set_simulation_mode Sim_HIL))
    else if !go_simu then
      ignore(Krobot_message.send bus (Unix.gettimeofday (), Set_simulation_mode Sim_normal))
    else
    exit 0
  end;

  (* Fork if not prevented. *)
  if !fork then Krobot_daemon.daemonize bus;

  (* Handle krobot message. *)
  E.keep (E.map (handle_message bus) (Krobot_bus.recv bus));

  (* Kill any running simulator. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "simulator") in

  (* Wait a bit to let the other handler release the connection *)
  lwt () = Lwt_unix.sleep 0.4 in

  (* Initial state of the simulator *)
  let local_sim = {
    state = { x = 0.; y = 0.; theta = 0. };
    (*state_indep = { x = 0.; y = 0.; theta = 0. };*)
    internal_state = { theta_f = 0.; theta_bl = 0.; theta_br = 0. };
    velocity_f = 0.;
    velocity_bl = 0.;
    velocity_br = 0.;
    (*ghost = { x = 0.; y = 0.; theta = 0. };*)
    command_x = Idle;
    command_y = Idle;
    command_theta = Idle;
    lock_ref = 0.;
    lock_error = 0.;
    time = Unix.gettimeofday ();
    omni_limits = 0.3, pi/.4., 0.5, pi/.4.;
  } in
  sim := Some local_sim;

  (* Set the correct simulation mode *)
  if !robot_sim || !sensors_emu then
    (if !hil then
        ignore(Krobot_message.send bus (Unix.gettimeofday (), Set_simulation_mode Sim_HIL))
     else
        ignore(Krobot_message.send bus (Unix.gettimeofday (), Set_simulation_mode Sim_normal))
    );
  (* Launch robot simulation *)
  if !robot_sim then begin
    ignore(send_CAN_messages local_sim bus);
    ignore(loop bus local_sim)
  end;
  (* Launch sensor simulation *)
  if !sensors_emu then begin
  ignore(loop_urg local_sim bus)
  end;

  (* Run forever *)
  let t, _ = Lwt.wait () in
  t
