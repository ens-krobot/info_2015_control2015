open Krobot_bus
open Krobot_message

type ax12_side =
  | Left
  | Right

type world_update =
  | Position_updated
  | Motor_started
  | Motor_stopped
  | New_vertice
  | Jack_changed
  | Team_changed
  | Emergency_changed
  | Ax12_changed of ax12_side

type jack_state =
   | In
   | Out

type emergency_state =
   | Pressed
   | OK

type ax12_state = {
  position : int;
  speed : int;
  torque : int;
}

type robot = {
  position : Krobot_geom.vertice;
  (* The position of the robot on the table. *)
  orientation : float;
  (* The orientation of the robot. *)
  motors_moving : bool;
  (* Are motors moving ? *)
  left_ax12_state : ax12_state;
  right_ax12_state : ax12_state;
}

type world = {
  robot : robot;
  jack : jack_state;
  team : Krobot_bus.team;
  em_stop : emergency_state;
}

let default_ax12_state =
  { position = 0; speed = 0; torque = 0 }

let init_world = {
  robot = {
    position = { x = 0.; y = 0. };
    orientation = 0.;
    motors_moving = false;
    left_ax12_state = default_ax12_state;
    right_ax12_state = default_ax12_state;
  };
  jack = In;
  team = Krobot_bus.Yellow;
  em_stop = Pressed;
}

let ax12_state_of_side world = function
  | Left -> world.robot.left_ax12_state
  | Right -> world.robot.right_ax12_state

let update_world : world -> Krobot_bus.message -> (world * world_update) option =
  fun world message ->
    match message with

    | CAN (_,frame) -> begin
        match decode frame with
        | Odometry(x, y, theta) ->
          let open Krobot_geom in
          let position = { x; y } in
          let orientation = math_mod_float theta (2. *. pi) in
          if position = world.robot.position (* maybe add a threshold ? *)
             && orientation = world.robot.orientation
          then None
          else
            Some
              ({ world with
                 robot = { world.robot with position;orientation } },
               Position_updated)

        | Motor_status (b1, b2, b3, b4) ->
          let r = b1 || b2 || b3 || b4 in
          if world.robot.motors_moving <> r
          then
            let update =
              if r
              then
                (* let () = Lwt_log.ign_info_f "motor start %f" (current_time ()) in *)
                Motor_started
              else
                (* let () = Lwt_log.ign_info_f "motor stop %f" (current_time ()) in *)
                Motor_stopped
            in
            Some ({ world with
                    robot = { world.robot with motors_moving = r } },
                  update)
          else
            None

        | Switch1_status(new_jack, new_team, new_em_stop, _, _, _, _, _)  ->
          (* Did the emergency button changed ? *)
          let new_em_stop = if new_em_stop then Pressed else OK in
          if new_em_stop <> world.em_stop then
            Some ({ world with
                    em_stop = new_em_stop},
                  Emergency_changed)
          else
            (* Did the jack changed ? *)
            let new_jack = if new_jack then Out else In in
            if new_jack <> world.jack then
              Some ({world with
                     jack = new_jack},
                    Jack_changed)
            else
              (* Did the team changed ? *)
              let new_team = if new_team then Yellow else Green in
              if new_team <> world.team then
                Some ({world with
                       team = new_team},
                      Team_changed)
              else
                None
        | (Ax12_State (id, position, speed, torque)) -> begin
            let ax12_state = { position; speed; torque } in
            if id = Krobot_config.right_arm_idx then
              if world.robot.right_ax12_state <> ax12_state then
                Some ({ world with
                        robot = { world.robot with right_ax12_state = ax12_state } },
                      Ax12_changed Right)
              else
                None
            else if id = Krobot_config.left_arm_idx then
              if world.robot.left_ax12_state <> ax12_state then
                Some ({ world with
                        robot = { world.robot with left_ax12_state = ax12_state } },
                      Ax12_changed Left)
              else
                None
            else None
          end
        | _ ->
          None
      end
    | Log _ ->
      None
    | message ->
      (* Lwt_log.ign_info_f "msg: %s" (string_of_message message); *)
      None
