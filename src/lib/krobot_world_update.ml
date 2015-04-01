open Krobot_bus
open Krobot_message

type world_update =
  | Position_updated
  | Motor_started
  | Motor_stopped
  | New_vertice

type robot = {
  position : Krobot_geom.vertice;
  (* The position of the robot on the table. *)
  orientation : float;
  (* The orientation of the robot. *)
  motors_moving : bool;
  (* Are motors moving ? *)
}

type world = {
  robot : robot;
}

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
        | _ ->
          None
      end
    | Log _ ->
      None
    | message ->
      (* Lwt_log.ign_info_f "msg: %s" (string_of_message message); *)
      None
