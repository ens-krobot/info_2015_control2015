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

val init_world : world

val update_world : world -> Krobot_bus.message -> (world * world_update) option
