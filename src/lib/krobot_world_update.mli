type world_update =
  | Position_updated
  | Motor_started
  | Motor_stopped
  | New_vertice
  | Jack_changed
  | Team_changed
  | Emergency_changed

type jack_state =
   | In
   | Out

type emergency_state =
   | Pressed
   | OK

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
  jack : jack_state;
  team : Krobot_bus.team;
  em_stop : emergency_state;
}

val init_world : world

val update_world : world -> Krobot_bus.message -> (world * world_update) option
