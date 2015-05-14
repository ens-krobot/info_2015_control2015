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
  | Obstacles_updated
  | Beacons_updated

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
  urg_obstacles : Krobot_rectangle_path.obstacle list;
  beacons : Krobot_geom.vertice list;
}

val init_world : world

val update_world : world -> Krobot_bus.message -> (world * world_update) option

val ax12_state_of_side : world -> ax12_side -> ax12_state
