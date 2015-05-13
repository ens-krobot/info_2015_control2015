
type state

exception Match_end_exn

val make : unit -> state Lwt.t

type goto_result =
  | Goto_success
  | Goto_failure
  | Goto_unreachable

val goto : state:state -> destination:Krobot_geom.vertice -> (state * goto_result) Lwt.t

type turn_result =
  | Turn_success
  | Turn_failure

val turn : state:state -> orientation:float -> (state * turn_result) Lwt.t

type move_result =
  | Move_success
  | Move_failure

val move : state:state -> ignore_fixed_obstacles:bool ->
  destination:Krobot_geom.vertice -> (state * move_result) Lwt.t

type clap_result = unit
type clap_status = Clap_in | Clap_out

val clap : state:state -> side:Krobot_world_update.ax12_side -> status:clap_status ->
  (state * clap_result) Lwt.t

val wait_for_jack : jack_state:Krobot_world_update.jack_state -> state:state -> state Lwt.t

val wait_for_team_change : state:state -> (state * Krobot_bus.team) Lwt.t

val reset_odometry : state:state -> state Lwt.t

val get_team : state -> Krobot_bus.team

val on_match_end : state -> (Krobot_bus.t -> unit Lwt.t) -> unit

val stop : state -> unit Lwt.t

val lcd_message : state:state -> line:int -> text:string -> unit Lwt.t
