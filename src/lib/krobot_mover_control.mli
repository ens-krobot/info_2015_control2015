
type state

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

val move : state:state -> destination:Krobot_geom.vertice -> (state * move_result) Lwt.t
