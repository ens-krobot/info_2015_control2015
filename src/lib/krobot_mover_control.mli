
type state

val make : unit -> state Lwt.t

type goto_result =
  | Goto_success
  | Goto_failure

val goto : state:state -> destination:Krobot_geom.vertice -> (state * goto_result) Lwt.t