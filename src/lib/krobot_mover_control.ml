open Krobot_world_update
open Krobot_bus

type state =
  { world : Krobot_world_update.world;
    bus : Krobot_bus.t;
    stream : (float * Krobot_bus.message) Lwt_stream.t; }

let make () : state Lwt.t =
  lwt bus = Krobot_bus.get () in
  let ev = Krobot_bus.recv bus in
  let stream = Lwt_react.E.to_stream ev in
  let world = Krobot_world_update.init_world in
  Lwt.return { world; bus; stream }

let send state msg =
  Krobot_bus.send state.bus (Unix.gettimeofday (), msg)

let consume_until_mover_message state : (state * mover_message) Lwt.t =
  let rec loop world stream : (world * mover_message) Lwt.t =
    match_lwt Lwt_stream.get stream with
    | None -> raise_lwt (Failure "connection closed")
    | Some (_,msg) ->
      let world =
        match Krobot_world_update.update_world world msg with
        | None -> world
        | Some (world, _world_update) -> world
      in
      match msg with
      | Mover_message mover_message ->
        Lwt.return (world, mover_message)
      | _ ->
        loop world stream
  in
  lwt (world, msg) = loop state.world state.stream in
  Lwt.return ({ state with world }, msg)

type goto_result =
  | Goto_success
  | Goto_failure

let goto ~state ~destination =
  (* TODO: id *)
  lwt () = send state (Goto (0, destination)) in
  lwt (state, msg) = consume_until_mover_message state in
  match msg with
  | Idle ->
    (* TODO: better announcing *)
    Lwt.return (state, Goto_success)
  | _ ->
    Lwt.return (state, Goto_failure)
