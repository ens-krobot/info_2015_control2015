open Krobot_world_update
open Krobot_bus

type state =
  { world : Krobot_world_update.world;
    bus : Krobot_bus.t;
    stream : (float * Krobot_bus.message) Lwt_stream.t;
    next_request_id : request_id }

let make () : state Lwt.t =
  lwt bus = Krobot_bus.get () in
  let ev = Krobot_bus.recv bus in
  let stream = Lwt_react.E.to_stream ev in
  let world = Krobot_world_update.init_world in
  Random.self_init ();
  let next_request_id = Random.int 10000000 in
  Lwt.return { world; bus; stream; next_request_id }

let send state msg =
  Krobot_bus.send state.bus (Unix.gettimeofday (), msg)

let mover_message_id = function
  | First_obstacle _
  | Not_idle _
  | Idle -> None
  | Planning_error id
  | Planning_done id
  | Collision id
  | Request_completed id -> Some id

type request_until =
  | Idle
  | Id of request_id

let consume_until_mover_message (until:request_until) state : (state * mover_message) Lwt.t =
  let rec loop world stream : (world * mover_message) Lwt.t =
    match_lwt Lwt_stream.get stream with
    | None -> raise_lwt (Failure "connection closed")
    | Some (_,msg) ->
      let world =
        match Krobot_world_update.update_world world msg with
        | None -> world
        | Some (world, _world_update) -> world
      in
      match msg, until with
      | Mover_message Idle, Idle ->
        Lwt.return (world, (Idle:mover_message))
      | Mover_message (Not_idle _), Idle ->
        lwt () = send state Request_mover_state in
        loop world stream
      | Mover_message mover_message, Id id ->
        begin match mover_message_id mover_message with
          | Some id' when id' = id ->
            Lwt.return (world, mover_message)
          | _ -> loop world stream
        end
      | _ ->
        loop world stream
  in
  lwt (world, msg) = loop state.world state.stream in
  Lwt.return ({ state with world }, msg)

type goto_result =
  | Goto_success
  | Goto_failure
  | Goto_unreachable

let new_request_id state =
  state.next_request_id,
  { state with next_request_id = state.next_request_id + 1 }

let wait_idle state =
  let _ : _ list = Lwt_stream.get_available state.stream in
  lwt () = send state Request_mover_state in
  lwt (state, _) = consume_until_mover_message Idle state in
  Lwt.return state

let goto ~state ~destination =
  lwt state = wait_idle state in
  let request_id, state = new_request_id state in
  let _ : _ list = Lwt_stream.get_available state.stream in
  lwt () = send state (Goto (request_id, destination)) in
  let rec loop () =
    lwt (state, msg) = consume_until_mover_message (Id request_id) state in
    Printf.printf "msg: %s\n%!" (Krobot_bus.string_of_message (Mover_message msg));
    match msg with
    | Planning_done _ ->
      loop ()
    | Request_completed _ ->
      Lwt.return (state, Goto_success)
    | Planning_error _ ->
      Lwt.return (state, Goto_unreachable)
    | _ ->
      Lwt.return (state, Goto_failure)
  in
  loop ()
