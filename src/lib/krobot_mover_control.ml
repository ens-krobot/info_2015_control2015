open Krobot_world_update
open Krobot_bus

type state =
  { world : Krobot_world_update.world;
    bus : Krobot_bus.t;
    stream : (float * Krobot_bus.message) Lwt_stream.t;
    next_request_id : request_id }

let inner_make () : state Lwt.t =
  lwt bus = Krobot_bus.get () in
  let ev = Krobot_bus.recv bus in
  let stream = Lwt_react.E.to_stream ev in
  let world = Krobot_world_update.init_world in
  Random.self_init ();
  let next_request_id = Random.int 10000000 in
  Lwt.return { world; bus; stream; next_request_id }

let send state msg =
  Krobot_bus.send state.bus (Unix.gettimeofday (), msg)

let send_can state msg =
  Krobot_bus.send state.bus (Unix.gettimeofday (), CAN (Info, Krobot_message.encode msg))

let mover_message_id = function
  | Escaping _
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

exception Match_end_exn

let test_end = function
  | Match_end -> raise Match_end_exn
  | _ -> ()

let consume_and_update state =
  try
    let l = Lwt_stream.get_available state.stream in
    let r =
      List.fold_left (fun state (_,msg) ->
        test_end msg;
        match Krobot_world_update.update_world state.world msg with
        | None -> state
        | Some (world, _) -> { state with world })
        state l
    in
    Lwt.return r
  with e -> raise_lwt e

let stream_get stream =
  match_lwt Lwt_stream.get stream with
  | None -> raise_lwt (Failure "connection closed")
  | Some (_,Match_end) -> raise_lwt Match_end_exn
  | Some (_, msg) -> Lwt.return msg

let consume_until_mover_message (until:request_until) state : (state * mover_message) Lwt.t =
  let rec loop world stream : (world * mover_message) Lwt.t =
    lwt msg = stream_get stream in
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

let consume_until_ax12_state ~ax12_side state : (state * ax12_state) Lwt.t =
  let rec loop world stream : (world * ax12_state) Lwt.t =
    lwt msg = stream_get stream in
    match Krobot_world_update.update_world world msg with
    | Some (world, Ax12_changed side) when side = ax12_side ->
      Lwt.return (world, ax12_state_of_side world side)
    | Some (world, _) ->
      loop world stream
    | None ->
      loop world stream
  in
  lwt (world, ax12_state) = loop state.world state.stream in
  Lwt.return ({ state with world }, ax12_state)

type goto_result =
  | Goto_success
  | Goto_failure
  | Goto_unreachable

let new_request_id state =
  lwt state = consume_and_update state in
  Lwt.return
    (state.next_request_id,
     { state with next_request_id = state.next_request_id + 1 })

let wait_idle state =
  lwt state = consume_and_update state in
  lwt () = send state Request_mover_state in
  lwt (state, _) = consume_until_mover_message Idle state in
  Lwt.return state

let goto ~state ~destination =
  lwt state = wait_idle state in
  lwt request_id, state = new_request_id state in
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

type turn_result =
  | Turn_success
  | Turn_failure

let turn ~state ~orientation =
  lwt state = wait_idle state in
  lwt request_id, state = new_request_id state in
  let order = state.world.robot.position, Some orientation in
  lwt () = send state (Trajectory_set_vertices [order]) in
  lwt () = send state (Trajectory_go (request_id, Normal)) in
  let rec loop () =
    lwt (state, msg) = consume_until_mover_message (Id request_id) state in
    Printf.printf "msg: %s\n%!" (Krobot_bus.string_of_message (Mover_message msg));
    match msg with
    | Request_completed _ ->
      Lwt.return (state, Turn_success)
    | _ ->
      Lwt.return (state, Turn_failure)
  in
  loop ()


type move_result =
  | Move_success
  | Move_failure

let move ~state ~ignore_fixed_obstacles ~destination =
  lwt state = wait_idle state in
  lwt request_id, state = new_request_id state in
  let order = destination, None in
  let kind = if ignore_fixed_obstacles then Direct else Normal in
  lwt () = send state (Trajectory_set_vertices [order]) in
  lwt () = send state (Trajectory_go (request_id, kind)) in
  let rec loop () =
    lwt (state, msg) = consume_until_mover_message (Id request_id) state in
    Printf.printf "msg: %s\n%!" (Krobot_bus.string_of_message (Mover_message msg));
    match msg with
    | Request_completed _ ->
      Lwt.return (state, Move_success)
    | _ ->
      Lwt.return (state, Move_failure)
  in
  loop ()


type clap_result = unit
type clap_status = Clap_in | Clap_out

let (left_arm_in, _, _, left_arm_out) = Krobot_config.left_arm_positions
let (right_arm_in, _, _, right_arm_out) = Krobot_config.right_arm_positions

let ax12_action side status = match side, status with
  | Left, Clap_out -> Krobot_config.left_arm_idx, left_arm_out
  | Left, Clap_in -> Krobot_config.left_arm_idx, left_arm_in
  | Right, Clap_out -> Krobot_config.left_arm_idx, left_arm_out
  | Right, Clap_in -> Krobot_config.left_arm_idx, left_arm_in

let admissible_ax12_distance = 10

let clap ~state ~side ~status =
  lwt state = consume_and_update state in
  let idx, position = ax12_action side status in
  lwt () = send_can state (Ax12_Goto (idx, position, 500)) in
  lwt () = Lwt_unix.sleep 0.1 in
  lwt () = send_can state (Ax12_Request_State idx) in
  let rec loop state =
    lwt (state, { position = state_position }) = consume_until_ax12_state ~ax12_side:side state in
    if abs (state_position - position) < admissible_ax12_distance then
      Lwt.return (state, ())
    else
      lwt () = Lwt_unix.sleep 0.05 in
      lwt () = send_can state (Ax12_Request_State idx) in
      loop state
  in
  loop state

let wait_for_jack' ~jack_state ~(state:state) : state Lwt.t =
  let rec loop (world:world) stream : world Lwt.t =
    lwt msg = stream_get stream in
    match Krobot_world_update.update_world world msg with
    | Some (world, Jack_changed) when world.jack = jack_state ->
      Lwt.return world
    | Some (world, _) ->
      loop world stream
    | _ ->
      loop world stream
  in
  lwt state = consume_and_update state in
  if state.world.jack = jack_state then
    Lwt.return state
  else
    lwt world = loop state.world state.stream in
    Lwt.return { state with world }

let wait_for_jack ~jack_state ~state =
  lwt state = wait_for_jack' ~jack_state ~state in
  lwt () = Lwt_unix.sleep 0.1 in
  wait_for_jack' ~jack_state ~state

let wait_for_team_change ~state : (state * Krobot_bus.team) Lwt.t =
  let rec loop (world:world) stream : (world * Krobot_bus.team) Lwt.t =
    lwt msg = stream_get stream in
    match Krobot_world_update.update_world world msg with
    | Some (world, Team_changed) ->
      Lwt.return (world, world.team)
    | Some (world, _) ->
      loop world stream
    | _ ->
      loop world stream
  in
  lwt (world, team) = loop state.world state.stream in
  Lwt.return ({ state with world }, team)

let close world position =
  match position with
  | None -> true
  | Some position -> Krobot_geom.distance world.robot.position position <= 0.01

let wait_for_odometry ~state ~position =
  let rec loop (world:world) stream : world Lwt.t =
    lwt msg = stream_get stream in
    match Krobot_world_update.update_world world msg with
    | Some (world, _) when close world position ->
      Lwt.return world
    | Some (world, _) ->
      loop world stream
    | _ ->
      loop world stream
  in
  lwt state = consume_and_update state in
  lwt world = loop state.world state.stream in
  Lwt.return { state with world }

let send_team_initial_position state =
  let (pos, theta) =
    match state.world.team with
    | Green -> Krobot_config.green_initial_position
    | Yellow -> Krobot_config.yellow_initial_position
  in
  lwt () = send_can state (Set_odometry (pos.x, pos.y, theta)) in
  Lwt.return pos

let reset_odometry ~state =
  lwt state = consume_and_update state in
  lwt position = send_team_initial_position state in
  wait_for_odometry ~state ~position:(Some position)

let get_team state = state.world.team

let on_match_end state callback =
  let ev = Lwt_react.E.map_s (function
    | (_, Match_end) ->
      Printf.printf "match end\n%!";
      callback state.bus
    | _ -> Lwt.return ())
    (Krobot_bus.recv state.bus)
  in
  Lwt_react.E.keep ev

let stop state = send state Stop

let lcd_message ~state ~line ~text =
  Krobot_lcd.send_line state.bus line text

let wait_for_some_odometry ~state =
  wait_for_odometry ~state ~position:None

(* exported state: we wait for an initial state *)
let make () =
  lwt state = inner_make () in
  Lwt.return state
  (* wait_for_some_odometry ~state *)
