open Krobot_bus
open Krobot_message

type world_update =
  | Position_updated
  | Motor_started
  | Motor_stopped
  | New_vertice

type input =
  | Message of Krobot_bus.message
  | World_updated of world_update
  | Timeout

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
  prepared_vertices : (Krobot_geom.vertice * float) list;
}

type state =
  | Transition_to_Moving_to of (Krobot_geom.vertice * float) * ((Krobot_geom.vertice * float) list)
  | Moving_to of (Krobot_geom.vertice * float) * ((Krobot_geom.vertice * float) list)
  | Idle
  | Transition_to_Idle
  | Transition_to_Stop
  | Stop of float

type message =
  | Bus of Krobot_bus.message
  | CAN of Krobot_message.t

type output = {
  timeout : float; (* in seconds *)
  messages : message list;
  state : state;
  world : world;
}

let init_world = {
  robot = {
    position = { x = 0.; y = 0. };
    orientation = 0.;
    motors_moving = false;
  };

  prepared_vertices = [];
}

let init_state = Transition_to_Idle

let name = "demo"

let time_zero = Unix.gettimeofday ()
let current_time () = Unix.gettimeofday () -. time_zero

module Date : sig
  type t
  val add : t -> float -> t
  val now : unit -> t
  val time_to_wait : t -> float
end = struct
  type t = float
  let add t d = t +. d
  let now () = current_time ()
  let time_to_wait dest =
    max 0. (dest -. now ())
end

let update_world : world -> Krobot_bus.message -> (world * input) option =
  fun world message ->
    match message with
    | Kill killed when killed = name ->
      exit 0

    | CAN (_,frame) as message -> begin
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
               World_updated Position_updated)

        | Motor_status (b1, b2, b3, b4) ->
          let r = b1 || b2 || b3 || b4 in
          if world.robot.motors_moving <> r
          then
            let update =
              if r
              then
                let () = Lwt_log.ign_info_f "motor start %f" (current_time ()) in
                Motor_started
              else
                let () = Lwt_log.ign_info_f "motor stop %f" (current_time ()) in
                Motor_stopped
            in
            Some ({ world with
                    robot = { world.robot with motors_moving = r } },
                  World_updated update)
          else
            None
        | _ ->
          Some (world, Message message)
      end
    | Trajectory_set_vertices l ->
      let s = List.map (fun {Krobot_geom.x;y} -> Printf.sprintf "(%f, %f)" x y) l in
      let s = String.concat ", " s in
      Lwt_log.ign_info_f "Set vertice [%s]" s;
      let l = List.map (fun v -> (v, world.robot.orientation)) l in
      Some ({ world with prepared_vertices = l },
            World_updated New_vertice)
    | Trajectory_add_vertice (v, dir) ->
      let theta = match dir with
        | None -> world.robot.orientation
        | Some vect -> Krobot_geom.angle vect
      in
      Lwt_log.ign_info_f "Add vertice (%f, %f, %f)" v.Krobot_geom.x v.Krobot_geom.y theta;
      let s = List.map (fun ({Krobot_geom.x;y}, theta) -> Printf.sprintf "(%f, %f, %f)" x y theta) ((v, theta) :: world.prepared_vertices) in
      let s = String.concat ", " s in
      Lwt_log.ign_info_f "vertice [%s]" s;
      Some ({ world with prepared_vertices = (v, theta) :: world.prepared_vertices },
            World_updated New_vertice)
    | Log _ ->
      None
    | message ->
      Lwt_log.ign_info_f "msg: %s" (string_of_message message);
      Some (world, Message message)

let idle world =
  { timeout = 1.;
    messages = [];
    world;
    state = Idle }

let motor_stop = Motor_stop(0.4, 0.4)
let safe_stop_time = 0.1 (* second *)

let general_step (input:input) (world:world) (state:state) : output =
  match state with
  | Transition_to_Idle ->
    Lwt_log.ign_info_f "Idle";
    idle world
  | Idle -> begin match input with
    | Message Trajectory_go -> begin
        match List.rev world.prepared_vertices with
        | [] ->
          Lwt_log.ign_warning_f "nowhere to go";
          idle world
        | (dest, theta) :: rest ->
          Lwt_log.ign_warning_f "Run Go";
          { timeout = 1.;
            messages = [];
            world = { world with prepared_vertices = [] };
            state = Transition_to_Moving_to ((dest, theta), rest) }
      end
    | Timeout ->
      Lwt_log.ign_info_f "still idle...";
      idle world
    | _ ->
      idle world
  end
  | Transition_to_Moving_to ((dest, theta), rest) -> begin
      let open Krobot_geom in
      Lwt_log.ign_info_f "Moving_to (%f, %f, %f)" dest.x dest.y theta;
      (* let limits = Motor_omni_limits(0.1, 0.25, (pi/.4.), (pi/.8.)) in *)
      let goto = Motor_omni_goto(dest.x, dest.y, theta) in
      { timeout = 1.;
        messages = [CAN goto];
        world;
        state = Moving_to ((dest, theta), rest) }
    end
  | Moving_to (dest_theta, rest) -> begin
      let next_step = function
        | [] -> Idle
        | dest_theta :: rest -> Transition_to_Moving_to (dest_theta, rest) in
      let state =
        match input with
        | World_updated Motor_stopped ->
          next_step rest
        | Timeout when world.robot.motors_moving ->
          Lwt_log.ign_info_f "still moving...";
          Moving_to (dest_theta, rest)
        | Timeout ->
          next_step rest
        | _ -> Moving_to (dest_theta, rest)
      in
      { timeout = safe_stop_time;
        messages = [];
        world;
        state }
    end
  | Transition_to_Stop ->
    Lwt_log.ign_info_f "Stop";
    let t = Unix.gettimeofday () in
    { timeout = 0.01;
      messages = [CAN motor_stop];
      world;
      state = Stop t }
  | Stop stopped ->
    let state, msg =
      match input with
      | Timeout when world.robot.motors_moving ->
        (* If we waited too long: restop *)
        Stop stopped, [CAN motor_stop]
      | Timeout ->
        Idle, []
      | World_updated Motor_stopped ->
        Idle, []
      | _ ->
        Stop stopped, []
    in
    { timeout = safe_stop_time;
      messages = [];
      world;
      state }

let step (input:input) (world:world) (state:state) : output =
  let state =
    match input with
    | Message Stop ->
      Transition_to_Stop
    | _ -> state in
  general_step input world state

type receiver = (float * Krobot_bus.message) option Lwt.t

(* quite wrong: can loose messages *)
let mk_recv ev = Lwt.(Lwt_react.E.next ev >|= fun v -> Some v)

let next_event ev (recv:receiver) timeout =
  lwt () = Lwt_unix.yield () in
  let time = Lwt.(Lwt_unix.sleep timeout >>= fun _ -> return_none) in
  match_lwt Lwt.(time <?> recv) with
  | None -> Lwt.return (recv, None)
  | Some (msg_time, msg) ->
    Lwt.cancel time;
    Lwt.return (mk_recv ev, Some msg)

let send_msg bus time = function
  | Bus m -> Krobot_bus.send bus (time, m)
  | CAN c -> Krobot_bus.send bus (time, CAN (Info, Krobot_message.encode c))

let main_loop bus ev =
  let rec aux recv world state timeout =
    lwt (recv,msg) = next_event ev recv Date.(time_to_wait timeout) in
    let update =
      match msg with
      | None ->
        (* Lwt_log.ign_info_f "timeout"; *)
        Some (world, Timeout)
      | Some m -> update_world world m in
    match update with
    | None ->
      aux recv world state timeout
    | Some (world, input) ->
      let output = step input world state in
      let time = Unix.gettimeofday () in
      lwt () = Lwt_list.iter_s (fun m -> send_msg bus time m) output.messages in
      let timeout_date = Date.(add (now ()) output.timeout) in
      aux recv output.world output.state timeout_date in
  aux (mk_recv ev) init_world init_state (Date.now ())

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
]

let usage = Printf.sprintf "\
Usage: krobot-%s [options]\n\
options are:"
    name

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

lwt () =
  Arg.parse options ignore usage;

  (* Display all informative messages. *)
  Lwt_log.append_rule "*" Lwt_log.Info;

  (* Open the krobot bus. *)
  lwt bus = Krobot_bus.get () in

  (* Fork if not prevented. *)
  if !fork then Krobot_daemon.daemonize bus;

  (* Kill any running vm. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill name) in

  let ev = Krobot_bus.recv bus in
  main_loop bus ev
