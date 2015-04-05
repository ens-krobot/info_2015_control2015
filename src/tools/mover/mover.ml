open Krobot_bus
open Krobot_message
open Krobot_geom

type world_update =
  | Position_updated
  | Beacons_updated
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
  obstacles : Krobot_rectangle_path.obstacle list;
  beacons : Krobot_geom.vertice list;
}

type state =
  | Transition_to_Moving_to of (Krobot_geom.vertice * float) * ((Krobot_geom.vertice * float) list)
  | Moving_to of (Krobot_geom.vertice * float) * ((Krobot_geom.vertice * float) list)
  | Idle
  | Transition_to_Idle
  | Transition_to_Stop
  | Transition_to_Goto of Krobot_geom.vertice * bool
  | Stop of float

type message =
  | Bus of Krobot_bus.mover_message
  | CAN of Krobot_message.t
  | Msg of Krobot_bus.message

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
  obstacles = Krobot_config.fixed_obstacles;
  beacons = [];
}

let init_state = Transition_to_Idle

let name = "mover"

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

let generate_path_display world waypoints =
  let _, curves = List.fold_left (fun (prev_vert,curves) (vert,_) ->
    let dir = vector prev_vert vert in
    let cp1 = translate prev_vert (dir *| 0.1) in
    let cp2 = translate vert (dir *| -0.1) in
    vert, (Bezier.of_vertices prev_vert cp1 cp2 vert) :: curves )
    (world.robot.position,[])
    waypoints
  in
  curves

let update_world : world -> Krobot_bus.message -> ((world * input) option) * (message list) =
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
          then None, []
          else
            Some
              ({ world with
                 robot = { world.robot with position;orientation } },
               World_updated Position_updated),
            []

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
                  World_updated update),
            []
          else
            None, []

        | Beacon_position (a1, a2, d1, d2) ->
          let new_beacons = List.fold_left (fun beacons (d,a) ->
            if d > 0.001 then
              let delta = vector_of_polar d (a +. world.robot.orientation) in
              (translate world.robot.position delta) :: beacons
            else
              beacons)
            []
            [(d1,a1);(d2,a2)]
          in
          if (List.length world.beacons) == (List.length new_beacons) then
            let total_diff = List.fold_left2 (fun t_diff beacon new_beacon ->
              t_diff +. (square_distance beacon new_beacon))
              0.
              world.beacons
              new_beacons
            in
            if total_diff > (0.05 *. 0.05) then
              Some ({ world with
                      beacons = new_beacons},
                    World_updated Beacons_updated),
              []
            else
              None, []
          else
            Some ({ world with
                    beacons = new_beacons},
                  World_updated Beacons_updated),
            []
        | _ ->
          Some (world, Message message), []
      end
    | Trajectory_set_vertices l ->
      let s = List.map (fun {Krobot_geom.x;y} -> Printf.sprintf "(%f, %f)" x y) l in
      let s = String.concat ", " s in
      Lwt_log.ign_info_f "Set vertice [%s]" s;
      let l = List.map (fun v -> (v, world.robot.orientation)) l in
      Some ({ world with prepared_vertices = l },
            World_updated New_vertice),
      [Msg (Trajectory_path (generate_path_display world (List.rev l)))]
    | Trajectory_add_vertice (v, dir) ->
      let theta = match dir with
        | None -> world.robot.orientation
        | Some vect -> Krobot_geom.angle vect
      in
      Lwt_log.ign_info_f "Add vertice (%f, %f, %f)" v.Krobot_geom.x v.Krobot_geom.y theta;
      let s = List.map (fun ({Krobot_geom.x;y}, theta) -> Printf.sprintf "(%f, %f, %f)" x y theta) ((v, theta) :: world.prepared_vertices) in
      let s = String.concat ", " s in
      Lwt_log.ign_info_f "vertice [%s]" s;
      let new_vertices = (v, theta) :: world.prepared_vertices in
      Some ({ world with prepared_vertices = new_vertices },
            World_updated New_vertice),
      [Msg (Trajectory_path (generate_path_display world (List.rev new_vertices)))]
    | Log _ ->
      None, []
    | message ->
      Lwt_log.ign_info_f "msg: %s" (string_of_message message);
      Some (world, Message message), []

let idle ~notify world =
  { timeout = 1.;
    messages =
      if notify
      then [Bus Idle]
      else [];
    world;
    state = Idle }

let motor_stop = Motor_stop(0.4, 0.4)
let safe_stop_time = 0.1 (* second *)

let general_step (input:input) (world:world) (state:state) : output =
  match state with
  | Transition_to_Idle ->
    Lwt_log.ign_info_f "Idle";
    idle ~notify:true world
  | Idle -> begin match input with
    | Message (Goto dest) ->
      { timeout = 0.01;
        messages = [];
        world;
        state = Transition_to_Goto (dest, true) }
    | Message Trajectory_find_path -> begin
        match world.prepared_vertices with
        | [] ->
          Lwt_log.ign_warning_f "nowhere to go";
          idle ~notify:true world
        | (dest, _) :: _ ->
          Lwt_log.ign_warning_f "Path finding...";
          { timeout = 0.01;
            messages = [];
            world = { world with prepared_vertices = [] };
            state = Transition_to_Goto (dest, false) }
      end
    | Message Trajectory_go -> begin
        match List.rev world.prepared_vertices with
        | [] ->
          Lwt_log.ign_warning_f "nowhere to go";
          idle ~notify:true world
        | (dest, theta) :: rest ->
          Lwt_log.ign_warning_f "Run Go";
          { timeout = 0.1;
            messages = [];
            world = { world with prepared_vertices = [] };
            state = Transition_to_Moving_to ((dest, theta), rest) }
      end
    | Timeout ->
      Lwt_log.ign_info_f "still idle...";
      idle ~notify:true world
    | _ ->
      idle ~notify:false world
  end
  | Transition_to_Moving_to ((dest, theta), rest) -> begin
      let open Krobot_geom in
      Lwt_log.ign_info_f "Moving_to (%f, %f, %f)" dest.x dest.y theta;
      (* let limits = Motor_omni_limits(0.1, 0.25, (pi/.4.), (pi/.8.)) in *)
      let goto = Motor_omni_goto(dest.x, dest.y, theta) in
      { timeout = 0.1;
        messages = [CAN goto; Msg (Trajectory_path (generate_path_display world ((dest, theta)::rest)))];
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
        | World_updated Beacons_updated ->
          Lwt_log.ign_warning_f "New beacons while moving...";
          Moving_to (dest_theta, rest)
        | Timeout when world.robot.motors_moving ->
          Lwt_log.ign_info_f "still moving...";
          Moving_to (dest_theta, rest)
        | Timeout ->
          next_step rest
        | _ -> Moving_to (dest_theta, rest)
      in
      let messages =
        match state with
        | Idle -> [Msg (Trajectory_path [])]
        | _ -> []
      in
      { timeout = safe_stop_time;
        messages = messages;
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
  | Transition_to_Goto (dest, move) ->
    Lwt_log.ign_info_f "Goto";
    if dest.x < Krobot_config.robot_radius +. Krobot_config.safety_margin ||
       dest.x > Krobot_config.world_width -. Krobot_config.robot_radius -. Krobot_config.safety_margin ||
       dest.y < Krobot_config.robot_radius +. Krobot_config.safety_margin ||
       dest.y > Krobot_config.world_height -. Krobot_config.robot_radius -. Krobot_config.safety_margin
    then begin
      Lwt_log.ign_warning_f "Pathfinding error: destination out of game area";
      { timeout = 0.01;
        messages = [Bus Planning_error];
        world = {world with prepared_vertices = []};
        state = Transition_to_Idle }
    end
    else
      let beacon_obstacles = List.fold_left (fun obstacles beacon ->
        let dir = normalize { vx = 1.; vy = 1.} in
        let radius = (sqrt 2.) *. Krobot_config.beacon_radius in
        (translate beacon (dir *| radius),
         translate beacon (dir *| (-. radius))) :: obstacles)
        []
        world.beacons
      in
      let path =
        Krobot_rectangle_path.find_path
          ~src:world.robot.position
          ~dst:dest
          ~obstacles:(beacon_obstacles @ world.obstacles) in
      begin match path with
        | [] ->
          Lwt_log.ign_warning_f "Pathfinding error: destination unreachable";
          { timeout = 0.01;
            messages = [Bus Planning_error];
            world = {world with prepared_vertices = []};
            state = Transition_to_Idle }
        | h::t ->
          let theta = world.robot.orientation in
          let rest = List.map (fun v -> v, theta) t in
          if move then
            { timeout = 0.01;
              messages = [Bus Planning_done];
              world = {world with prepared_vertices = []};
              state = Transition_to_Moving_to ((h, theta), rest) }
          else
            { timeout = 0.01;
              messages = [Bus Planning_done; Msg (Trajectory_path (generate_path_display world ((h, theta)::rest)))];
              world = {world with prepared_vertices = List.rev ((h,theta)::rest)};
              state = Transition_to_Idle }
      end
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
  | Bus m -> Krobot_bus.send bus (time, Mover_message m)
  | CAN c -> Krobot_bus.send bus (time, CAN (Info, Krobot_message.encode c))
  | Msg m -> Krobot_bus.send bus (time, m)

let main_loop bus ev =
  let rec aux recv world state timeout =
    lwt (recv,msg) = next_event ev recv Date.(time_to_wait timeout) in
    let update, update_messages =
      match msg with
      | None ->
        (* Lwt_log.ign_info_f "timeout"; *)
        Some (world, Timeout), []
      | Some m -> update_world world m in
    match update with
    | None ->
      aux recv world state timeout
    | Some (world, input) ->
      let output = step input world state in
      let time = Unix.gettimeofday () in
      lwt () = Lwt_list.iter_s (fun m -> send_msg bus time m) update_messages in
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
