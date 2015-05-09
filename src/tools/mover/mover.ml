open Krobot_bus
open Krobot_message
open Krobot_geom

type world_update =
  | Position_updated
  | Beacons_updated
  | Obstacles_updated
  | Target_lock_updated
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
  urg_obstacles : Krobot_rectangle_path.obstacle list;
  beacons : Krobot_geom.vertice list;
  manage_theta : bool;
}

type first_obstacle = Krobot_geom.vertice option

type moving_to = {
  move_kind : move_kind;
  first_obstacle : vertice option;
  next : Krobot_geom.vertice * float;
  rest : (Krobot_geom.vertice * float * move_kind) list;
}

type start_moving_to = {
  original_position : vertice;
  start_motor_stopped : bool;
  moving_to : moving_to;
}

type state =
  | Transition_to_Moving_to of (Krobot_geom.vertice * float * move_kind) * ((Krobot_geom.vertice * float * move_kind) list)
  | Start_moving_to of start_moving_to
  | Moving_to of moving_to
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
  urg_obstacles = [];
  beacons = [];
  manage_theta = true;
}

let init_state = Transition_to_Idle

let distance_before_stop = 0.6 (* stop if half a metter before collision *)
let distance_before_handling_obstacle = distance_before_stop +. 0.1
let close_distance_from_destination = 0.01

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

let not_fixed_obstacles world =
  let beacon_obstacles = List.fold_left (fun obstacles beacon ->
    let dir = normalize { vx = 1.; vy = 1.} in
    let radius = (sqrt 2.) *. Krobot_config.beacon_radius in
    (translate beacon (dir *| radius),
     translate beacon (dir *| (-. radius))) :: obstacles)
    []
    world.beacons
  in
  (beacon_obstacles @ world.urg_obstacles)

let obstacles world =
  Krobot_config.fixed_obstacles @ not_fixed_obstacles world

let update_world : world -> Krobot_bus.message -> ((world * input) option) * (message list) =
  fun world message ->
    match message with

    | CAN (_,frame) as message -> begin
        match decode frame with
        | Lock_target _ ->
          Some ({ world with
                  manage_theta = false},
                World_updated Target_lock_updated),
          []
        | Unlock_target ->
          Some ({ world with
                  manage_theta = true},
                World_updated Target_lock_updated),
          []
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
          let r =
            if world.manage_theta then
              b1 || b2 || b3
            else
              b1 || b2
          in
          if world.robot.motors_moving <> r
          then
            let update =
              if r
              then
                let () =
                  Lwt_log.ign_info_f "motor start %a"
                    Krobot_date.pr (Krobot_date.now ())
                in
                Motor_started
              else
                let () =
                  Lwt_log.ign_info_f "motor stop %a"
                    Krobot_date.pr (Krobot_date.now ())
                in
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
    | Urg _ -> None, []
    | Obstacles obstacles ->
      let world =
        { world with
          urg_obstacles = List.map (fun (Rectangle (v1, v2)) -> (v1, v2)) obstacles } in
      Some (world, World_updated Obstacles_updated), []
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
let started_distance = 0.005

let drop_kind l = List.map (fun (d,t,_) -> (d,t)) l

let rec general_step (input:input) (world:world) (state:state) : output =
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
    | Message (Trajectory_go kind) -> begin
        match List.rev world.prepared_vertices with
        | [] ->
          Lwt_log.ign_warning_f "nowhere to go";
          idle ~notify:true world
        | (dest, theta) :: rest ->
          let rest = List.map (fun (d, t) -> (d, t, kind)) rest in
          Lwt_log.ign_warning_f "Run Go";
          { timeout = 0.1;
            messages = [];
            world = { world with prepared_vertices = [] };
            state = Transition_to_Moving_to ((dest, theta, kind), rest) }
      end
    | Timeout ->
      Lwt_log.ign_info_f "still idle...";
      idle ~notify:true world
    | _ ->
      idle ~notify:false world
  end
  | Transition_to_Moving_to ((dest, theta, move_kind), rest) -> begin
      let open Krobot_geom in
      let date = Krobot_date.now () in
      Lwt_log.ign_info_f "Start_moving_to (%f, %f, %f) %a" dest.x dest.y theta
        Krobot_date.pr date;
      (* let limits = Motor_omni_limits(0.1, 0.25, (pi/.4.), (pi/.8.)) in *)
      let goto = Motor_omni_goto(dest.x, dest.y, theta) in
      let command_of_limits { Krobot_config.v_lin_max; v_rot_max;
                              a_lin_max; a_rot_max; torque_limit } =
        [CAN (Motor_omni_limits (v_lin_max, v_rot_max, a_lin_max, a_rot_max));
         CAN (Drive_torque_limit torque_limit)]
      in
      let limit_command =
        match move_kind with
        | Constrained -> command_of_limits Krobot_config.constrained_limits
        | Normal
        | Direct -> command_of_limits Krobot_config.normal_limits
      in
      let display_path = (dest, theta) :: drop_kind rest in
      { timeout = 0.1;
        messages =
          limit_command @
          [CAN goto; Msg (Trajectory_path (generate_path_display world display_path))];
        world;
        state =
          Start_moving_to
            { original_position = world.robot.position;
              start_motor_stopped = world.robot.motors_moving;
              moving_to =
                { first_obstacle = Some world.robot.position;
                  (* force testing for intersections *)
                  next = (dest, theta);
                  rest;
                  move_kind; } } }
    end
  | Start_moving_to { original_position; start_motor_stopped; moving_to } -> begin
      let date = Krobot_date.now () in
      let nothing = { timeout = 0.1; messages = []; world; state } in
      let move_kind =
        match moving_to.move_kind with
        | Constrained -> "constrained"
        | Normal -> ""
        | Direct -> "direct"
      in
      match input with
      | World_updated Position_updated ->
        if Krobot_geom.distance original_position world.robot.position >= started_distance
        then begin
          let (dest, theta) = moving_to.next in
          Lwt_log.ign_info_f "Moving_to %s (%f, %f, %f) %a"
            move_kind
            dest.x dest.y theta
            Krobot_date.pr date;
          general_step input world (Moving_to moving_to)
        end
        else
          nothing
      | World_updated Motor_stopped ->
        if start_motor_stopped then begin
          Lwt_log.ign_info_f "Moving_to %s: Motor stopped %a"
            move_kind Krobot_date.pr date;
          general_step input world (Moving_to moving_to)
        end
        else
          nothing
      | World_updated Motor_started -> begin
          Lwt_log.ign_info_f "Moving_to %s: Motor started %a"
            move_kind Krobot_date.pr date;
          general_step input world (Moving_to moving_to)
        end
      | Timeout ->
        if Krobot_geom.distance world.robot.position (fst moving_to.next)
           <= close_distance_from_destination
        then begin
          Lwt_log.ign_info_f "Already arrived %s %a"
            move_kind Krobot_date.pr date;
          general_step input world (Moving_to moving_to)
        end
        else begin
          Lwt_log.ign_info_f "Still starting %s %a"
            move_kind Krobot_date.pr date;
          nothing
        end
      | _ ->
        nothing
    end
  | Moving_to ({ first_obstacle; next = dest_theta; rest;
                 move_kind } as moving_to) -> begin
      let next_step = function
        | [] -> Idle, [Msg (Trajectory_path [])]
        | (dest, theta, move_kind) :: rest ->
          Transition_to_Moving_to ((dest, theta, move_kind), rest), [] in
      let close_to_first_obstacle = match first_obstacle with
        | None -> false
        | Some first_obstacle ->
          Krobot_geom.distance world.robot.position first_obstacle
          <= distance_before_handling_obstacle
      in
      let date = Krobot_date.now () in
      let handle_collision () =
        (* Lwt_log.ign_warning_f "Collision handling"; *)
        let first_intersection =
          match move_kind with
          | Constrained ->
            (* In constrained_move we ignore the collisions.
               We limited the torque to avoid having too much problems *)
            None
          | Normal ->
            Krobot_rectangle_path.first_collision
              ~src:world.robot.position
              ~path:(List.map fst (dest_theta :: drop_kind rest))
              ~obstacles:(obstacles world)
          | Direct ->
            (* Direct move ignore fixed obstacles *)
            Krobot_rectangle_path.first_collision
              ~src:world.robot.position
              ~path:(List.map fst (dest_theta :: drop_kind rest))
              ~obstacles:(not_fixed_obstacles world)
        in
        match first_intersection with
        | None ->
          (* Lwt_log.ign_warning_f "No collision"; *)
          Moving_to moving_to,
          [Bus (First_obstacle None)]
        | Some { Krobot_rectangle_path.distance; collision } ->
          if distance >= distance_before_stop then begin
            Lwt_log.ign_warning_f "Far collision %f" distance;
            (* If this is too far, just ignore it *)
            Moving_to { moving_to with first_obstacle = Some collision; },
            [Bus (First_obstacle (Some collision))]
          end
          else begin
            Lwt_log.ign_warning_f "Collision";
            Transition_to_Stop, [Bus Collision]
          end
      in
      let state, messages =
        match input with
        | World_updated Position_updated when close_to_first_obstacle ->
          handle_collision ()
        | World_updated Obstacles_updated -> begin
            handle_collision ()
          end
        | World_updated Beacons_updated -> begin
            Lwt_log.ign_warning_f "New beacons while moving...";
            handle_collision ()
          end
        | World_updated Motor_stopped ->
          Lwt_log.ign_info_f "Motor_stopped %a" Krobot_date.pr date;
          next_step rest
        | Timeout when world.robot.motors_moving ->
          (* Lwt_log.ign_info_f "still moving..."; *)
          Moving_to moving_to, []
        | Timeout ->
          Lwt_log.ign_info_f "Timeout with motor stopped %a" Krobot_date.pr date;
          next_step rest
        | World_updated (Position_updated | Motor_started |
                         Target_lock_updated | New_vertice )
        | Message _
          -> Moving_to moving_to, []
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
    Lwt_log.ign_info_f "Goto %b" move;
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
      let path =
        Krobot_rectangle_path.colliding_pathfinding
          ~src:world.robot.position
          ~dst:dest
          ~inflate:Krobot_config.pathfinding_width_inflate
          ~obstacles:(obstacles world) in
      let go world h t ~constrained_move =
        let theta = world.robot.orientation in
        let rest = List.map (fun v -> v, theta, Normal) t in
        if move then
          { timeout = 0.01;
            messages = [Bus Planning_done];
            world = {world with prepared_vertices = []};
            state = Transition_to_Moving_to ((h, theta, constrained_move), rest) }
        else
          { timeout = 0.01;
            messages = [Bus Planning_done; Msg (Trajectory_path (generate_path_display world ((h, theta)::drop_kind rest)))];
            world = {world with prepared_vertices = List.rev ((h,theta)::drop_kind rest)};
            state = Transition_to_Idle }
      in
      begin match path with
        | No_path msg ->
          Lwt_log.ign_warning msg;
          { timeout = 0.01;
            messages = [Bus Planning_error];
            world = {world with prepared_vertices = []};
            state = Transition_to_Idle }
        | Simple_path (h,t) -> go world h t ~constrained_move:Normal
        | Escaping_path { escape_point; path = (h, t) } ->
          (* TODO: handle the first one specificaly *)
          Printf.printf "escaping: %f %f\n%!" escape_point.x escape_point.y;
          Printf.printf "first: %f %f\n%!" h.x h.y;
          go world escape_point (h::t) ~constrained_move:Constrained
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

let send_msg bus time = function
  | Bus m -> Krobot_bus.send bus (time, Mover_message m)
  | CAN c -> Krobot_bus.send bus (time, CAN (Info, Krobot_message.encode c))
  | Msg m -> Krobot_bus.send bus (time, m)

let main_loop bus iter =
  let rec aux world state timeout =
    lwt msg = Krobot_entry_point.next ~timeout:Krobot_date.(time_to_wait timeout) iter  in
    let update, update_messages =
      match msg with
      | Krobot_entry_point.Timeout ->
        (* Lwt_log.ign_info_f "timeout"; *)
        Some (world, Timeout), []
      | Krobot_entry_point.Message(t,m) -> update_world world m in
    match update with
    | None ->
      aux world state timeout
    | Some (world, input) ->
      let output = step input world state in
      let time = Unix.gettimeofday () in
      lwt () = Lwt_list.iter_s (fun m -> send_msg bus time m) update_messages in
      lwt () = Lwt_list.iter_s (fun m -> send_msg bus time m) output.messages in
      let timeout_date = Krobot_date.(add (now ()) output.timeout) in
      aux output.world output.state timeout_date in
  aux init_world init_state (Krobot_date.now ())

module S : Krobot_entry_point.S = struct
  let name = "mover"
  let options = []
  let main bus ev =
    let iter = Krobot_entry_point.iterator_from_react ev in
    main_loop bus iter
end

module Run = Krobot_entry_point.Make(S)
