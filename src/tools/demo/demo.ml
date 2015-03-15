open Krobot_bus;;
open Krobot_message;;

type world_update =
  | Position_updated
  | Motor_started
  | Motor_stopped

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

  something_else : unit;
}

type state =
  | Transition_to_Moving_to of Krobot_geom.vertice
  | Moving_to of Krobot_geom.vertice
  | Idle
  | Transition_to_Idle

type message =
  | Bus of Krobot_bus.message
  | CAN of Krobot_message.t

type output = {
  timeout : float; (* in seconds *)
  messages : message list;
  state : state
}

let init_world = {
  robot = {
    position = { x = 0.; y = 0. };
    orientation = 0.;
    motors_moving = false;
  };

  something_else = ();
}

let init_state = Transition_to_Moving_to Krobot_geom.{x = 0.1; y = 0.05}

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
  fun world -> function
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
    | message ->
      Some (world, Message message)

let step (input:input) (world:world) (state:state) : output =
  match state with
  | Transition_to_Idle ->
    Lwt_log.ign_info_f "Idle";
    { timeout = 1.;
      messages = [];
      state = Idle }
  | Idle ->
    { timeout = 1.;
      messages = [];
      state = Idle }
  | Transition_to_Moving_to dest -> begin
      Lwt_log.ign_info_f "Moving_to";
      let open Krobot_geom in
      let move = vector world.robot.position dest in
      let ratio = normalize move in
      let max_speed = 0.5 in
      let max_accel = 0.8 in
      let move_x = Motor_move_x(move.vx, max_speed *. ratio.vx, max_accel *. ratio.vx ) in
      let move_y = Motor_move_y(move.vy, max_speed *. ratio.vy, max_accel *. ratio.vy ) in
      { timeout = 1.;
        messages = [CAN (move_x); CAN (move_y)];
        state = Moving_to dest }
    end
  | Moving_to dest -> begin
      let state =
        match input with
        | World_updated Motor_stopped -> Idle
        | _ -> Moving_to dest
      in
      { timeout = 1.;
        messages = [];
        state }
    end

type receiver = (float * Krobot_bus.message) option Lwt.t

let mk_recv ev = Lwt.(Lwt_react.E.next ev >|= fun v -> Some v)

let next_event ev (recv:receiver) timeout =
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
      | None -> Some (world, Timeout)
      | Some m -> update_world world m in
    match update with
    | None ->
      aux recv world state timeout
    | Some (world, input) ->
      let output = step input world state in
      let time = Unix.gettimeofday () in
      lwt () = Lwt_list.iter_s (fun m -> send_msg bus time m) output.messages in
      let timeout_date = Date.(add (now ()) output.timeout) in
      aux recv world output.state timeout_date in
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
