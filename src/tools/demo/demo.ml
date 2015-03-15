open Krobot_bus;;
open Krobot_message;;

type input =
  | Message of Krobot_bus.message
  | World_updated
  | Timeout

type robot = {
  position : Krobot_geom.vertice;
  (* The position of the robot on the table. *)
  orientation : float;
  (* The orientation of the robot. *)

  some_other_stuff : unit;
}

type world = {
  robot : robot;

  something_else : unit;
}

type state =
  | Idle

type output = {
  timeout : float; (* in seconds *)
  messages : Krobot_bus.message list;
  state : state
}

let init_world = {
  robot = {
    position = { x = 0.; y = 0. };
    orientation = 0.;

    some_other_stuff = ();
  };

  something_else = ();
}

let init_state = Idle

let name = "demo"

let update_world : world -> Krobot_bus.message -> world * input =
  fun world -> function
    | Kill killed when killed = name ->
      exit 0

    | CAN (_,frame) as message -> begin
        match decode frame with
        | Odometry(x, y, theta) ->
          { world with
            robot =
              Krobot_geom.
                { world.robot with
                  position = { x; y };
                  orientation = math_mod_float theta (2. *. pi) } },
          World_updated
        | _ ->
          world, Message message
      end
    | message -> world, Message message

let step (input:input) (world:world) (state:state) : output =
  match state with
  | Idle  ->
    Lwt_log.ign_info_f "idle";
    { timeout = 1.;
      messages = [];
      state = Idle }

type receiver = (float * Krobot_bus.message) option Lwt.t

let mk_recv ev = Lwt.(Lwt_react.E.next ev >|= fun v -> Some v)

let next_event ev (recv:receiver) timeout =
  let time = Lwt.(Lwt_unix.sleep timeout >>= fun _ -> return_none) in
  match_lwt Lwt.(time <?> recv) with
  | None -> Lwt.return (recv, None)
  | Some (msg_time, msg) ->
    Lwt.cancel time;
    Lwt.return (mk_recv ev, Some msg)

let main_loop bus ev =
  let rec aux recv world state timeout =
    lwt (recv,msg) = next_event ev recv timeout in
    let world, input =
      match msg with
      | None -> world, Timeout
      | Some m -> update_world world m in
    let output = step input world state in
    let time = Unix.gettimeofday () in
    lwt () = Lwt_list.iter_s (fun m -> Krobot_bus.send bus (time, m)) output.messages in
    aux recv world output.state output.timeout in
  aux (mk_recv ev) init_world init_state 0.

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
