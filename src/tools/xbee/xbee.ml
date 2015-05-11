(*
 * xbee.ml
 * -------
 * Copyright : (c) 2013, Pierre Chambart
 * Copyright : (c) 2015, Xavier Lagorce
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Xbee interface with the second robot. *)

open Lwt
open Lwt_react
open Krobot_bus
open Krobot_message
open Krobot_serial

let section = Lwt_log.Section.make "krobot(xbee)"

type info = {
  bus : Krobot_bus.t;
  (* The bus used to communicate with the robot. *)
  mutable team : [ `Yellow | `Green ];
  (* The state of the team selector. *)
  mutable jack : bool;
  (* Status of the jack. *)
  mutable started : bool;
  start_condition : unit Lwt_condition.t;
}

(* +-----------------------------------------------------------------+
   | read/send loop                                                  |
   +-----------------------------------------------------------------+ *)

exception Restart

let slave_start = "KBSS\r"
let master_acknowledge = "KBMA\r"
let kb_match_start = "KBMS\r"
let kb_start_get_team = "KBSGT\r"
let team_yellow = "KBMTY\r"
let team_green = "KBMTG\r"

let msg_len = max (String.length slave_start) (String.length kb_start_get_team)

let slave_start_matching =
  Str.regexp (".*"^slave_start^".*"), msg_len

let slave_get_team =
  Str.regexp (".*"^kb_start_get_team^".*"), msg_len

type matched =
  | Matched
  | Not_matched of string

let string_tail str len =
  if String.length str <= len
  then str
  else String.sub str (String.length str - len) len

let get_string (pattern,length) str =
  if Str.string_match pattern str 0
  then Matched
  else Not_matched (string_tail str (length-1))


type recv =
  | Slave_start
  | Slave_get_team

let rec read_until_pattern acc serial =
  lwt () = Lwt_log.info_f ~section "wait msg" in
  lwt s = read_string ~max_length:20 serial in
  lwt () = Lwt_log.info_f ~section "recv %s" s in
  let s = acc ^ s in
  match get_string slave_start_matching s with
  | Matched -> Lwt.return Slave_start
  | Not_matched _ ->
    match get_string slave_get_team s with
    | Matched -> Lwt.return Slave_get_team
    | Not_matched acc -> read_until_pattern acc serial

let rec read_slave_start serial =
  match_lwt read_until_pattern "" serial with
  | Slave_start -> Lwt.return ()
  | Slave_get_team -> read_slave_start serial

let rec answer_slave_start info serial =
  lwt () = Lwt.pick
      [read_slave_start serial;
       Lwt_condition.wait info.start_condition] in
  lwt () = send_string serial master_acknowledge in
  if info.started
  then Lwt.return ()
  else answer_slave_start info serial

let loop_match_start info serial =
  let answered = ref None in
  let rec send () =
    match !answered with
    | None ->
      lwt () = Lwt_unix.sleep 0.05 in
      lwt () = send_string serial kb_match_start in
      send ()
    | Some (Slave_get_team) -> Lwt.return ()
    | Some (Slave_start) ->
      raise_lwt Restart
  in
  let t = send () in
  let _ =
    lwt r = read_until_pattern "" serial in
    answered := Some r;
    Lwt.return ()
  in
  t

let rec recv serial =
  match_lwt read_until_pattern "" serial with
  | Slave_get_team ->
    recv serial
  | Slave_start ->
    raise_lwt Restart

let rec loop_team info serial =
  let msg = match info.team with
  | `Yellow -> team_yellow
  | `Green -> team_green in
  lwt () = send_string serial msg in
  lwt () = Lwt_unix.sleep 0.2 in
  loop_team info serial

(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

let handle_message info (timestamp, message) =
  match message with
    | CAN(_, frame) -> begin
        match decode frame with
        | Switch1_status(jack, team, _, _, _, _, _, _) ->
          info.jack <- not jack;
          info.team <- if team then `Yellow else `Green
        | _ ->
          ()
      end

    | Match_start ->
      info.started <- true;
      Lwt_condition.broadcast info.start_condition ()

    | Kill "xbee" ->
      exit 0
    | _ ->
        ()

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true
let tty = ref "/dev/xbee"
let baudrate = ref 57600
let start_on = ref false
let start_team : [`Yellow|`Green] ref = ref (`Yellow:>[`Yellow|`Green])

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
  "-tty", Arg.Set_string tty, " set tty file";
  "-baudrate", Arg.Set_int baudrate, " set tty baudrate file";
  "-start", Arg.Set start_on, " suppose the robot started";
  "-green", Arg.Unit (fun () -> start_team := `Green), "green team";
]

let usage = "\
Usage: krobot-xbee [options]
options are:"

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let rec main_loop info serial =
  try_lwt
    lwt () = Lwt_log.info ~section "wait slave" in
    lwt () = answer_slave_start info serial in
    lwt () = Lwt_log.info ~section "match started" in
    lwt () = loop_match_start info serial in
    let _ = recv serial in
    lwt () = Lwt_log.info ~section "send team" in
    loop_team info serial
  with Restart ->
   info.started <- false;
   main_loop info serial


lwt () =
  Arg.parse options ignore usage;

  (* Display all informative messages. *)
  Lwt_log.append_rule "*" Lwt_log.Info;

  (* Open the krobot bus. *)
  lwt bus = Krobot_bus.get () in

  lwt serial = open_serial ~baudrate:!baudrate ~path:!tty in

  (* Fork if not prevented. *)
  if !fork then Krobot_daemon.daemonize bus;

  let info = {
    bus = bus;
    team = !start_team;
    jack = false;
    started = !start_on;
    start_condition = Lwt_condition.create ();
  } in

  (* Handle krobot message. *)
  E.keep (E.map (handle_message info) (Krobot_bus.recv bus));

  (* Kill any running urg_handler. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "xbee") in

  (* Loop forever. *)
  main_loop info serial
