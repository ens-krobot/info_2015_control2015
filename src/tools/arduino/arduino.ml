(*
 * krobot_arduino.ml
 * -----------------
 * Copyright : (c) 2015, Pierre Chambart
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Read serial port. *)

open Lwt
open Lwt_react
open Krobot_bus
open Krobot_message

let section = Lwt_log.Section.make "krobot(arduino-lift)"

type info = {
  bus : Krobot_bus.t;
  serial : Krobot_serial.serial;
}

let name = "arduino-lift"

let lift_up = "u"
let lift_down = "i"
let lift_grip_close = "g"
let lift_grip_open = "h"
let lift_door_close = "d"
let lift_door_open = "f"
let lift_done = "o"

let current_id = ref None

(* +-----------------------------------------------------------------+
   | read/send loop                                                  |
   +-----------------------------------------------------------------+ *)

type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

let parse line =
  try
    Scanf.sscanf line "R%c%i" (fun c i -> Ok (c, i))
  with Scanf.Scan_failure s -> Error s

let rec read_loop info =
  lwt line = Krobot_serial.read_line info.serial in
  lwt () = Lwt_log.info_f ~section "Received: %s" line in
  lwt () = match line, !current_id with
    | _, None ->
      Lwt_log.error_f ~section "Received end of action with no request in progress"
    | s, Some id when s = lift_done ->
      lwt () = Lwt_log.info_f ~section "End of request %x" id in
      Krobot_bus.send info.bus (Unix.gettimeofday (), Lift_action_done id)
    | _ ->
      Lwt_log.error_f ~section "Received unknown response while processing request"
  in
  (* begin match parse line with *)
  (*   | Error s -> Printf.printf "parse error: %s\n%!" s; *)
  (*   | Ok (c, i) -> *)
  (*     Printf.printf "parsed: %c %i\n%!" c i *)
  (* end; *)
  lwt () = Lwt_unix.sleep 0.1 in
  read_loop info

(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

let handle_message info (timestamp, message) =
  match message with
  | Kill kill when kill = name ->
    exit 0
  | Lift_up (request_id) -> begin
      match !current_id with
      | None ->
        lwt () = Lwt_log.info_f ~section "Requesting lift up (%X)" request_id in
        current_id := Some request_id;
        Krobot_serial.write_line info.serial lift_up
      | Some cur_id ->
        Lwt_log.error_f ~section "Requesting lift up %X while %X in progress" request_id cur_id
    end
  | Lift_down (request_id) -> begin
      match !current_id with
      | None ->
        lwt () = Lwt_log.info_f ~section "Requesting lift down (%X)" request_id in
        current_id := Some request_id;
        Krobot_serial.write_line info.serial lift_down
      | Some cur_id ->
        Lwt_log.error_f ~section "Requesting lift down %X while %X in progress" request_id cur_id
    end
  | Lift_grip_open (request_id) -> begin
      match !current_id with
      | None ->
        lwt () = Lwt_log.info_f ~section "Requesting grip open (%X)" request_id in
        current_id := Some request_id;
        Krobot_serial.write_line info.serial lift_grip_open
      | Some cur_id ->
        Lwt_log.error_f ~section "Requesting lift grip open %X while %X in progress" request_id cur_id
    end
  | Lift_grip_close (request_id) -> begin
      match !current_id with
      | None ->
        lwt () = Lwt_log.info_f ~section "Requesting grip close (%X)" request_id in
        current_id := Some request_id;
        Krobot_serial.write_line info.serial lift_grip_close
      | Some cur_id ->
        Lwt_log.error_f ~section "Requesting frip close %X while %X in progress" request_id cur_id
    end
  | Lift_door_open (request_id) -> begin
      match !current_id with
      | None ->
        lwt () = Lwt_log.info_f ~section "Requesting door open (%X)" request_id in
        current_id := Some request_id;
        Krobot_serial.write_line info.serial lift_door_open
      | Some cur_id ->
        Lwt_log.error_f ~section "Requesting door open %X while %X in progress" request_id cur_id
    end
  | Lift_door_close (request_id) -> begin
      match !current_id with
      | None ->
        lwt () = Lwt_log.info_f ~section "Requesting door close (%X)" request_id in
        current_id := Some request_id;
        Krobot_serial.write_line info.serial lift_door_close
      | Some cur_id ->
        Lwt_log.error_f ~section "Requesting door close %X while %X in progress" request_id cur_id
    end
  | _ ->
    Lwt.return ()

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true
let tty = ref "/dev/arduino"
let baudrate = ref 115200

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
  "-tty", Arg.Set_string tty, " set tty file";
  "-baudrate", Arg.Set_int baudrate, " set tty baudrate file";
]

let usage = "\
Usage: krobot-arduino [options]
options are:"

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

lwt () =
  Arg.parse options ignore usage;

  (* Display all informative messages. *)
  Lwt_log.append_rule "*" Lwt_log.Info;

  (* Open the krobot bus. *)
  lwt bus = Krobot_bus.get () in

  lwt serial = Krobot_serial.open_serial ~baudrate:!baudrate ~path:!tty in

  (* Fork if not prevented. *)
  if !fork then Krobot_daemon.daemonize bus;

  let info = {
    bus = bus;
    serial = serial;
  } in

  (* Handle krobot message. *)
  E.keep (E.map (handle_message info) (Krobot_bus.recv bus));

  (* Kill any running urg_handler. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill name) in

  (* Loop forever. *)
  read_loop info
