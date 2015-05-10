(*
 * krobot_arduino.ml
 * ----------------
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

let section = Lwt_log.Section.make "krobot(arduino stuff)"

type info = {
  bus : Krobot_bus.t;
}

let name = "arduino"

(* +-----------------------------------------------------------------+
   | read/send loop                                                  |
   +-----------------------------------------------------------------+ *)


(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

let handle_message info (timestamp, message) =
  match message with
  | Kill kill when kill = name ->
    exit 0
  | _ ->
    ()

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true
let tty = ref "/dev/ttyUSB1"
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

type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

let parse line =
  try
    Scanf.sscanf line "R%c%i" (fun c i -> Ok (c, i))
  with Scanf.Scan_failure s -> Error s

let count = ref 0

let rec read_loop info serial =
  incr count;
  let msg = Printf.sprintf "R%c%i" 'a' !count in
  lwt () = Krobot_serial.write_line serial msg in
  Printf.printf "send: %s\n%!" msg;
  lwt line = Krobot_serial.read_line serial in
  Printf.printf "received: %s\n%!" line;
  begin match parse line with
    | Error s -> Printf.printf "parse error: %s\n%!" s;
    | Ok (c, i) ->
      Printf.printf "parsed: %c %i\n%!" c i
  end;
  lwt () = Lwt_unix.sleep 0.1 in
  read_loop info serial

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
  } in

  (* Handle krobot message. *)
  E.keep (E.map (handle_message info) (Krobot_bus.recv bus));

  (* Kill any running urg_handler. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill name) in

  (* Loop forever. *)
  read_loop info serial
