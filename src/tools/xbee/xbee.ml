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
open Krobot_world_update

let section = Lwt_log.Section.make "krobot(xbee)"

type match_state =
  | Waiting
  | Ready
  | Started
  | Cancelled
  | Ended

type info = {
  bus : Krobot_bus.t;
  serial : Krobot_serial.serial;
  (* The bus used to communicate with the robot. *)
  mutable world : world;
  mutable match_state : match_state;
}

(* +-----------------------------------------------------------------+
   | CAN YOU HEAR MEEEE ???? loop                                    |
   +-----------------------------------------------------------------+ *)

let rec broadcast_state_loop info =
  let msg = match info.match_state with
    | Waiting -> "w"
    | Ready -> (match info.world.team with Yellow -> "y" | Green -> "g" )
    | Started -> "s"
    | Cancelled -> "c"
    | Ended -> "e"
  in
  lwt () = Krobot_serial.write_line info.serial msg in
  lwt () = Lwt_unix.sleep 0.1 in
  broadcast_state_loop info

(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

let handle_message info (timestamp, message) =
  match message with
  | Kill "xbee" ->
    exit 0
  | Match_ready ->
    info.match_state <- Ready;
    Lwt.return ()
  | Match_start ->
    lwt () = Krobot_serial.write_line info.serial "g" in
    info.match_state <- Started;
    Lwt.return ()
  | Match_cancelled ->
    info.match_state <- Cancelled;
    Lwt.return ()
  | Match_end ->
    info.match_state <- Ended;
    Lwt.return ()
  | _ -> match update_world info.world message with
    | None ->
      Lwt.return ()
    | Some (world, update) ->
      info.world <- world;
      Lwt.return ()

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true
let tty = ref "/dev/xbee"
let baudrate = ref 9600
let start_on = ref false
let start_team = ref Yellow

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
  "-tty", Arg.Set_string tty, " set tty file";
  "-baudrate", Arg.Set_int baudrate, " set tty baudrate file";
  "-start", Arg.Set start_on, " suppose the robot started";
  "-green", Arg.Unit (fun () -> start_team := Green), "green team";
]

let usage = "\
Usage: krobot-xbee [options]
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

  lwt serial = open_serial ~baudrate:!baudrate ~path:!tty in

  (* Fork if not prevented. *)
  if !fork then Krobot_daemon.daemonize bus;

  let info = {
    bus = bus;
    world = {init_world with team = !start_team};
    serial = serial;
    match_state = Waiting;
  } in

  (* Handle krobot message. *)
  E.keep (E.map (handle_message info) (Krobot_bus.recv bus));

  (* Kill any running urg_handler. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "xbee") in

  (* serial communication loop *)
  (* ignore(broadcast_state_loop info); *)

  (* Loop forever. *)
  fst (wait ())
