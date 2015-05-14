(*
 * monitor.ml
 * ----------
 * Copyright : (c) 2015, Xavier Lagorce
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Monitor program for match management. *)

open Lwt
open Lwt_react
open Krobot_bus
open Krobot_message
open Krobot_world_update
open Krobot_config

let section = Lwt_log.Section.make "krobot(monitor)"

type info = {
  bus : Krobot_bus.t;
  (* The bus used to communicate with the robot. *)
  mutable world : world;
  mutable started : bool;
}

let match_length = 90. (* Match length in seconds *)

let started_match = ref None

(* +-----------------------------------------------------------------+
   | Helpers                                                         |
   +-----------------------------------------------------------------+ *)

let update_team_leds bus team =
  let turn_on, turn_off = match team with
    | Yellow -> yellow_led, green_led
    | Green -> green_led, yellow_led
  in
  lwt () = Krobot_message.send bus (Unix.gettimeofday(), (Switch_request(turn_on, true))) in
  Krobot_message.send bus (Unix.gettimeofday(), (Switch_request(turn_off, false)))

let print_state bus msg =
  Krobot_lcd.send_line bus 2 msg

let print_team bus team =
  match team with
  | Yellow ->
    print_state bus " switched to Yellow"
  | Green ->
    print_state bus "  switched to Green"

let print_emergency bus em =
  match em with
  | Pressed ->
    print_state bus " Emer. Stop pressed"
  | OK ->
    print_state bus "Emer. Stop released"

(* +-----------------------------------------------------------------+
   | Refresh standard display                                        |
   +-----------------------------------------------------------------+ *)
let rec refresh_display bus =
  lwt () = Krobot_lcd.send_line bus 1 "   -==[ Nuky ]==-   " in
  lwt () = Lwt_unix.sleep 1. in
  refresh_display bus

(* +-----------------------------------------------------------------+
   | Notify emergency stop                                           |
   +-----------------------------------------------------------------+ *)
let rec blink info state last_state =
  match info.world.em_stop with
  | Pressed ->
    lwt () = Krobot_message.send info.bus (Unix.gettimeofday (), LCD_backlight state) in
    lwt () = Lwt_unix.sleep 0.5 in
    blink info (not state) state
  | OK ->
    lwt () =
      if (not last_state) then
        Krobot_message.send info.bus (Unix.gettimeofday (), LCD_backlight true)
      else
        Lwt.return ()
    in
    lwt () = Lwt_unix.sleep 0.5 in
    blink info false true

let count_match_time bus =
  let canceled = ref false in
  started_match := Some canceled;
  let rec loop remaining_time =
    if !canceled then
      Krobot_lcd.send_line bus 3 "     Match cancelled"
    else
    if remaining_time < 0.1 then
      Krobot_lcd.send_line bus 3 "         Match End !"
    else
      let msg = Printf.sprintf "   %02.0f s remaining..." remaining_time in
      lwt () = Krobot_lcd.send_line bus 3 msg in
      lwt () = Lwt_unix.sleep 1. in
      loop (remaining_time-.1.)
  in
  let _ : unit Lwt.t =
    loop match_length
  in
  let _ : unit Lwt.t =
    lwt () = Lwt_unix.sleep (match_length-.1.) in
    if (not !canceled) then
      Krobot_bus.send bus (Unix.gettimeofday(), Match_end)
    else
      Lwt.return ()
  in
  ()

(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

let handle_message info (timestamp, message) =
  match message with
  | Kill "monitor" ->
    exit 0
  | _ -> match update_world info.world message with
    | None ->
      Lwt.return ()
    | Some (world, update) ->
      info.world <- world;
      begin
        match update with
        | Emergency_changed ->
          print_emergency info.bus world.em_stop
        | Team_changed ->
          lwt () = update_team_leds info.bus world.team in
          print_team info.bus world.team
        | Jack_changed when world.jack = Out ->
          count_match_time info.bus;
          Krobot_bus.send info.bus (Unix.gettimeofday(), Match_start)
        | Jack_changed when world.jack = In -> begin
            match !started_match with
            | None ->
              lwt () = Krobot_lcd.send_line info.bus 3 "        Match ready" in
              Krobot_bus.send info.bus (Unix.gettimeofday(), Match_ready)
            | Some cancelation ->
              cancelation := true;
              lwt () = Krobot_lcd.send_line info.bus 3 "    Match cancelled" in
              Krobot_bus.send info.bus (Unix.gettimeofday(), Match_cancelled)
          end
        | _ ->
          Lwt.return ()
      end

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true
let start_on = ref false
let start_team = ref Yellow

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
  "-start", Arg.Set start_on, " suppose the robot started";
  "-green", Arg.Unit (fun () -> start_team := Green), "green team";
]

let usage = "\
Usage: krobot-monitor [options]
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

  (* Fork if not prevented. *)
  if !fork then Krobot_daemon.daemonize bus;

  let info = {
    bus = bus;
    world = {init_world with team = !start_team};
    started = !start_on;
  } in

  lwt () = Lwt_log.info ~section "start monitor" in

  (* Handle krobot message. *)
  E.keep (E.map (handle_message info) (Krobot_bus.recv bus));

  (* Kill any running urg_handler. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "monitor") in

  (* utils loops *)
  ignore(blink info false false);
  ignore(refresh_display info.bus);

  (* signal on LCD screen *)
  lwt () = Krobot_lcd.send_line bus 1 "   -==[ Nuky ]==-   " in
  lwt () = Krobot_lcd.send_line bus 2 "         Monitor Rdy" in

  (* Loop forever. *)
  fst (wait ())
