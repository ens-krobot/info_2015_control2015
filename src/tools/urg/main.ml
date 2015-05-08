(*
 * krobot_urg.ml
 * ----------------
 * Copyright : (c) 2013, Pierre Chambart
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Service providing raw urg data. *)

open Lwt
open Lwt_react
open Lwt_preemptive
open Krobot_bus
open Krobot_message
open Krobot_geom

let section = Lwt_log.Section.make "krobot(urg)"

(* +-----------------------------------------------------------------+
   | read/send loop                                                  |
   +-----------------------------------------------------------------+ *)

(* let min_distance = 60 *)

let scale = 0.001

let min_distance = (* in millimeters *)
  int_of_float (1000. *. Krobot_config.urg_min_distance)

let min_angle, max_angle = Krobot_config.urg_angle_limits


let convert_pos dist angle =
  let x = float dist *. cos angle *. scale +. Krobot_config.urg_position.x in
  let y = float dist *. sin angle *. scale +. Krobot_config.urg_position.y in
  (x), (-. y) (* the urg is top down -> y is reversed *)

let convert (b:Urg.point_data) =
  let dim = Bigarray.Array1.dim b in
  let l = ref [] in
  for i = 0 to dim - 1 do
    let data = Nativeint.to_int b.{i} in
    if data > min_distance
    then
      let angle = Krobot_config.urg_angles.(i) in
      if angle >= min_angle && angle <= max_angle
      then
        let (x,y) = convert_pos data angle in
        l := {x;y} :: !l
  done;
  Array.of_list !l

let loop bus urg =
  let id =
    let id = urg.Urg_simple.id in
    if id = Krobot_config.urg_up_id then
      Krobot_bus.Up
    else if id = Krobot_config.urg_down_id then
      Krobot_bus.Down
    else failwith (Printf.sprintf "unknown urg id %s" id)
  in
  let rec aux () =
    lwt _ = Lwt_preemptive.detach Urg_simple.get urg in
    let time = Unix.gettimeofday () in
    let msg = Urg (id, convert urg.Urg_simple.data) in
    lwt () = Krobot_bus.send bus (time, msg) in
    lwt () = Lwt_unix.sleep 0.01 in
    aux () in
  aux ()

(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

let urgs = ref []

let handle_message (timestamp, message) =
  match message with
  | Kill "urg" ->
    List.iter (fun urg -> Urg.urg_disconnect urg.Urg_simple.urg) !urgs;
    exit 0
  | _ ->
    ()

(* let print_pos l = *)
(*   Format.printf "[@[<1>@ "; *)
(*   Array.iter (fun {x;y} -> Format.printf "(%f,%f);@ " x y) l; *)
(*   Format.printf "@]]@." *)

let string_of_urg_id = function
  | Up -> "Up"
  | Down -> "Down"

let print_pos id ts l =
  Printf.printf "%f %s " ts (string_of_urg_id id);
  Array.iter (fun {x;y} -> Format.printf "%f %f " x y) l;
  Printf.printf "\n%!"

let handle_listener (timestamp, message) =
  match message with
    | Urg (id, data) ->
      print_pos id timestamp data;
      return ()
    | _ -> Lwt.return ()

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true
let listen = ref false
let tty = ref []

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
  "-listen", Arg.Set listen, " listen results";
  "-tty", Arg.String (fun s -> tty := s :: !tty), " add tty";
]

let usage = "\
Usage: krobot-urg [options]
options are:"

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let run_sender bus =
  (* Fork if not prevented. *)
  if !fork then Krobot_daemon.daemonize bus;

  (* Handle krobot message. *)
  E.keep (E.map handle_message (Krobot_bus.recv bus));

  (* Kill any running urg_handler. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "urg") in

  (* Wait a bit to let the other handler release the connection *)
  lwt () = Lwt_unix.sleep 0.4 in

  urgs := List.map (fun tty -> Urg_simple.init ~tty ()) !tty;

  (* Loop forever. *)
  Lwt_list.iter_p (fun urg -> loop bus urg) !urgs

let run_listener bus =
  E.keep (E.map_s handle_listener (Krobot_bus.recv bus));
  let t, _ = Lwt.wait () in
  t

lwt () =
  Arg.parse options ignore usage;
  begin match !tty with
    | [] ->
      Printf.printf "No urg tty provided\n%!";
      exit 1;
    | _ -> () end;

  (* Display all informative messages. *)
  Lwt_log.append_rule "*" Lwt_log.Info;

  (* Open the krobot bus. *)
  lwt bus = Krobot_bus.get () in

  if !listen
  then run_listener bus
  else run_sender bus
