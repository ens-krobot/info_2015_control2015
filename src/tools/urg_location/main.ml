(*
 * krobot_location.ml
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

let section = Lwt_log.Section.make "krobot(urg_location)"

let name = "urg_location"

type urg_data =
  { data : Adjust_map.point array;
    timestamp : float; }

type state =
  { world : Krobot_world_update.world;
    bus : Krobot_bus.t;
    stream : (float * Krobot_bus.message) Lwt_stream.t }

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
]

let usage = "\
Usage: krobot-urg-location [options]
options are:"

(* +-----------------------------------------------------------------+
   | Main part                                                       |
   +-----------------------------------------------------------------+ *)

(* TODO: mettre ailleurs et rajouter le centre*)
let map =
  let open Adjust_map in
  let size_x = 3. in
  let size_y = 2. in
  let p1 = { x = 0.; y = 0. } in
  let p2 = { x = 0.; y = size_y } in
  let p3 = { x = size_x; y = size_y } in
  let p4 = { x = size_x; y = 0. } in
  [
    { a = p1; b = p2 };
    { a = p2; b = p3 };
    { a = p3; b = p4 };
    { a = p4; b = p1 } ]

let fit_world ~init data =
  let ransac_param = Adjust_map.ransac_param ~rounds:5 ~init map (Array.of_list data) in
  match Ransac.ransac ransac_param with
  | None -> None
  | Some ransac_result ->
    (* Refit for better precision *)
    let tr' =
      Adjust_map.solve ~rounds:5
        ~init:ransac_result.Ransac.model
        map (Array.to_list ransac_result.Ransac.in_model)
    in
    Some (tr', ransac_result.Ransac.in_model)



(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let init () : state Lwt.t =
  lwt bus = Krobot_bus.get () in
  let ev = Krobot_bus.recv bus in
  let stream = Lwt_react.E.to_stream ev in
  let world = Krobot_world_update.init_world in
  Lwt.return { world; bus; stream }

let consume_and_update state =
  try
    let l = Lwt_stream.get_available state.stream in
    let r =
      List.fold_left (fun (urg_updated, state) (timestamp,msg) ->
        match msg with
        | Kill killed when killed = name ->
          Printf.printf "die!\n%!";
          exit 0
        | Urg (id, data) ->
          Some ({ data; timestamp }, state.world.robot), state
        | _ ->
          match Krobot_world_update.update_world state.world msg with
          | None -> urg_updated, state
          | Some (world, _) -> urg_updated, { state with world })
        (None, state) l
    in
    Lwt.return r
  with e -> raise_lwt e

let start_loop bus =
  let rec loop state =
    lwt urg_data, state = consume_and_update state in
    lwt () =
      match urg_data with
      | None -> Lwt.return_unit
      | Some (data, robot) ->
        let init =
          (* TODO: shift to hokuyo's position, this is the center *)
          { Adjust_map.th = robot.orientation;
            x = robot.position.x;
            y = robot.position.y }
        in
        let fitted = fit_world ~init (Array.to_list data.data) in
        let () =
          match fitted with
          | None ->
            Printf.printf "Can't fit\n%!"
          | Some (tr, in_model) ->
            Printf.printf "fitted, size: %i\n%!" (Array.length in_model);
            Printf.printf "sol th: %0.4f x: %0.4f y: %0.4f\n%!" tr.th tr.x tr.y;
            Printf.printf "pos th: %0.4f x: %0.4f y: %0.4f\n%!" init.th init.x init.y
        in
        Lwt.return_unit
    in
    lwt () = Lwt_unix.sleep 0.05 in
    loop state
  in
  lwt state = init () in
  loop state

lwt () =
  Arg.parse options ignore usage;

  (* Display all informative messages. *)
  Lwt_log.append_rule "*" Lwt_log.Info;

  (* Open the krobot bus. *)
  lwt bus = Krobot_bus.get () in

  (* Kill any running urg_handler. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill name) in

  start_loop bus
