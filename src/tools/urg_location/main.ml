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
let set_odometry = ref false

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
  "-set", Arg.Set set_odometry, " Set odometry";
]

let usage = "\
Usage: krobot-urg-location [options]
options are:"

(* +-----------------------------------------------------------------+
   | Main part                                                       |
   +-----------------------------------------------------------------+ *)

(* TODO: mettre ailleurs *)
let center_line_1 =
  let open Adjust_map in
  let y = 1.25 +. 0.022 /. 2. in
  { a = { x = 0.9; y}; b = { x = 0.9 +. 1.2; y } }

let center_line_2 =
  let open Adjust_map in
  let x = 1.5 in
  { a = { x; y = 1.25 }; b = { x; y = 1.25 +. (-0.512 -. 0.044) } }

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
  @ [ center_line_1; center_line_2 ]

let fit_world ~init data =
  let ransac_param = Adjust_map.ransac_param ~rounds:5 ~init map (Array.of_list data) in
  match Ransac.ransac ransac_param with
  | None -> None
  | Some ransac_result ->
    (* Refit for better precision *)
    try
      let tr', rank =
        Adjust_map.solve ~rounds:5
          ~init:ransac_result.Ransac.model
          map (Array.to_list ransac_result.Ransac.in_model)
      in
      if rank < 3 then
        None
      else
        Some (tr', ransac_result.Ransac.in_model)
    with _ ->
      None

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

let rotation_mat th =
  Lacaml.D.Mat.of_array
    [| [| cos th; -. sin th; 0. |];
       [| sin th; cos th;    0. |];
       [| 0.;     0.;        1. |]; |]

let start_loop bus =
  let rec loop state =
    lwt urg_data, state = consume_and_update state in
    lwt () =
      match urg_data with
      | None -> Lwt.return_unit
      | Some (data, robot) ->
        let rotation_matrix = rotation_mat robot.orientation in
        let hokuyo_vec =
          Lacaml.D.Vec.of_array [|
            Krobot_config.urg_down_position.x;
            Krobot_config.urg_down_position.y;
            1.
          |]
        in
        let rotated_vec =
          Lacaml.D.gemv rotation_matrix hokuyo_vec
        in
        let dx = rotated_vec.{1} in
        let dy = rotated_vec.{2} in
        let init =
          (* TODO: shift to hokuyo's position, this is the center *)
          { Adjust_map.th = robot.orientation;
            x = robot.position.x +. dx;
            y = robot.position.y +. dy }
        in
        let fitted = fit_world ~init (Array.to_list data.data) in
        lwt () =
          match fitted with
          | None ->
            Printf.printf "Can't fit\n%!";
            Lwt.return_unit
          | Some (tr, in_model) ->
            (* Printf.printf "fitted, size: %i\n%!" (Array.length in_model); *)
            (* Printf.printf "sol th: %0.4f x: %0.4f y: %0.4f\n%!" tr.th tr.x tr.y; *)
            (* Printf.printf "pos th: %0.4f x: %0.4f y: %0.4f\n%!" init.th init.x init.y; *)
            (* Printf.printf "dif th: %0.4f x: %0.4f y: %0.4f\n%!" *)
            (*   (init.th -. tr.th) (init.x -. tr.x) (init.y -. tr.y) *)
            let rotation_matrix = rotation_mat tr.th in
            let rotated_vec =
              Lacaml.D.gemv rotation_matrix hokuyo_vec
            in
            let dx = rotated_vec.{1} in
            let dy = rotated_vec.{2} in
            let tr = { tr with x = tr.x -. dx; y = tr.y -. dy } in
            Printf.printf "fitted, size: %i " (Array.length in_model);
            Printf.printf " th: %0.4f x: %0.4f y: %0.4f\n%!"
              (init.th -. tr.th) (init.x -. tr.x) (init.y -. tr.y);
            if Array.length in_model > 140 then
              Krobot_bus.send bus
                (data.timestamp,
                 Krobot_bus.Urg_location ({x = tr.x; y = tr.y}, tr.th))
            else
              Lwt.return_unit
        in
        Lwt.return_unit
    in
    lwt () = Lwt_unix.sleep 0.05 in
    loop state
  in
  lwt state = init () in
  loop state

let send_odometry bus =
  lwt _state = init () in
  Lwt.return_unit

lwt () =
  Arg.parse options ignore usage;

  if !set_odometry then
    (* Open the krobot bus. *)
    lwt bus = Krobot_bus.get () in

    send_odometry bus
  else begin

    (* Display all informative messages. *)
    Lwt_log.append_rule "*" Lwt_log.Info;

    (* Open the krobot bus. *)
    lwt bus = Krobot_bus.get () in

    (* Kill any running urg_handler. *)
    lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill name) in

    start_loop bus
  end
