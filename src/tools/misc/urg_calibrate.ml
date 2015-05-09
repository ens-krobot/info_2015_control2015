(*
 * urg_calibrate.ml
 * ----------------
 * Copyright : (c) 2015, Pierre Chambart
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Lwt_react
open Lwt_preemptive
open Krobot_bus
open Krobot_message
open Krobot_geom

(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

(* let string_of_urg_id = function *)
(*   | Up -> "Up" *)
(*   | Down -> "Down" *)

(* let print_pos id ts l = *)
(*   Printf.printf "%f %s " ts (string_of_urg_id id); *)
(*   Array.iter (fun {x;y} -> Format.printf "%f %f " x y) l; *)
(*   Printf.printf "\n%!" *)

let filter id (_, msg) =
  match msg with
  | Urg (id', data) when id = id' -> Some data
  | _ -> None

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let id = ref Down

let id_of_string s =
  match String.lowercase s with
  | "up" -> Up
  | "down" -> Down
  | _ -> failwith (Printf.sprintf "unknown id %s" s)

let options = Arg.align [
  "-id", Arg.String (fun s -> id := id_of_string s), " urg id (up or down)"
]

let usage = "\
Usage: krobot-urg-calibrate [options]
options are:"

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let n_points = 3
let min_dist = 0.05
let max_dist = 0.3

let filter_points a =
  let l = Array.to_list a in
  List.filter (fun p ->
    let d = distance { x = 0.; y = 0. } p in
    d < max_dist && d > min_dist) l

let mean_pos l =
  let x, y, n =
    List.fold_left (fun (accx, accy, n) { x; y } ->
      (accx +. x, accy +. y, succ n))
      (0., 0., 0) l
  in
  { x = x /. float n; y = y /. float n }

lwt () =
  Arg.parse options ignore usage;
    lwt bus = Krobot_bus.get () in
    let id = !id in
    let event = React.E.fmap (filter id) (Krobot_bus.recv bus) in
    let stream = Lwt_react.E.to_stream event in
    lwt l = Lwt_stream.nget n_points stream in
    let l = List.map filter_points l in
    let mean = mean_pos (List.flatten l) in
    let vect = vector { x = 0.; y = 0. } mean in
    let dist = norm vect in
    Printf.printf "pos: { x: %.03f; y: %.03f }\nangle : %.03f (rad) %.03f\ndistance: %.03f\n%!"
      mean.x mean.y (angle vect) (pi /. 2. +. (angle vect))
      dist;
    Lwt.return ()
