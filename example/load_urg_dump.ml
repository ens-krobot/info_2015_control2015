#use "topfind";;
#require "str";;
#require "krobot";;
#camlp4o;;
#require "lwt.syntax";;
#require "lacaml";;
#require "lacaml.top";;
lwt bus = Krobot_bus.get ();;

let rec couples = function
  | [] -> []
  | [_] -> assert false
  | h1 :: h2 :: t -> Krobot_geom.{x = h1; y = h2} :: couples t

let line_points line =
  let s = Str.split_delim (Str.regexp_string " ") line in
  let time, urg, x, y, theta, rest =
    match s with
    | time :: urg :: "odo:" :: x :: y :: theta :: "data:" :: rest ->
      time, urg, x, y, theta, rest
    | _ -> assert false
  in
  let l =
    List.map float_of_string
      (List.filter (fun x -> try ignore (float_of_string x); true with _ -> false) rest) in
  let points = couples l in
  let urg = match urg with
    | "Up" -> Krobot_bus.Up
    | "Down" -> Krobot_bus.Down
    | _ -> assert false
  in
  urg, float_of_string x, float_of_string y, float_of_string theta, Array.of_list points

let read_lines file =
  let ic = open_in file in
  let lines = ref [] in
  try while true do
      let line = input_line ic in
      let urg, x, y, theta, points = line_points line in
      lines := (urg, x, y, theta, points) :: !lines
    done;
    assert false
  with End_of_file ->
    close_in ic;
    List.rev !lines

let file = read_lines "/home/chambart/urg_dump_odo_main"
let file = read_lines "/home/chambart/urg_dump_odo_go"

let is_start (urg, x, y, theta, points) =
  let open Krobot_config in
  let open Krobot_geom in
  abs_float (x -. (fst yellow_initial_position).x) < 0.05 &&
  abs_float (y -. (fst yellow_initial_position).y) < 0.05

exception Found of int
let () = Array.iteri (fun i p -> if is_start p then raise (Found i)) (Array.of_list file)

let send_tuple bus (urg, x, y, theta, points) =
  let msg = Krobot_bus.Urg (urg, points) in
  lwt () = Krobot_message.send bus (Unix.gettimeofday (), Krobot_message.Odometry(x, y, theta)) in
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), msg) in
  Lwt.return ();;

let send_lines bus vs =
  let msg = Krobot_bus.Urg_lines (Array.of_list vs) in
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), msg) in
  Lwt.return ();;

let first_pt = 239;;

let send_tuple l = Lwt_main.run (send_tuple bus l)
let send_lines l = Lwt_main.run (send_lines bus l)
let send_lines' l = send_lines (List.map (fun ((x1,y1), (x2,y2)) -> {x = x1;y = y1}, {x = x2;y = y2}) l)

let () = send_tuple (List.nth file 239);;
let () = send_tuple (List.nth file 751);;
let () = send_tuple (List.nth file 901);;
(* let () = send_tuple (List.nth file 240);; *)


open Krobot_geom
type line = vertice * vector (* normal *)

let turn_trigo { vx; vy } = { vx = -. vy; vy = vx }

let dist_line ((vert, vect):line) point =
  let nv = turn_trigo vect in
  abs_float (prod nv (vector vert point))

let dist_line (vert, {vx;vy}) point =
  abs_float
    ((-. vy) *. (vert.x -. point.x) +.
     vx *. (vert.y -. point.y))

(* let tl = ({x = 0.; y = 0.}, { vx = 1. /. sqrt 2.; vy = 1. /. sqrt 2. }) *)
(* let tl = ({x = 0.; y = 1.}, { vx = 1.; vy = 0. }) *)
(* let p = { x = 100.; y = 3. } *)

(* let d = dist_line tl p *)

let filter_points ~line ~threshold points =
  List.filter (fun point -> dist_line line point <= threshold) points

let line1 = ({ x = 0.; y = 0. }, { vx = 1.; vy = 0. })
let line2 = ({ x = 0.; y = 0. }, { vx = 0.; vy = 1. })
let line3 = ({ x = Krobot_config.world_width; y = Krobot_config.world_height }, { vx = -. 1.; vy = 0. })
let line4 = ({ x = Krobot_config.world_width; y = Krobot_config.world_height }, { vx = 0.; vy = -. 1. })

let rotate_vert theta vert =
  let rot = rot_mat theta in
  let pos = [| vert.x; vert.y; 1. |] in
  let r_pos = mult rot pos in
  { x = r_pos.(0); y = r_pos.(1) }

let down_urg_pos (x, y, theta) =
  let urg_pos = Krobot_config.urg_down_position in
  let vert = rotate_vert theta urg_pos in
  { vx = vert.x +. x; vy = vert.y +. y }

let project_down_urg (x, y, theta) pos =
  let rot = rot_mat (theta +. pi) in
  let down_urg_pos = down_urg_pos (x, y, theta) in
  let f { x;y } =
    let urg_pos = [| x; y; 1. |] in
    let urg_pos = mult rot urg_pos in
    let state_pos = Krobot_geom.translate
        { x = urg_pos.(0); y = urg_pos.(1) }
        down_urg_pos in
    state_pos
  in
  Array.map f pos

let get_points file n =
  let (urg,x,y,theta,points) = List.nth file n in
  assert(urg = Krobot_bus.Down);
  Array.to_list (project_down_urg (x,y,theta) points)

let points = get_points file 239
  (* let (_,_,_,_,points) = List.nth file 239 in *)
  (* Array.to_list points *)
let points = get_points file 901
  (* let (_,_,_,_,points) = List.nth file 901 in *)
  (* Array.to_list points *)
let pl1 = filter_points ~line:line1 ~threshold:0.2 points
let pl2 = filter_points ~line:line2 ~threshold:0.2 points
let pl3 = filter_points ~line:line3 ~threshold:0.2 points
let pl4 = filter_points ~line:line4 ~threshold:0.2 points

let l' = List.filter (fun {x;_} -> abs_float (x -. Krobot_config.world_width) < 0.2) points

let mat l =
  let len = List.length l in
  let mat = Lacaml.D.Mat.create len 3 in
  List.iteri (fun i { x; y } ->
    mat.{i+1,1} <- x;
    mat.{i+1,2} <- y;
    mat.{i+1,3} <- 1.)
    l;
  mat

let coeffs pl =
  let m = mat pl in
  let _v, _r1, r2 = Lacaml.D.gesvd m in
  r2.{3,1}, r2.{3,2}, r2.{3,3}

let coeffs_line line threshold points =
  let pl = filter_points ~line ~threshold points in
  coeffs pl

let line_of_coef (a, b, c) =
  let p =
    if abs_float a > abs_float b then
      { x = 0.; y = (-.c /. a) }
    else
      { x = (-.c /. b); y = 0. }
  in
  p, { vx = -. b; vy = a }

let step_line line threshold points =
  let pl = filter_points ~line ~threshold points in
  line_of_coef (coeffs pl)

let l1 = coeffs_line line1 0.2 points
let l2 = coeffs_line line2 0.2 points
let l3 = coeffs_line line3 0.2 points
let l4 = coeffs_line line4 0.2 points

let l1 = step_line line1 0.1 points
let l2 = step_line line2 0.1 points
let l3 = step_line line3 0.1 points
let l4 = step_line line4 0.1 points

(* let l = line_of_coef (x, y, c) *)
(* let (p, v) = l *)
let segs (p,v) =
  let coeff =
    if abs_float v.vx > abs_float v.vy then
      if v.vx > 0.
      then 2.
      else -.2.
    else if v.vy > 0.
    then 2.
    else -.2.
  in
  p, translate p (v *| coeff)

let s = List.map segs [l2; l3; l4]

let draw_lines l = send_lines (List.map segs l)
let () = draw_lines [l2;l4]

