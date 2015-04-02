#use "topfind";;
#require "krobot";;
#require "graphics";;

open Krobot_geom
open Krobot_rectangle_path
open Graphics

let () = open_graph " 500x500"

let draw_obstacle (v1,v2) =
  let {x = x1; y = y1} = v1 in
  let {x = x2; y = y2} = v2 in
  let radius = Krobot_config.robot_radius in
  let min_x = min x1 x2 -. radius in
  let max_x = max x1 x2 +. radius in
  let min_y = min y1 y2 -. radius in
  let max_y = max y1 y2 +. radius in
  draw_rect
    (int_of_float min_x)
    (int_of_float min_y)
    (int_of_float (max_x -. min_x))
    (int_of_float (max_y -. min_y))

let draw_line v1 v2 =
  moveto (int_of_float v1.x) (int_of_float v1.y);
  lineto (int_of_float v2.x) (int_of_float v2.y)

let draw_lines src l =
  moveto (int_of_float src.x) (int_of_float src.y);
  List.iter (fun v -> lineto (int_of_float v.x) (int_of_float v.y)) l

let draw_obstacles obs =
  List.iter draw_obstacle obs

let obstacles =
  [
    { x = 100.; y = 100. }, { x = 200.; y = 110. };
    { x = 140.; y = 120. }, { x = 420.; y = 140. };
    { x = 180.; y = 130. }, { x = 190.; y = 220. };
    { x = 190.; y = 220. }, { x = 220.; y = 210. };
    { x = 200.; y = 180. }, { x = 240.; y = 190. };
  ]

let src =
  { x = 210.; y = 95. }

let dst =
  { x = 210.; y = 160. }

let path = find_path ~src ~dst ~obstacles

let () =
  clear_graph ();
  draw_obstacles obstacles;
  draw_lines src path

