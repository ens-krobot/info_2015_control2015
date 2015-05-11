(* #use "topfind";;
   #require "krobot";; *)

let debug = false

open Krobot_geom

type obstacle = rect_obj
type segment = vertice * vertice
type distance = float

module Vertice = struct
  type t = vertice
  let compare v1 v2 =
    let c = compare v1.x v2.x in
    if c <> 0 then c
    else compare v1.y v2.y
end
module VerticeSet = Set.Make(Vertice)
module VerticeMap = Map.Make(Vertice)

module Vertice_with_cost = struct
  type t = {
    vertice : vertice;
    path : vertice list;
    path_length : distance;
    cost : distance;
  }
  let compare v1 v2 = compare v1.cost v2.cost
end
module VerticeHeap = Krobot_heap.Make(Vertice_with_cost)

type graph =
  {
    vertices : VerticeSet.t;
    blocking : segment list;
    blocking_tree : segment Krobot_spatial_search.t;
  }

type state =
  {
    queue : VerticeHeap.t;
    not_visited : VerticeSet.t;
  }

let count_intersect = ref 0
let count_steps = ref 0

let init_state ~src ~dst vertices =
  let v : Vertice_with_cost.t =
    { vertice = src;
      path_length = 0.;
      path = [];
      cost = distance src dst } in
  { queue = VerticeHeap.insert v VerticeHeap.empty;
    not_visited = vertices }

let epsilon = 0.00000001

let add_segment_to_tree tree ((v1, v2) as s) =
  let box = rect_bounding_box (v1, v2) in
  Krobot_spatial_search.add s box tree

let add_vertices_and_blocking inflate ( vertices, blocking ) (v1,v2) =
  let {x = x1; y = y1} = v1 in
  let {x = x2; y = y2} = v2 in
  let radius = (max Krobot_config.pathfinding_min_radius_to_consider
                  (Krobot_config.robot_radius +. Krobot_config.safety_margin +. inflate)) in
  let min_x = min x1 x2 -. radius in
  let max_x = max x1 x2 +. radius in
  let min_y = min y1 y2 -. radius in
  let max_y = max y1 y2 +. radius in
  let vertices =
    vertices
    |> VerticeSet.add {x = min_x ; y = min_y}
    |> VerticeSet.add {x = min_x ; y = max_y}
    |> VerticeSet.add {x = max_x ; y = min_y}
    |> VerticeSet.add {x = max_x ; y = max_y} in
  (* reduce that by epsilon to avoid numerical problems *)
  let min_x = min_x +. epsilon in
  let max_x = max_x -. epsilon in
  let min_y = min_y +. epsilon in
  let max_y = max_y -. epsilon in
  let s1 = {x = min_x ; y = min_y}, {x = min_x ; y = max_y} in
  let s2 = {x = min_x ; y = max_y}, {x = max_x ; y = max_y} in
  let s3 = {x = max_x ; y = max_y}, {x = max_x ; y = min_y} in
  let s4 = {x = max_x ; y = min_y}, {x = min_x ; y = min_y} in
  let blocking =
    s1 :: s2 :: s3 :: s4 :: blocking in
  ( vertices, blocking )

(** list graph vertices: points where it is allowed to go.
    i.e. les sommets des rectangles étendus par le rayon du robot
    les obstacles fixes ne sont pas impactés par inflate *)
let graph_vertices : dst:vertice -> inflate:float -> fixed_obstacles:obstacle list ->
  obstacles:obstacle list -> graph =
  fun ~dst ~inflate ~fixed_obstacles ~obstacles ->
  let (vertices, blocking) =
    List.fold_left (add_vertices_and_blocking 0.)
      ( VerticeSet.singleton dst, [] )
      fixed_obstacles
  in
  let (vertices, blocking) =
    List.fold_left (add_vertices_and_blocking inflate)
      (vertices, blocking)
      obstacles
  in
  let min_x = VerticeSet.fold (fun { x } min_x -> min min_x x) vertices min_float in
  let max_x = VerticeSet.fold (fun { x } max_x -> max max_x x) vertices max_float in
  let min_y = VerticeSet.fold (fun { y } min_y -> min min_y y) vertices min_float in
  let max_y = VerticeSet.fold (fun { y } max_y -> max max_y y) vertices max_float in
  let world_box : Krobot_spatial_search.world_box =
    { min_x; max_x; min_y; max_y }
  in
  let blocking_tree =
    List.fold_left add_segment_to_tree
      Krobot_spatial_search.(empty world_box)
      blocking
  in
  { vertices; blocking; blocking_tree }

let first_intersections graph ~src ~dst =
  let intersections =
    Krobot_utils.filter_map (fun s -> segment_intersect s (src, dst)) graph.blocking
  in
  match intersections with
  | [] -> None
  | h :: t ->
    let _, intersection =
      List.fold_left (fun ((d, min) as acc) p ->
        let d' = distance src p in
        if d' < d then
          d', p
        else
          acc)
        (distance src h, h) t in
    Some intersection

(* let exists_intersection graph segment = *)
(*   let blocking = Krobot_spatial_search.segment_collisions segment graph.blocking_tree in *)
(*   (\* let blocking = List.map (fun v -> (v, v)) graph.blocking in *\) *)
(*   List.exists (fun (s,_) -> *)
(*     incr count_intersect; *)
(*     segment_intersect s segment <> None) blocking *)

let print_vert ppf { x; y } =
  Format.fprintf ppf "{ %f, %f }" x y

let exists_intersection graph segment =
  let test s1 s2 =
    if debug then
      Format.printf "intersect (%a - %a) (%a - %a)@."
        print_vert (fst s1) print_vert (snd s1)
        print_vert (fst s2) print_vert (snd s2);
    incr count_intersect;
    segment_intersect s1 s2 <> None in
  Krobot_spatial_search.find_segment_collision ~test segment graph.blocking_tree

(** Les voisins du sommet src: l'ensemble des sommets [s] de [graph] pour
    lesquels il n'y a pas d'intersection entre le segment [src, s] et un
    segment de graph.blocking *)
let neighbors : src:vertice -> graph -> VerticeSet.t = fun ~src graph ->
  VerticeSet.filter (fun v -> not (exists_intersection graph (src, v))) graph.vertices

type step =
  | Found of vertice list
  | Unreachable
  | Continue of state

let next_to_evaluate : VerticeHeap.t -> VerticeHeap.t * Vertice_with_cost.t = fun queue ->
  let v = VerticeHeap.find_min queue in
  let queue = VerticeHeap.delete_min queue in
  queue, v

let step : dst:vertice -> state -> graph -> step = fun ~dst state graph ->
  incr count_steps;
  if VerticeHeap.is_empty state.queue then
    Unreachable
  else
    let queue, vertice_path = next_to_evaluate state.queue in
    let neighbors =
      neighbors ~src:vertice_path.vertice
        { graph with vertices = state.not_visited } in
    if VerticeSet.mem dst neighbors then
      Found (List.rev (dst :: vertice_path.path))
    else
      let state =
        VerticeSet.fold (fun vertice state ->
          let path_length =
            vertice_path.path_length +.
            distance vertice_path.vertice vertice in
          let s = Vertice_with_cost.{
            vertice;
            path = vertice :: vertice_path.path;
            path_length;
            cost = path_length +. distance vertice dst } in
          { queue = VerticeHeap.insert s state.queue;
            not_visited = VerticeSet.remove vertice state.not_visited})
          neighbors { state with queue }
      in
      Continue state

let rec loop ~dst state graph =
  match step ~dst state graph with
  | Found l -> l
  | Unreachable -> []
  | Continue state ->
    loop ~dst state graph

let find_path ~src ~dst ~inflate ~fixed_obstacles ~obstacles =
  count_intersect := 0;
  count_steps := 0;

  let t1 = Unix.gettimeofday () in

  let graph = graph_vertices ~dst ~inflate ~fixed_obstacles ~obstacles in

  let t2 = Unix.gettimeofday () in

  let state = init_state ~src ~dst graph.vertices in

  let t3 = Unix.gettimeofday () in

  let r = loop ~dst state graph in

  let t4 = Unix.gettimeofday () in

  if debug then
    Format.printf "steps %i\nintersect %i\nt graph: %f init %f loop %f\ndepths %a\n@."
      !count_steps
      !count_intersect
      (t2 -. t1)
      (t3 -. t2)
      (t4 -. t3)
      Krobot_spatial_search.print_depths graph.blocking_tree;

  r


let radius = (Krobot_config.robot_radius +. Krobot_config.safety_margin)

let is_colliding_object ~inflate obstacle point =
  is_inside_bounding_box point
    (expand_bounding_box (rect_bounding_box obstacle)
       (max Krobot_config.pathfinding_min_radius_to_consider
          (radius +. inflate)))

let colliding ~inflate ~obstacles ~fixed_obstacles point =
  List.filter (fun obj -> is_colliding_object ~inflate:0. obj point) fixed_obstacles @
  List.filter (fun obj -> is_colliding_object ~inflate obj point) obstacles

let has_collision ~inflate ~obstacles ~fixed_obstacles point =
  List.exists (fun obj -> is_colliding_object ~inflate:0. obj point) fixed_obstacles ||
  List.exists (fun obj -> is_colliding_object ~inflate obj point) obstacles

type collision = {
  prefix_without_collision : Krobot_geom.vertice list;
  collision : Krobot_geom.vertice;
  distance : float;
}

let first_collision ~src ~path ~obstacles =
  let graph = graph_vertices ~dst:src ~inflate:0. ~fixed_obstacles:obstacles ~obstacles:[] in
  let rec loop src path = match path with
    | [] -> None
    | h :: t ->
      match first_intersections graph ~src ~dst:h with
      | Some collision ->
        (* Printf.printf "collision (%.02f,%.02f) (%.02f,%.02f) (%.02f,%.02f),%f\n%!" *)
        (*   src.x src.y h.x h.y collision.x collision.y *)
        (*   (distance src collision); *)
        Some { collision;
               prefix_without_collision = [];
               distance = distance src collision }
      | None ->
        match loop h t with
        | None -> None
        | Some collision_info ->
          (* Printf.printf "acc collision %f\n%!" *)
          (*   (distance src h +. collision_info.distance); *)
          Some { collision_info with
                 prefix_without_collision = h :: collision_info.prefix_without_collision;
                 distance = distance src h +. collision_info.distance }
  in
  if has_collision ~inflate:0. ~obstacles:[] ~fixed_obstacles:obstacles src then
    Some {
      collision = src;
      prefix_without_collision = [];
      distance = 0. }
  else
    loop src path

let first_position_non_colliding ~inflate ~all_obstacles ~src direction =
  let colliding, not_colliding =
    List.partition (fun obj -> is_colliding_object ~inflate obj src) all_obstacles in
  let max_distance = 10. in
  let dst = translate src (normalize direction *| max_distance) in
  let graph = graph_vertices ~dst ~inflate:0. ~fixed_obstacles:not_colliding ~obstacles:[] in
  let bound =
    match first_intersections graph ~src ~dst with
    | None -> dst
    | Some bound -> bound in
  if has_collision ~inflate ~obstacles:[] ~fixed_obstacles:colliding bound then
    (* If the first intersection with the world is still too close
       from an original object, we consider that we can't escape *)
    None
  else
    let co_graph = graph_vertices ~dst:bound ~inflate:0. ~fixed_obstacles:colliding ~obstacles:[] in
    let first_acceptable =
      match first_intersections co_graph ~src:bound ~dst:src with
      | None -> src
      | Some bound -> bound in
    Some first_acceptable

let escaping_directions ~all_obstacles ~src:origin =
  let colliding_obstacles = colliding ~inflate:0. ~fixed_obstacles:all_obstacles ~obstacles:[] origin in
  (* the closest points of each obstacle too close *)
  let closest_points = List.map (fun obstacle ->
    let bb = rect_bounding_box obstacle in
    let _, point = distance_bounding_box origin bb in
    point)
    colliding_obstacles
  in
  (* the direction of the closest points from the origin *)
  let forbidden_directions = List.map (fun point ->
    angle (vector origin point))
    closest_points
  in
  match forbidden_directions with
  | [] -> AngleSet.all
  | h :: t ->
    let mk forbidden_direction =
      let set =
        AngleSet.(complement
                    (create ~bisect:forbidden_direction ~width:(pi/.2. -. 0.05))) in
      (* Format.printf "mk: %a@." AngleSet.print set; *)
      set
    in
    let set =
      List.fold_left (fun set forbidden_direction ->
        let forbidden_set = mk forbidden_direction in
        AngleSet.(intersection set forbidden_set))
        (mk h) t
    in
    (* Format.printf "set: %a@." AngleSet.print set; *)
    set

type escaping_path =
  { escape_point : Krobot_geom.vertice;
    path : Krobot_geom.vertice * Krobot_geom.vertice list }

type pathfinding_result =
  | No_path of string
  | Simple_path of Krobot_geom.vertice * Krobot_geom.vertice list
  | Escaping_path of escaping_path

let filter_directions (l:AngleSet.t) =
  match (l:>AngleSet.a list) with
  | [] -> []
  | h :: t ->
    let close_angle { AngleSet.bisect = a1 } { AngleSet.bisect = a2 } =
      (* assumes a1 and a2 are between - pi and pi *)
      let threshold = 0.1 in
      (a1 -. a2 < threshold) ||
      (a1 -. a2 > 2. *. pi -. threshold)
    in
    let rec aux v acc rem =
      let rem = List.filter (fun r -> not (close_angle v r)) rem in
      let acc = v :: acc in
      match rem with
      | [] -> acc
      | h :: t -> aux h acc t
    in
    aux h [] t

let find_path_for_directions ~src ~dst ~inflate ~obstacles ~fixed_obstacles sectors =
  let all_obstacles = fixed_obstacles @ obstacles in
  let rec aux err = function
    | [] -> No_path err
    | sector :: rest ->

      let bisect = sector.AngleSet.bisect in
      let dir = vector_of_polar ~norm:1. ~angle:bisect in
      match first_position_non_colliding ~inflate:0. ~all_obstacles ~src dir with
      | None ->
        aux "nowhere to go away" rest
      | Some start ->

        (* Hackish: we extend this a bit to avoid floating point problems *)
        let v = vector src start in
        let start = translate src (normalize v *| (norm v +. 0.0001)) in

        match find_path ~src:start ~inflate ~dst ~obstacles ~fixed_obstacles with
        | [] -> aux "no path after escaping" rest
        | h::t ->
          Escaping_path {escape_point = start;
                         path = (h,t)}
  in
  aux "cannot go away from obstacles" sectors

let inflate_step = 0.02

let escape_and_pathfind ~src ~dst ~inflate ~obstacles ~fixed_obstacles =
  (* escaping must be done with real sizes: so inflate = 0 *)
  let all_obstacles = fixed_obstacles @ obstacles in
  let dir = filter_directions (escaping_directions ~all_obstacles ~src) in
  find_path_for_directions ~src ~dst ~inflate ~obstacles ~fixed_obstacles dir

let rec colliding_pathfinding ~src ~dst ~inflate ~fixed_obstacles ~obstacles =
  (* Printf.printf "try with inflate: %f (%f)\n%!" inflate (radius +. inflate); *)
  if (radius +. inflate) < Krobot_config.pathfinding_min_radius_to_consider
  then No_path "no path"
  else if has_collision ~inflate:0. ~obstacles ~fixed_obstacles src then
    match escape_and_pathfind ~src ~dst ~inflate ~obstacles ~fixed_obstacles with
    | No_path _ ->
      colliding_pathfinding ~src ~dst ~inflate:(inflate -. inflate_step) ~obstacles ~fixed_obstacles
    | r -> r
  else if has_collision ~inflate ~obstacles ~fixed_obstacles src then
    (* here inflate must be positive (otherwise the first branch would
       have been taken *)
    match escape_and_pathfind ~src ~dst ~inflate ~obstacles ~fixed_obstacles with
    | No_path _ ->
      colliding_pathfinding ~src ~dst ~inflate:(inflate -. inflate_step) ~obstacles ~fixed_obstacles
    | r -> r
  else match find_path ~src ~dst ~inflate ~obstacles ~fixed_obstacles with
    | [] ->
      colliding_pathfinding ~src ~dst ~inflate:(inflate -. inflate_step) ~obstacles ~fixed_obstacles
    | h::t -> Simple_path (h,t)
