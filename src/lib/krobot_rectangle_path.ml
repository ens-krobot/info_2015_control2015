(* #use "topfind";;
   #require "krobot";; *)

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

let add_vertices_and_blocking ( vertices, blocking ) (v1,v2) =
  let {x = x1; y = y1} = v1 in
  let {x = x2; y = y2} = v2 in
  let radius = (Krobot_config.robot_radius +. Krobot_config.safety_margin) in
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
    i.e. les sommets des rectangles étendus par le rayon du robot *)
let graph_vertices : dst:vertice -> obstacle list -> graph = fun ~dst obstacles ->
  let (vertices, blocking) =
    List.fold_left add_vertices_and_blocking
    ( VerticeSet.singleton dst, [] )
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

let exists_intersection graph segment =
  let blocking = Krobot_spatial_search.segment_collisions segment graph.blocking_tree in
  (* let blocking = List.map (fun v -> (v, v)) graph.blocking in *)
  List.exists (fun (s,_) ->
    incr count_intersect;
    segment_intersect s segment <> None) blocking

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

let find_path ~src ~dst ~obstacles =
  count_intersect := 0;
  count_steps := 0;

  let graph = graph_vertices ~dst obstacles in
  let state = init_state ~src ~dst graph.vertices in
  let r = loop ~dst state graph in

  (* Printf.printf "steps %i\nintersect %i\n%!" *)
  (*   !count_steps *)
  (*   !count_intersect; *)

  r


type collision = {
  prefix_without_collision : Krobot_geom.vertice list;
  collision : Krobot_geom.vertice;
  distance : float;
}

let first_collision ~src ~path ~obstacles =
  let graph = graph_vertices ~dst:src obstacles in
  let rec loop src path = match path with
    | [] -> None
    | h :: t ->
      match first_intersections graph ~src ~dst:h with
      | Some collision ->
        Some { collision;
               prefix_without_collision = [];
               distance = distance src collision }
      | None ->
        match loop h t with
        | None -> None
        | Some collision_info ->
          Some { collision_info with
                 prefix_without_collision = h :: collision_info.prefix_without_collision;
                 distance = distance src h +. collision_info.distance }
  in
  loop src path

let radius = (Krobot_config.robot_radius +. Krobot_config.safety_margin)

let is_colliding_object obstacle point =
  is_inside_bounding_box point
    (expand_bounding_box (rect_bounding_box obstacle) radius)

let colliding ~obstacles point =
  List.filter (fun obj -> is_colliding_object obj point) obstacles

let has_collision ~obstacles point =
  List.exists (fun obj -> is_colliding_object obj point) obstacles

let first_position_non_colliding ~obstacles ~src direction =
  let colliding, not_colliding =
    List.partition (fun obj -> is_colliding_object obj src) obstacles in
  let max_distance = 10. in
  let dst = translate src (normalize direction *| max_distance) in
  let graph = graph_vertices ~dst not_colliding in
  let bound =
    match first_intersections graph ~src ~dst with
    | None -> dst
    | Some bound -> bound in
  if has_collision ~obstacles:colliding bound then
    (* If the first intersection with the world is still too close
       from an original object, we consider that we can't escape *)
    None
  else
    let co_graph = graph_vertices ~dst:bound colliding in
    let first_acceptable =
      match first_intersections co_graph ~src:bound ~dst:src with
      | None -> src
      | Some bound -> bound in
    Some first_acceptable

let escaping_directions ~obstacles ~src:origin =
  let colliding_obstacles = colliding ~obstacles origin in
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
  List.fold_left (fun set forbidden_direction ->
    AngleSet.(intersection set (half forbidden_direction)))
    AngleSet.all forbidden_directions

type escaping_path =
  { escape_point : Krobot_geom.vertice;
    path : Krobot_geom.vertice * Krobot_geom.vertice list }

type pathfinding_result =
  | Cannot_escape
  | No_path
  | Simple_path of Krobot_geom.vertice * Krobot_geom.vertice list
  | Escaping_path of escaping_path

let colliding_pathfinding ~src ~dst ~obstacles =
  if not (has_collision ~obstacles src)
  then match find_path ~src ~dst ~obstacles with
    | [] -> No_path
    | h::t -> Simple_path (h,t)
  else
    let dir = escaping_directions ~obstacles ~src in
    if dir.AngleSet.width <= 0. then
      Cannot_escape
    else begin
      let dir = vector_of_polar ~norm:1. ~angle:dir.AngleSet.bisect in
      match first_position_non_colliding ~obstacles ~src dir with
      | None ->
        Cannot_escape
      | Some start ->
        match find_path ~src:start ~dst ~obstacles with
        | [] -> No_path
        | h::t -> Escaping_path {escape_point = start;
                                 path = (h,t)}
    end
