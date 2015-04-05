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
  }

type state =
  {
    queue : VerticeHeap.t;
    not_visited : VerticeSet.t;
  }

let init_state ~src ~dst vertices =
  let v : Vertice_with_cost.t =
    { vertice = src;
      path_length = 0.;
      path = [];
      cost = distance src dst } in
  { queue = VerticeHeap.insert v VerticeHeap.empty;
    not_visited = vertices }

let epsilon = 0.00000001

let add_vertices_and_blocking { vertices; blocking } (v1,v2) =
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
  let blocking =
    ({x = min_x ; y = min_y}, {x = min_x ; y = max_y}) ::
    ({x = min_x ; y = max_y}, {x = max_x ; y = max_y}) ::
    ({x = max_x ; y = max_y}, {x = max_x ; y = min_y}) ::
    ({x = max_x ; y = min_y}, {x = min_x ; y = min_y}) :: blocking in
  { vertices; blocking }

(** list graph vertices: points where it is allowed to go.
    i.e. les sommets des rectangles étendus par le rayon du robot *)
let graph_vertices : dst:vertice -> obstacle list -> graph = fun ~dst obstacles ->
  List.fold_left add_vertices_and_blocking
    { vertices = VerticeSet.singleton dst;
      blocking = [] }
    obstacles

let exists_intersection graph segment =
  List.exists (fun s -> segment_intersect s segment <> None) graph.blocking

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
  let graph = graph_vertices ~dst obstacles in
  let state = init_state ~src ~dst graph.vertices in
  loop ~dst state graph
