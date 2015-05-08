
type obstacle = Krobot_geom.rect_obj

val find_path :
  src:Krobot_geom.vertice ->
  dst:Krobot_geom.vertice ->
  obstacles:obstacle list ->
  Krobot_geom.vertice list
(** Find a trajectory between [src] and [dst] avoiding the [obstacles].
    If no trajectory was found it returns an empty list.
    The src point is not part of the returned vertice list *)

type collision = {
  prefix_without_collision : Krobot_geom.vertice list;
  collision : Krobot_geom.vertice;
  distance : float;
}

val first_collision :
  src:Krobot_geom.vertice ->
  path:Krobot_geom.vertice list ->
  obstacles:obstacle list ->
  collision option
(** If there is a collision between the trajectory and an obstacle, returns
    the prefix without collision and the first collision *)

val escaping_directions :
  obstacles:obstacle list ->
  src:Krobot_geom.vertice ->
  Krobot_geom.AngleSet.t
(** The escaping directions are the directions where we are going away of
    every colliding obstacles *)

type escaping_path =
  { escape_point : Krobot_geom.vertice;
    path : Krobot_geom.vertice * Krobot_geom.vertice list }

type pathfinding_result =
  | Cannot_escape
  | No_path
  | Simple_path of Krobot_geom.vertice * Krobot_geom.vertice list
  | Escaping_path of escaping_path

val colliding_pathfinding :
  src:Krobot_geom.vertice ->
  dst:Krobot_geom.vertice ->
  obstacles:obstacle list ->
  pathfinding_result
(** Find a trajectory between [src] and [dst] avoiding the [obstacles].
    If the original situation is already colliding, it is allowed to
    find a path going away from those. In this case the result is
    'Escaping_path(first_point,path)' where [first_point] is the
    destination of the escaping move, then the rest of the path start
    from [first_point].
    If the original situation is not constrained, the result is like
    find_path *)
