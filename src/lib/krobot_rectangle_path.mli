
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
