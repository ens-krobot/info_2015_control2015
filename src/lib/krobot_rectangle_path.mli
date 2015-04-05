
type obstacle = Krobot_geom.rect_obj

val find_path :
  src:Krobot_geom.vertice ->
  dst:Krobot_geom.vertice ->
  obstacles:obstacle list ->
  Krobot_geom.vertice list
(** Find a trajectory between [src] and [dst] avoiding the [obstacles].
    If no trajectory was found it returns an empty list.
    The src point is not part of the returned vertice list *)
