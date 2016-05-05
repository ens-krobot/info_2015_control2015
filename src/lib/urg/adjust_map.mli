type transform = { x : float; y: float; th: float }
(** represent a translate then rotate transformation *)

type point = Krobot_geom.vertice = { x : float; y : float }
type line = { a : point; b : point }
type map = line list

val distance_map : map -> point -> float

val solve : ?rounds:int -> ?init:transform -> map -> point list -> transform * int
(** [solve ... = transform, rank] *)
val invert_transformation : transform -> transform
(** [compose tr (invert_transformation tr)] is the identity  *)

val ransac_param :
  ?rounds:int -> ?init:transform -> map -> point array -> (point, transform) Ransac.ransac

