open Krobot_geom

type 'a t

type world_box = {
  min_x : float;
  min_y : float;
  max_x : float;
  max_y : float;
}

type bounding_box = {
  min_x : float;
  min_y : float;
  max_x : float;
  max_y : float;
}

val empty : ?max_depth:int -> world_box -> 'a t

val add : 'a -> bounding_box -> 'a t -> 'a t

val segment_collisions : vertice * vertice -> 'a t -> ('a * bounding_box) list