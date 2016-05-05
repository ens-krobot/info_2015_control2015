
type ('a, 'b) ransac = {
  model : 'a array -> 'b;
  data : 'a array;
  subset_size : int;
  rounds : int;
  distance : 'a -> 'b -> float;
  filter_distance : float;
  minimum_valid : int;
  partition : int -> 'a array -> 'a array * 'a array;
  error : 'a array -> 'b -> float;
}

type ('a, 'b) ransac_result =
  { model : 'b;
    in_model : 'a array;
    out_of_model : 'a array;
    error : float }

val random_partition : int -> 'a array -> 'a array * 'a array

val one_round : ('a, 'b) ransac -> ('a, 'b) ransac_result option

val ransac : ('a, 'b) ransac -> ('a, 'b) ransac_result option
