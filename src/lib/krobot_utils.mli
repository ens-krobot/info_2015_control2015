
val filter_map : ('a -> 'b option) -> 'a list -> 'b list

val array_rev : 'a array -> 'a array

type urg_filters =
  { urg_up_filter_file : bool array;
    urg_down_filter_file : bool array }

val read_urg_filter_file : filename:string -> urg_filters option
