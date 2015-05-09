
type t

val add : t -> float -> t
(* [add date t] adds t seconds to [date] *)

val now : unit -> t

val time_to_wait : t -> float

val print : Format.formatter -> t -> unit
val pr : unit -> t -> string
