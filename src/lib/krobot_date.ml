
let time_zero = Unix.gettimeofday ()
let current_time () = Unix.gettimeofday () -. time_zero

type t = float

let add t d = t +. d

let (<=) t1 t2 = t1 <= t2

let now () = current_time ()

let time_to_wait dest =
  max 0. (dest -. now ())

let print ppf date = Format.fprintf ppf "%.02f" date
let pr () date = Printf.sprintf "%.02f" date
