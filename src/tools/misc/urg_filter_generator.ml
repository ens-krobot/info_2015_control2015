
let distance = ref 1.
let tty = ref None

let options = Arg.align [
  "-d", Arg.Set_float distance, " Filter distance (default 1 meter)";
  "-tty", Arg.String (fun s -> tty := Some s), " urg tty";
]

let usage = "\
Usage: krobot-urg-filter-generator [options]
Generates a table of values to filter out.
Kept points are those for wich the measured distance is bigger that the distance parameter
options are:"

let () = Arg.parse options ignore usage
let urg = Urg_simple.init ?tty:!tty ()

let captures = 10
let keep = 3
let filter_dist = 1

let copy_data ba =
  let open Bigarray in
  let dim = Bigarray.Array1.dim ba in
  Array.init dim (fun i -> Nativeint.to_float ba.{i} /. 1000.)

let datum = Array.init captures (fun _ ->
  ignore (Urg_simple.get urg:int);
  copy_data urg.data)

let found =
  Array.mapi (fun i _ ->
    let count = ref 0 in
    for capture = 0 to captures - 1 do
      if datum.(capture).(i) <= !distance then incr count;
    done;
    !count >= keep)
    datum.(0)

let filtered =
  Array.mapi (fun i b ->
    let point = ref b in
    for d = -filter_dist to filter_dist do
      let p = d + i in
      if p > 0 && p < Array.length found then
        point := found.(p) || !point;
    done;
    !point)
    found

open Format

let () =
  printf "@[<1>[|";
  Array.iter (fun b -> printf "%b;@ " b) filtered;
  printf "@]|]@."
