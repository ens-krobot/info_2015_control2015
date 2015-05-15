
let filter_dist = ref 1
let distance = ref 1.
let tty = ref []
let captures = ref 10
let keep = ref 1
let urg_ocaml_file = ref false

let options = Arg.align [
  "-k", Arg.Set_int keep, " number of mesures for keeping";
  "-c", Arg.Set_int captures, " number of captures";
  "-w", Arg.Set_int filter_dist, " number of additionnal points to filter";
  "-d", Arg.Set_float distance, " Filter distance (default 1 meter)";
  "-tty", Arg.String (fun s -> tty := s :: !tty), " urg tty";
  "-o", Arg.Set urg_ocaml_file, " output hardcoded ocaml code";
]

let usage = "\
Usage: krobot-urg-filter-generator [options]
Generates a table of values to filter out.
Kept points are those for wich the measured distance is bigger that the distance parameter
options are:"

let () = Arg.parse options ignore usage

let captures = !captures
let keep = !keep
let filter_dist = !filter_dist
let urg_ocaml_file = !urg_ocaml_file
let distance = !distance

let copy_data ba =
  let open Bigarray in
  let dim = Bigarray.Array1.dim ba in
  Array.init dim (fun i -> Nativeint.to_float ba.{i} /. 1000.)

let generate_urg tty =
  let urg = Urg_simple.init ~tty () in

  let datum = Array.init captures (fun _ ->
    ignore (Urg_simple.get urg:int);
    copy_data urg.data) in

  let found =
    Array.mapi (fun i _ ->
      let count = ref 0 in
      for capture = 0 to captures - 1 do
        if datum.(capture).(i) <= distance then incr count;
      done;
      !count >= keep)
      datum.(0) in

  let filtered =
    Array.mapi (fun i b ->
      let point = ref b in
      for d = -filter_dist to filter_dist do
        let p = d + i in
        if p > 0 && p < Array.length found then
          point := found.(p) || !point;
      done;
      !point)
      found in
  Urg.urg_disconnect urg.urg;
  urg.id, filtered

open Format

let print_ocaml (id, filtered) =
  if id = Krobot_config.urg_up_id then
    printf "let urg_up_filter' =@."
  else if id = Krobot_config.urg_down_id then
    printf "let urg_down_filter' =@."
  else printf "urg id: %s@." id;
  printf "@[<1>[|";
  Array.iter (fun b -> printf "%b;@ " b) filtered;
  printf "@]|]@."

let print_config_file data =
  Array.iter (function
    | true -> print_char 't'
    | false -> print_char 'f') data;
  print_char '\n'

let () =
  if urg_ocaml_file then
    let urg_data = List.map generate_urg !tty in
    List.iter print_ocaml urg_data;
    Format.printf "let urg_filtered_distance' = %f@." distance
  else
    match !tty with
    | [ tty1; tty2 ] -> begin
        let id1, data1 = generate_urg tty1 in
        let id2, data2 = generate_urg tty2 in
        let urg_up =
          if id1 = Krobot_config.urg_up_id then data1
          else if id2 = Krobot_config.urg_up_id then data2
          else failwith "up_urg not found"
        in
        let urg_down =
          if id1 = Krobot_config.urg_down_id then data1
          else if id2 = Krobot_config.urg_down_id then data2
          else failwith "up_down not found"
        in
        print_config_file urg_down;
        print_config_file urg_up;
        print_float distance;
        print_char '\n';
        flush stdout
      end
    | _ ->
      failwith "two urgs needed for printing config file"
