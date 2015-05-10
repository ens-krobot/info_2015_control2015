open Krobot_bus
open Krobot_message
open Lwt_react

lwt bus = Krobot_bus.get ()

type ax12_info =
    { id : int;
      pos : int;
      speed : int;
      torque : int;
      time : float }

module IntMap = Map.Make(struct type t = int let compare = compare end)

let previous = ref IntMap.empty
let is_previous info =
  try
    let v = IntMap.find info.id !previous in
    (v.pos,v.speed) = (info.pos,info.speed)
  with
    | Not_found -> false

let print_info ({ id; pos; speed; torque; time } as info) =
  previous := IntMap.add id info !previous;
  Printf.printf "id %i,pos %i, speed %i, torque %i, time %f\n%!" id pos speed torque time

let log () =
  E.keep
    (E.map
       (fun (time, message) ->
         match message with
           | CAN(_, frame) ->
             begin
               match Krobot_message.decode frame with
                 | Ax12_State (id,pos,speed,torque) ->
                   let info = { id;pos;speed;torque;time } in
                   if not (is_previous info)
                   then print_info info
                 | _ -> ()
             end
           | _ -> ())
       (Krobot_bus.recv bus))

let ax12_id = ref []
let ax12_delay = ref 0.02
let spec =
  [ "-id", Arg.Int (fun i -> ax12_id := i::(!ax12_id)), "id of the recorded ax12";
    "-delay", Arg.Set_float ax12_delay, "delay between to points"; ]

let msg = "record ax12 movement ax12"
let message _ = Arg.usage spec msg; flush stdout; exit 0
let () = Arg.parse spec message msg

let rec loop_request () =
  (*lwt () = Lwt_unix.sleep 0.01 in*)
  lwt () = Lwt_list.iter_s (fun i ->
    lwt () = Krobot_bus.send bus (Unix.gettimeofday (), CAN (Info, Krobot_message.encode (Ax12_Request_State i))) in
    Lwt_unix.sleep !ax12_delay)
      !ax12_id in
  loop_request ()

let t = loop_request ()
let () = log ()
lwt () = fst (Lwt.wait ())
