open Krobot_bus
open Krobot_message
open Krobot_geom
open Krobot_world_update

type world = Krobot_world_update.world

type state = Idle

type message =
  | CAN of Krobot_message.t
  | Msg of Krobot_bus.message

type output = {
  timeout : float; (* in seconds *)
  messages : message list;
  state : state;
  world : world;
}

type input =
  | Message of Krobot_bus.message
  | World_updated of world_update
  | Timeout

let init_state = Idle

let update_world world msg =
  match Krobot_world_update.update_world world msg with
  | Some (w, u) -> w, World_updated u
  | None -> world, Message msg

let step (input:input) (world:world) (state:state) : output =
  { timeout = 1.;
    messages = [];
    state = state;
    world = world }

let send_msg bus time = function
  | CAN c -> Krobot_bus.send bus (time, CAN (Info, Krobot_message.encode c))
  | Msg m -> Krobot_bus.send bus (time, m)

let main_loop bus iter =
  let rec aux world state timeout =
    lwt msg = Krobot_entry_point.next ~timeout:Krobot_date.(time_to_wait timeout) iter  in
    let world, input =
      match msg with
      | Krobot_entry_point.Timeout ->
        (* Lwt_log.ign_info_f "timeout"; *)
        world, Timeout
      | Krobot_entry_point.Message(t,m) ->
        update_world world m
    in
    let output = step input world state in
    let time = Unix.gettimeofday () in
    lwt () = Lwt_list.iter_s (fun m -> send_msg bus time m) output.messages in
    let timeout_date = Krobot_date.(add (now ()) output.timeout) in
    aux output.world output.state timeout_date in
  aux init_world init_state (Krobot_date.now ())

module S : Krobot_entry_point.S = struct
  let name = "control"
  let options = []
  let main bus ev =
    let iter = Krobot_entry_point.iterator_from_react ev in
    main_loop bus iter
end

module Run = Krobot_entry_point.Make(S)
