(*
 * krobot_bus.ml
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Lwt_react
open Krobot_geom

let section = Lwt_log.Section.make "krobot(bus)"
let port = 50000

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type frame_source = Elec | Info

type urg_id = Up | Down

type obstacle =
  | Rectangle of vertice * vertice

type collision =
  | Col_bezier of Krobot_geom.Bezier.curve * (float * (Krobot_geom.vertice * float) option) list
  | Col_rotation of (Krobot_geom.vertice * float) list

type request_id = int

type mover_message =
  | Planning_error of request_id
  | Planning_done of request_id
  | Idle
  | Not_idle of string
  | Collision of request_id
  | First_obstacle of vertice option
  | Request_completed of request_id

type move_kind =
  | Normal
  | Constrained
  | Direct

type message =
  | CAN of frame_source * Krobot_can.frame
  | Log of string
  | Send
  | Kill of string
  | Trajectory_path of Bezier.curve list
  | Trajectory_set_vertices of vertice list
  | Trajectory_add_vertice of vertice * vector option
  | Trajectory_simplify of float
  | Trajectory_go of request_id * move_kind
  | Goto of request_id * vertice
  | Trajectory_find_path
  | Request_mover_state
  | Mover_message of mover_message
  | Obstacles of obstacle list
  | Sharps of float array
  | Set_fake_beacons of vertice option * vertice option
  | Collisions of collision
  | Urg of urg_id * vertice array
  | Urg_lines of (vertice*vertice) array
  | Beacon_raw of (int * int * int * int * int * int
      * int * int * int * int * int)
  | Match_start
  | Vision_find_target of int * string
  | Vision_find_target_response of int * string * ((int * int) list)
  | Run_ax12_sequence of (string * Krobot_ax12_format.action list)
      (** log * actions *)
  | Run_ax12_framed_sequence of (string * Krobot_ax12_format.keyframe_dict * (int * int) list)
      (** log * keyframe dictionary * actions *)
  | Finished_ax12_sequence of string
  | Stop
  | Lift_up of int
  | Lift_down of int
  | Lift_grip_open of int
  | Lift_grip_close of int
  | Lift_door_open of int
  | Lift_door_close of int
  | Lift_action_done of int

type t = {
  oc : Lwt_io.output_channel;
  recv : (float * message) event;
}

(* +-----------------------------------------------------------------+
   | Message printer                                                 |
   +-----------------------------------------------------------------+ *)

open Printf

let string_of_vertice v =
  sprintf "{ x = %f; y = %f }" v.x v.y

let string_of_obstacle = function
  | Rectangle (v1, v2) ->
    sprintf "{ v1 = %s; v2 = %s }" (string_of_vertice v1) (string_of_vertice v2)

let string_of_vector v =
  sprintf "{ vx = %f; vy = %f }" v.vx v.vy

let string_of_option f v =
  match v with
    | Some x ->
      "Some " ^ (f x)
    | None ->
      "None"

let string_of_urg_id (id:urg_id) =
  match id with
  | Down -> "down"
  | Up -> "up"

let string_of_message = function
  | CAN(source, frame) ->
      sprintf
        "CAN(%s, %s)"
        (match source with Elec -> "Elec" | Info -> "Info")
        (Krobot_can.string_of_frame frame)
  | Log str ->
      sprintf
        "Log %S"
        str
  | Send ->
      "Send"
  | Kill name ->
      sprintf
        "Kill %S"
        name
  | Trajectory_path curves ->
      sprintf
        "Trajectory_path [%s]"
        (String.concat "; " (List.map Bezier.string_of_curve curves))
  | Trajectory_set_vertices l ->
      sprintf
        "Trajectory_set_vertices [%s]"
        (String.concat "; " (List.map string_of_vertice l))
  | Trajectory_add_vertice (v, dir) ->
      sprintf
        "Trajectory_add_vertice %s %s"
        (string_of_vertice v) (string_of_option string_of_vector dir)
  | Trajectory_simplify tolerance ->
      sprintf
        "Trajectory_simplify %f"
        tolerance
  | Trajectory_go (req_id, kind) ->
    let kind = match kind with
      | Normal -> "Normal"
      | Constrained -> "Constrained"
      | Direct -> "Direct"
    in
    Printf.sprintf "Trajectory_go (%X, %s)" req_id kind
  | Goto (req_id, v) ->
    sprintf
      "Goto (%X, %s)" req_id (string_of_vertice v)
  | Trajectory_find_path ->
    "Trajectory_find_path"
  | Request_mover_state ->
    "Request_mover_state"
  | Mover_message mover_message ->
    begin match mover_message with
      | Planning_error id -> Printf.sprintf "Mover: Planning_error %X" id
      | Planning_done id ->  Printf.sprintf "Mover: Planning_done %X" id
      | Idle -> "Mover: Idle"
      | Not_idle s -> Printf.sprintf "Mover: Not_idle %s" s
      | Collision id ->  Printf.sprintf "Mover: Collision %X" id
      | First_obstacle v ->
        Printf.sprintf "Mover: First_obstacle %s" (string_of_option string_of_vertice v)
      | Request_completed id ->
        Printf.sprintf "Mover: Request_completed %X" id
    end
  | Obstacles obstacles ->
      sprintf
        "Obstacles [%s]"
        (String.concat "; " (List.map string_of_obstacle obstacles))
  | Sharps a ->
      sprintf
        "Sharps [|%s|]"
        (String.concat "; " (List.map string_of_float (Array.to_list a)))
  | Set_fake_beacons (b1, b2) ->
      sprintf
        "Set_fake_beacons (%s, %s)"
        (string_of_option string_of_vertice b1)
        (string_of_option string_of_vertice b2)
  | Collisions (Col_rotation l) ->
      sprintf
        "Collisions rotation ([%s])"
        (String.concat "; "
           (List.map (fun (v,r) -> sprintf "(%s, %f)" (string_of_vertice v) r) l))
  | Collisions (Col_bezier (curve, l)) ->
      sprintf
        "Collisions bezier (<curve>, [%s])"
        (String.concat "; "
           (List.map
              (fun (u, opt) ->
                sprintf "(%f, %s)"
                  u
                  (match opt with
                    | None ->
                      "None"
                    | Some (v, r) ->
                      sprintf "Some (%s, %f)" (string_of_vertice v) r))
              l))
  | Urg (id, distances) ->
      sprintf "Urg %s (many_points...)" (string_of_urg_id id)
  | Urg_lines lines ->
      sprintf "Urg_lines (many_lines...)"
  | Beacon_raw _ ->
      sprintf "Raw beacon packet"
  | Match_start ->
      sprintf "Match start"
  | Vision_find_target (id,camera) ->
      sprintf "Vision find target %i %s" id camera
  | Vision_find_target_response (id,camera,points) ->
      sprintf "Vision find target response %i %s %i points" id camera (List.length points)
  | Run_ax12_sequence (log,_) ->
      sprintf "Run_ax12_sequence %s" log
  | Run_ax12_framed_sequence (log,_,_) ->
      sprintf "Run_ax12_framed_sequence %s" log
  | Finished_ax12_sequence log ->
      sprintf "Finished_ax12_sequence %s" log
  | Stop ->
      "Stop"
  | Lift_up (id) ->
      sprintf "Lift_up(%d)" id
  | Lift_down (id) ->
      sprintf "Lift_down(%d)" id
  | Lift_grip_open (id) ->
      sprintf "Lift_grip_open(%d)" id
  | Lift_grip_close (id) ->
      sprintf "Lift_grip_close(%d)" id
  | Lift_door_open (id) ->
      sprintf "Lift_door_open(%d)" id
  | Lift_door_close (id) ->
      sprintf "Lift_door_close(%d)" id
  | Lift_action_done (id) ->
      sprintf "Lift_action_done(%d)" id

(* +-----------------------------------------------------------------+
   | Sending/receiving messages                                      |
   +-----------------------------------------------------------------+ *)

let send bus v = Lwt_io.write_value bus.oc v
let recv bus = bus.recv

(* +-----------------------------------------------------------------+
   | Dispatching incomming messages                                  |
   +-----------------------------------------------------------------+ *)

let dispatch ic emit =
  try_lwt
    while_lwt true do
      (* Read one message. *)
      lwt v = Lwt_io.read_value ic in

      (* Emit it. *)
      begin
        try
          emit v
        with exn ->
          ignore (Lwt_log.error ~section ~exn "message handler failed with")
      end;

      return ()
    done
  with exn ->
    ignore (Lwt_log.error ~section ~exn "lost connection");
    exit 1

(* +-----------------------------------------------------------------+
   | Logger                                                          |
   +-----------------------------------------------------------------+ *)

let logger bus =
  Lwt_log.make
    (fun section level lines ->
      let buf = Buffer.create 42 in
      List.iter
        (fun line ->
          Buffer.clear buf;
          Lwt_log.render ~buffer:buf ~template:"$(name)[$(section)]: $(message)" ~section ~level ~message:line;
          ignore (send bus (Unix.gettimeofday (), Log(Buffer.contents buf))))
        lines;
      return ())
    return

(* +-----------------------------------------------------------------+
   | Creation                                                        |
   +-----------------------------------------------------------------+ *)

let bus = lazy(
  try_lwt
    (* Open a connection to the local HUB. *)
    lwt ic, oc = Lwt_io.open_connection (Unix.ADDR_INET(Unix.inet_addr_loopback, port)) in

    (* The event for incomming message. *)
    let recv, emit = E.create () in

    (* Dispatch message forever. *)
    ignore (dispatch ic emit);

    (* Create the bus. *)
    let bus = { oc; recv } in

    (* Send logs over the bus. *)
    Lwt_log.default := Lwt_log.broadcast [!Lwt_log.default; logger bus];

    return bus
  with exn ->
    ignore (Lwt_log.error ~section ~exn "failed to connect to the local hub");
    exit 1
)

let get () = Lazy.force bus
