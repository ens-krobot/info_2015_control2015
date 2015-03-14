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

type collision =
  | Col_bezier of Krobot_geom.Bezier.curve * (float * (Krobot_geom.vertice * float) option) list
  | Col_rotation of (Krobot_geom.vertice * float) list

type message =
  | CAN of frame_source * Krobot_can.frame
  | Log of string
  | Send
  | Kill of string
  | Trajectory_path of Bezier.curve list
  | Trajectory_set_vertices of vertice list
  | Trajectory_add_vertice of vertice
  | Trajectory_simplify of float
  | Trajectory_go
  | Trajectory_find_path
  | Objects of (vertice*float) list
  | Sharps of float array
  | Set_fake_beacons of vertice option * vertice option
  | Collisions of collision
  | Urg of vertice array
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

let string_of_object (v,d) =
  sprintf "{ x = %f; y = %f; d = %f }" v.x v.y d

let string_of_vector v =
  sprintf "{ vx = %f; vy = %f }" v.vx v.vy

let string_of_option f v =
  match v with
    | Some x ->
      "Some " ^ (f x)
    | None ->
      "None"

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
  | Trajectory_add_vertice v ->
      sprintf
        "Trajectory_add_vertice %s"
        (string_of_vertice v)
  | Trajectory_simplify tolerance ->
      sprintf
        "Trajectory_simplify %f"
        tolerance
  | Trajectory_go ->
      "Trajectory_go"
  | Trajectory_find_path ->
      "Trajectory_find_path"
  | Objects objects ->
      sprintf
        "Objects [%s]"
        (String.concat "; " (List.map string_of_object objects))
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
  | Urg distances ->
      sprintf "Urg (many_points...)"
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
