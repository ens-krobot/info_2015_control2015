(*
 * krobot_bus.mli
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** The krobot bus. *)

open Krobot_geom

type t
  (** Type of a krobot bus connected to the local HUB. *)

val get : unit -> t Lwt.t
  (** [get ()] returns the krobot bus. It exits the program on
      error. *)

val logger : t -> Lwt_log.logger
  (** [logger bus] creates a logger which sends log over [bus]. *)

type frame_source = Elec | Info
    (** The source of CAN frames. *)

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
  | Constrained (* Move assuming potential collisions *)
  | Direct (* Move ignoring the fixed world *)

type team =
  | Green
  | Yellow

(** Type of message exchanged over the bus. *)
type message =
  | CAN of frame_source * Krobot_can.frame
      (** [CAN(source, frmae)] a CAN frame. *)
  | Log of string
      (** A log message. *)
  | Send
      (** Ask for sending parameters. *)
  | Kill of string
      (** Kill the given service. *)

  (** Trajectory messages. *)

  | Trajectory_path of Bezier.curve list
      (** The planned trajectory. *)
  | Trajectory_set_vertices of vertice list
      (** Sets the trajectory. *)
  | Trajectory_add_vertice of vertice * vector option
      (** Add a vertice to the trajectory. *)
  | Trajectory_simplify of float
      (** Simplify the trajectory with the given tolerance. *)
  | Trajectory_go of request_id * move_kind
      (** Follow currently registered trajectory. *)
  | Goto of request_id * vertice
      (** Find a trajectory to that point and go.
          The integer is the request id *)
  | Trajectory_find_path
      (** Find a path avoiding objects. *)

  | Request_mover_state
  | Mover_message of mover_message

  (** Obstacles *)

  | Obstacles of obstacle list
  (** The list of objects on the board. *)

  (** Sharps *)

  | Sharps of float array
      (** Distances measured by the sharps. *)

  (** Fake beacons *)

  | Set_fake_beacons of vertice option * vertice option
      (** Sets the positions of the fake  beacons. *)

  (** Collisions *)

  | Collisions of collision
  (** A curve and a list of collision. *)

  (** distances mesured by the URG (in millimeters) *)
  | Urg of urg_id * vertice array
  | Urg_lines of (vertice*vertice) array

  | Beacon_raw of (int * int * int * int * int * int
      * int * int * int * int * int)

  | Match_start
  | Match_end

  | Vision_find_target of int * string (* request id, selected camera *)
  | Vision_find_target_response of int * string * ((int * int) list)

  | Run_ax12_sequence of (string * Krobot_ax12_format.action list)
      (** log * actions *)
  | Run_ax12_framed_sequence of (string * Krobot_ax12_format.keyframe_dict * (int * int) list)
      (** log * keyframe dictionary * actions *)
  | Finished_ax12_sequence of string
  | Stop

  | Lift_up of int (* request id *)
  | Lift_down of int
  | Lift_grip_open of int
  | Lift_grip_close of int
  | Lift_door_open of int
  | Lift_door_close of int
  | Lift_action_done of int

  | Jack_plugged_in
  | Jack_plugged_out
  | Team_select of team

val string_of_message : message -> string
  (** Returns a string representation of the given message. *)

val send : t -> (float * message) -> unit Lwt.t
  (** [send bus (timestamp, message)] sends a message over the krobot
      bus. *)

val recv : t -> (float * message) React.event
  (** [recv bus] returns the event which receive messages from the
      krobot bus. *)
