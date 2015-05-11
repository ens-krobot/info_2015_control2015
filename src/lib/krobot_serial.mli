(*
 * krobot_serial.ml
 * ----------------
 * Copyright : (c) 2015, Pierre Chambart
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* serial port handling *)

type serial

val open_serial : ?baudrate:int -> path:string -> serial Lwt.t
(** open the connection on the serial port [path] at
    [baudrate] (default 57600) *)

val read_line : serial -> string Lwt.t
(** read a "\n" or "\r\n" line *)

val write_line : serial -> string -> unit Lwt.t
(** write a string and terminates by a "\n" *)

val read_string : max_length:int -> serial -> string Lwt.t
(** read a string *)

val send_string : serial -> string -> unit Lwt.t
(** write a string *)
