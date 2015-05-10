(*
 * krobot_lcd.mli
 * ------------------
 * Copyright : (c) 2015, Pierre Chambart <chambart@crans.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

val line : int -> string option -> Krobot_message.t list

val display :
  ?l1 : string ->
  ?l2 : string ->
  ?l3 : string ->
  ?l4 : string ->
  unit ->
  Krobot_message.t list
(** prepare a sequence of messages for diplaying those lines on the
    lcd screen. Each line must be shorter that 20 characters *)

val text : string -> Krobot_message.t list

val send_text : Krobot_bus.t -> string -> unit Lwt.t

val send_line : Krobot_bus.t -> int -> string -> unit Lwt.t

val clear : Krobot_bus.t -> unit Lwt.t
