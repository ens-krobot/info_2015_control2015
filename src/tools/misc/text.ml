(*
 * text.ml
 * ------------------
 * Copyright : (c) 2015, Pierre Chambart <chambart@crans.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Print on the LCD. *)

open Lwt
open Lwt_react
open Krobot_message
open Krobot_bus

let text =
  let l = Array.to_list (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)) in
  String.concat " " l
let msg = Krobot_lcd.text text

let t =
  (* Open the krobot bus. *)
  lwt bus = Krobot_bus.get () in
  Lwt_list.iter_s
    (fun c ->
       Krobot_bus.send bus (Unix.gettimeofday (),
                            CAN (Info,Krobot_message.encode c)))
    msg

let () = Lwt_main.run t
