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

let t =
  (* Open the krobot bus. *)
  Printf.printf "input text is: %s\n%!" text;
  lwt bus = Krobot_bus.get () in
  lwt () = Krobot_lcd.clear bus in
  lwt () = Lwt_unix.sleep 0.05 in
  Krobot_lcd.send_text bus text

let () = Lwt_main.run t
