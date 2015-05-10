(*
 * krobot_lcd.ml
 * ------------------
 * Copyright : (c) 2015, Pierre Chambart <chambart@crans.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Krobot_message

let line line_number = function
  | None -> []
  | Some s ->
    let b = Bytes.make 20 ' ' in
    Bytes.blit_string s 0 b 0 (min 20 (String.length s));
    [LCD_message_part (3*(line_number-1) + 0, Bytes.sub_string b 0 7);
     LCD_message_part (3*(line_number-1) + 1, Bytes.sub_string b 7 7);
     LCD_message_part (3*(line_number-1) + 2, Bytes.sub_string b 14 6);
     LCD_refresh_line line_number]

let display ?l1 ?l2 ?l3 ?l4 () =
  let l1 = line 1 l1 in
  let l2 = line 2 l2 in
  let l3 = line 3 l3 in
  let l4 = line 4 l4 in
  l1 @ l2 @ l3 @ l4

let sub s p l =
  if p >= String.length s
  then ""
  else String.sub s p (min (String.length s - p) l)

let text s =
  display
    ~l1:(sub s 0 20)
    ~l2:(sub s 20 20)
    ~l3:(sub s 40 20)
    ~l4:(sub s 60 20)
    ()

let send_text bus s =
  Lwt_list.iter_s (fun c ->
    lwt () = Krobot_message.send bus (Unix.gettimeofday (), c) in
    Lwt_unix.sleep 0.02)
    (text s)

let send_line bus l s =
  Lwt_list.iter_s (fun c ->
    lwt () = Krobot_message.send bus (Unix.gettimeofday (), c) in
    Lwt_unix.sleep 0.010)
    (line l (Some s))

let clear bus =
  Krobot_message.send bus (Unix.gettimeofday (), LCD_clear)
