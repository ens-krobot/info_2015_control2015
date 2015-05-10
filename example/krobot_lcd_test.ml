#use "topfind";;
#camlp4o;;
#require "lwt.syntax";;
#require "krobot";;
open Krobot_bus;;
open Krobot_message;;
open Krobot_config;;

(* le hub et driver doivent être lancés *)
let bus = Lwt_unix.run (Krobot_bus.get ())
let send m = Lwt_unix.run (Krobot_message.send bus (Unix.gettimeofday (), m))
let sends m =
  let t = Lwt_list.iter_s (fun m -> Krobot_message.send bus (Unix.gettimeofday (), m)) m in
  Lwt_unix.run t
let sendt m = Krobot_message.send bus (Unix.gettimeofday (), m)

let lcd_clear () =
  Krobot_lcd.clear bus

let send_line num text =
  Krobot_lcd.send_line bus num text

let send_text text =
  Krobot_lcd.send_text bus text

let do_stuff () =
  lwt () = lcd_clear () in
  lwt () = Lwt_unix.sleep 0.1 in
  lwt () = send_text (  "1234567890abcdefghij"
                      ^ "klmnopqrstuvwxyzABCD"
                      ^ "EFGHIJKLMNOPQRSTUVWX"
                      ^ "YZ?!([{}]),;:-_%$*+/") in
  Lwt.return ()

let () = Lwt_unix.run (do_stuff ())

