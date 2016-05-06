open Krobot_geom;;
open Krobot_mover_control;;
open Krobot_config;;

let log fmt =
  Printf.ksprintf (fun s ->
    Printf.printf "%s\n%!" s;
    Lwt_log.ign_info s)
    fmt

(***** Utils ******)

let retry_goto ~state ~destination =
  let rec aux ~state =
    Format.printf "Start goto %a@." Krobot_date.print (Krobot_date.now ());
    match_lwt goto ~state ~destination with
    | state, Goto_success ->
      Format.printf "Cool %a@." Krobot_date.print (Krobot_date.now ());
      Lwt.return state
    | state, Goto_failure ->
      Format.printf "Fail %a@." Krobot_date.print (Krobot_date.now ());
      lwt () = Lwt_unix.sleep 0.1 in
      aux ~state
    | state, Goto_unreachable ->
      Format.printf "Unreachable %a@." Krobot_date.print (Krobot_date.now ());
      lwt () = Lwt_unix.sleep 0.1 in
      aux ~state
      (* Lwt.fail (Invalid_argument "unreachable") *)
  in
  Format.printf "try go %a@." Krobot_date.print (Krobot_date.now ());
  aux ~state;;

let retry_turn ~state ~orientation =
  let rec aux ~state =
    Format.printf "Start turn %a@." Krobot_date.print (Krobot_date.now ());
    match_lwt turn ~state ~orientation with
    | state, Turn_success ->
      Format.printf "Cool %a@." Krobot_date.print (Krobot_date.now ());
      Lwt.return state
    | state, Turn_failure ->
      Format.printf "Fail %a@." Krobot_date.print (Krobot_date.now ());
      lwt () = Lwt_unix.sleep 0.1 in
      aux ~state
  in
  Format.printf "try turn %a@." Krobot_date.print (Krobot_date.now ());
  aux ~state;;

let retry_ignore_all_turn ~state ~orientation =
  let rec aux ~state =
    Format.printf "Start ignore_all_turn %a@." Krobot_date.print (Krobot_date.now ());
    match_lwt ignore_all_turn ~state ~orientation with
    | state, Turn_success ->
      Format.printf "Cool %a@." Krobot_date.print (Krobot_date.now ());
      Lwt.return state
    | state, Turn_failure ->
      Format.printf "Fail %a@." Krobot_date.print (Krobot_date.now ());
      lwt () = Lwt_unix.sleep 0.1 in
      aux ~state
  in
  Format.printf "try turn %a@." Krobot_date.print (Krobot_date.now ());
  aux ~state;;

let retry_move ~state ~destination ~ignore_fixed_obstacles =
  let rec aux ~state =
    Format.printf "Start move %a@." Krobot_date.print (Krobot_date.now ());
    match_lwt move ~ignore_fixed_obstacles ~state ~destination with
    | state, Move_success ->
      Format.printf "Cool %a@." Krobot_date.print (Krobot_date.now ());
      Lwt.return state
    | state, Move_failure ->
      Format.printf "Fail %a@." Krobot_date.print (Krobot_date.now ());
      lwt () = Lwt_unix.sleep 0.1 in
      aux ~state
  in
  Format.printf "try move %a@." Krobot_date.print (Krobot_date.now ());
  aux ~state;;

let retry_ignore_all_move ~state ~destination =
  let rec aux ~state =
    Format.printf "Start ignore_all_move %a@." Krobot_date.print (Krobot_date.now ());
    match_lwt ignore_all_move ~state ~destination with
    | state, Move_success ->
      Format.printf "Cool %a@." Krobot_date.print (Krobot_date.now ());
      Lwt.return state
    | state, Move_failure ->
      Format.printf "Fail %a@." Krobot_date.print (Krobot_date.now ());
      lwt () = Lwt_unix.sleep 0.1 in
      aux ~state
  in
  Format.printf "try move %a@." Krobot_date.print (Krobot_date.now ());
  aux ~state;;

(***** Postion configuration ******)

let mirror v = { x = Krobot_config.world_width -. v.x; y = v.y }
let flip a = Krobot_geom.(angle_pi_minus_pi (2. *. pi -. a))

let centre_carre_a_pousser =
  { y = 0.908;
    x = 0.638 }

let purple_start_out_dir = pi

let beach_hut_1_x = 0.3
let beach_hut_2_x = 0.6

let y_before_push = 1.65
let y_after_push = Krobot_config.world_height -. (Krobot_config.robot_width /. 2. -. 0.02)

let purple_out_of_zone =
  let { x; y }, _th = Krobot_config.purple_initial_position in
  { x = x +. 0.2; y = y +. 0.15 }

let purple_in_front_of_wall_1 =
  { x = beach_hut_1_x; y = y_before_push }

let purple_push_wall_1 =
  { x = beach_hut_1_x; y = y_after_push }

let purple_in_front_of_wall_2 =
  { x = beach_hut_2_x; y = y_before_push }

let purple_push_wall_2 =
  { x = beach_hut_2_x; y = y_after_push }

let purple_in_front_of_wall_dir =
  pi /. 2.

let purple_end_dir = -. (pi /. 2.)

let quelquepart =
  { x = 0.7; y = 1. }

(***** Actions ******)

let actions state team =
  let mirror v = match team with
    | Krobot_bus.Purple -> v
    | Green -> mirror v
  in
  let flip a = match team with
    | Krobot_bus.Purple -> a
    | Green -> flip a
  in
  log "Leave start zone";
  lwt state = retry_move ~state ~destination:(mirror purple_out_of_zone) ~ignore_fixed_obstacles:true in
  log "Face move";
  lwt state = retry_turn ~state ~orientation:(flip purple_start_out_dir) in
  lwt state = retry_move ~state ~destination:(mirror purple_in_front_of_wall_1) ~ignore_fixed_obstacles:true in
  lwt state = retry_turn ~state ~orientation:(flip purple_in_front_of_wall_dir) in
  lwt state = retry_move ~state ~destination:(mirror purple_push_wall_1) ~ignore_fixed_obstacles:true in
  lwt state = retry_move ~state ~destination:(mirror purple_in_front_of_wall_1) ~ignore_fixed_obstacles:true in
  lwt state = retry_move ~state ~destination:(mirror purple_in_front_of_wall_2) ~ignore_fixed_obstacles:true in
  lwt state = retry_move ~state ~destination:(mirror purple_push_wall_2) ~ignore_fixed_obstacles:true in
  lwt state = retry_move ~state ~destination:(mirror purple_in_front_of_wall_2) ~ignore_fixed_obstacles:true in
  log "Finished";
  lwt state = retry_turn ~state ~orientation:(flip purple_end_dir) in
  lwt _state = retry_goto ~state ~destination:(mirror quelquepart) in
  log "All actions done";
  Lwt.return ()

(********************************)

let match_end state =
  log "Match end";
  lwt () = stop state in
  log "Stop";
  lwt () = lcd_message ~state ~line:4 ~text:"Match end !" in
  lwt () = Lwt_unix.sleep 0.1 in
  log "die !";
  Printf.printf "die !\n%!";
  exit 0

let init () =
  log "Start";
  lwt state = make () in
  log "Init done";
  lwt state = wait_for_jack ~state ~jack_state:In in
  log "Jack In";
  lwt state = reset_odometry ~state in
  log "Odometry reset";
  lwt state = wait_for_jack ~state ~jack_state:Out in
  log "Jack-off: Start match";
  lwt state = reset_odometry ~state in
  let team = get_team state in
  let team_str = match team with
    | Green -> "Green"
    | Purple -> "Purple"
  in
  log "Team set: %s" team_str;
  Lwt.return (state, team)

let start () =
  lwt (state, team) = init () in
  let () = log "Start moves" in
  try_lwt
    actions state team
  with Match_end_exn ->
    match_end state

let immediate_init team =
  log "Imm Start";
  lwt state = make () in
  log "Init done";
  lwt state = reset_team_odometry ~state ~team in
  log "Odometry set";
  let team_str = match team with
    | Green -> "Green"
    | Purple -> "Purple"
  in
  log "Team set: %s" team_str;
  Lwt.return state

let immediate_start team =
  lwt state = immediate_init team in
  let () = log "Start moves" in
  try_lwt
    actions state team
  with Match_end_exn ->
    match_end state

(****** Command_line ************)

lwt () =
  if Array.length Sys.argv > 1
  then
    let team = match Sys.argv.(1) with
      | "purple" -> Krobot_bus.Purple
      | _ -> Krobot_bus.Green
    in
    immediate_start team
  else start ()
