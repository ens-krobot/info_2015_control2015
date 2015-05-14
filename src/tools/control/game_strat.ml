open Krobot_geom;;
open Krobot_mover_control;;

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

(***** Clap part ******************)

let do_given_clap_run state clap ~goto_approach =
  let open Krobot_config in
  let side = if clap.left_side then
      Krobot_world_update.Left
    else
      Krobot_world_update.Right
  in
  log "Approach clap";
  lwt state =
    let destination = { x = clap.approach_pos; y = clap_y } in
    if goto_approach then
      retry_goto ~state ~destination
    else
      retry_ignore_all_move ~state ~destination
  in
  log "turn to position";
  lwt state = retry_turn ~state ~orientation:clap.dir in
  log "arm out";
  lwt (state, ()) = Krobot_mover_control.clap ~state ~side ~status:Clap_out in
  log "push clap";
  lwt state = retry_move ~state
      ~destination:{ x = clap.after_pos; y = clap_y }
      ~ignore_fixed_obstacles:false in
  log "arm in";
  lwt (state, ()) = Krobot_mover_control.clap ~state ~side ~status:Clap_in in
  log "escape a bit";
  lwt state = retry_move ~state
      ~destination:{ x = clap.after_pos; y = clap_y +. 0.1 }
      ~ignore_fixed_obstacles:false in
  log "Clap done !";
  Lwt.return state

type clap_num = Clap1 | Clap2 | Clap3

let team_clap team clap =
  let open Krobot_config in
  match team, clap with
  | Krobot_bus.Yellow, Clap1 -> yellow_clap_1
  | Yellow, Clap2 -> yellow_clap_2
  | Yellow, Clap3 -> yellow_clap_3
  | Green, Clap1 -> green_clap_1
  | Green, Clap2 -> green_clap_2
  | Green, Clap3 -> green_clap_3

let do_clap_run state team clapn =
  let clap = team_clap team clapn in
  do_given_clap_run state clap

(***** Postion configuration ******)

let mirror v = { x = Krobot_config.world_width -. v.x; y = v.y }
let flip a = Krobot_geom.(angle_pi_minus_pi (2. *. pi -. a))

let yellow_out_of_start_zone =
  { x = 0.5; y = 1. }

let yellow_start_first_push =
  { x = 0.67; y = 0.80 }

let yellow_aim_first_move =
  { x = 1.2; y = 0.1 }

let yellow_start_first_push_dir =
  Krobot_geom.(angle (vector yellow_start_first_push yellow_aim_first_move))
  +. (7. *. pi /. 6.)
  (* (pi /. 2.) *)

let yellow_end_first_push =
  let open Krobot_geom in
  let v = normalize (vector yellow_aim_first_move yellow_start_first_push) in
  let approach_distance = Krobot_config.robot_radius +. 0.07 in
  let trans_vect = v *| approach_distance in
  translate yellow_aim_first_move trans_vect

let yellow_back_after_first_push =
  let open Krobot_geom in
  let v = normalize (vector yellow_aim_first_move yellow_start_first_push) in
  let approach_distance = Krobot_config.robot_radius +. 0.3 in
  let trans_vect = v *| approach_distance in
  translate yellow_aim_first_move trans_vect

let yellow_clap1_approach =
  { x = 0.54; y = 0.3 }

let yellow_closer_from_border =
  { x = 0.54; y = 0.23 }

let yellow_corner_push =
  { x = 0.54; y = 0.23 }

let yellow_last_stand =
  { x = 1.634; y = 0.628 }

let yellow_before_last_push =
  { x = 1.; y = 0.65 }

let yellow_last_push_dir = pi /. 2.

let yellow_last_push_middle =
  { x = 2.; y = 0.65 }

let yellow_last_push_end =
  { x = 2.5; y = 0.55 }

let yellow_last_push_back =
  { x = 2.3; y = 0.55 }

let yellow_back_home =
  { x = 0.9; y = 0.9 }

(***** Actions ******)

let actions state team =
  let mirror v = match team with
    | Krobot_bus.Yellow -> v
    | Green -> mirror v
  in
  let flip a = match team with
    | Krobot_bus.Yellow -> a
    | Green -> flip a
  in
  let clap1 state team =
    lwt state = retry_goto ~state ~destination:(mirror yellow_clap1_approach) in
    lwt state = retry_ignore_all_move ~state ~destination:(mirror yellow_closer_from_border) in
    lwt state = do_clap_run state team Clap1 ~goto_approach:false in
    Lwt.return state
  in
  log "Leave start zone";
  lwt state = retry_move ~state ~destination:(mirror yellow_out_of_start_zone) ~ignore_fixed_obstacles:true in
  lwt state = retry_goto ~state ~destination:(mirror yellow_start_first_push) in
  lwt state = retry_turn ~state ~orientation:(flip yellow_start_first_push_dir) in
  lwt state = retry_move ~state ~destination:(mirror yellow_end_first_push) ~ignore_fixed_obstacles:false in
  lwt state = retry_move ~state ~destination:(mirror yellow_back_after_first_push) ~ignore_fixed_obstacles:false in
  lwt state = do_clap_run state team Clap2 ~goto_approach:true in
  (* lwt state = clap1 state team in *)
  lwt state = retry_goto ~state ~destination:(mirror yellow_before_last_push) in
  lwt state = retry_turn ~state ~orientation:(flip yellow_last_push_dir) in
  lwt state = retry_move ~state ~destination:(mirror yellow_last_push_middle) ~ignore_fixed_obstacles:false in
  lwt state = retry_move ~state ~destination:(mirror yellow_last_push_end) ~ignore_fixed_obstacles:false in
  lwt state = retry_move ~state ~destination:(mirror yellow_last_push_back) ~ignore_fixed_obstacles:false in
  lwt state = do_clap_run state team Clap3 ~goto_approach:true in
  lwt state = retry_goto ~state ~destination:(mirror yellow_back_home) in
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
    | Yellow -> "Yellow"
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
    | Yellow -> "Yellow"
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
      | "yellow" -> Krobot_bus.Yellow
      | _ -> Krobot_bus.Green
    in
    immediate_start team
  else start ()
