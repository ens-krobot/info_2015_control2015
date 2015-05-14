open Krobot_geom;;
open Krobot_mover_control;;

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

let rec loop state =
  Printf.printf "goto 1\n%!";
  lwt state = retry_goto ~state ~destination:{ x = 0.8; y = 1. } in
  Printf.printf "goto 2\n%!";
  lwt state = retry_goto ~state ~destination:{ x = 2.2; y = 1. } in
  Printf.printf "restart\n%!";
  loop state
;;

let demo_loop () =
  lwt state = make () in
  loop state

let command_goto args =
  match args with
  | [|x; y|] ->
    let x = float_of_string x in
    let y = float_of_string y in
    lwt state = make () in
    lwt state = retry_goto ~state ~destination:{ x; y } in
    Lwt.return ()
  | _ ->
    Printf.printf "goto: wrong number of arguments: %i, expected 2\n%!"
      (Array.length args);
    exit 1

let command_turn args =
  match args with
  | [||] ->
    Printf.printf "goto: wrong number of arguments: %i, expected at least 1\n%!"
      (Array.length args);
    exit 1
  | _ ->
    let args = Array.to_list (Array.map float_of_string args) in
    Lwt_list.iter_s
      (fun theta ->
         lwt state = make () in
         lwt state = retry_turn ~state ~orientation:theta in
         Lwt.return ())
      args

(****** Goto closest stand ***********)

let rec list_last = function
  | [t] -> Some t
  | h :: t -> list_last t
  | [] -> None

let goto_closest_stand rest =
  let wait_time =
    match rest with
    | [|t|] -> float_of_string t
    | _ -> 0.
  in
  lwt state = make () in
  lwt () = Lwt_unix.sleep wait_time in
  lwt state = update ~state in
  Printf.printf "state ready\n%!";
  match choose_close_stand state with
  | None ->
    Printf.printf "no reachable stand\n%!";
    Lwt.return ()
  | Some (_closest_stand, path) ->
    match list_last path with
    | None ->
      Printf.printf "no end of path for stand\n%!";
      Lwt.return ()
    | Some last_position ->
      Printf.printf "destination %f %f\n%!" last_position.x last_position.y;
      lwt state = retry_goto ~state ~destination:last_position in
      Lwt.return ()

(****** Test homologatoin ************)

type team = Krobot_bus.team

let target = { x = 1.1 ; y = Krobot_config.world_height -. 1.75 }
let dir = (3. /. 4.) *. pi
let distance = Krobot_config.robot_radius +. 0.2
let push_distance = 0.33

let yellow_out_of_start_zone =
  { x = 0.5; y = 1. }

let yellow_stuff_to_push = translate target (vector_of_polar ~angle:dir ~norm:distance)

let yellow_position_to_push =
  translate
    yellow_stuff_to_push
    (vector_of_polar ~angle:dir ~norm:(-.push_distance))

let mirror v = { x = Krobot_config.world_width -. v.x; y = v.y }

let out_of_start_zone_pos : Krobot_bus.team -> _ = function
  | Yellow -> yellow_out_of_start_zone
  | Green -> mirror yellow_out_of_start_zone
let first_position : Krobot_bus.team -> _  = function
  | Yellow -> yellow_stuff_to_push
  | Green -> mirror yellow_stuff_to_push
let push_position : Krobot_bus.team -> _  = function
  | Yellow -> yellow_position_to_push
  | Green -> mirror yellow_position_to_push

let yellow_push_orientation = pi /. 4.
let flip angle = 2. *. pi -. angle

let push_orientation : Krobot_bus.team -> _  = function
  | Yellow -> yellow_push_orientation
  | Green -> flip yellow_push_orientation

let out_of_start_zone state team =
  retry_move ~state ~destination:(out_of_start_zone_pos team) ~ignore_fixed_obstacles:true

let do_homologation_run state team =
  lwt state = out_of_start_zone state team in
  lwt state = retry_goto ~state ~destination:(first_position team) in
  lwt state = retry_turn ~state ~orientation:(push_orientation team) in
  lwt state = retry_move ~state ~destination:(push_position team) ~ignore_fixed_obstacles:false in
  Lwt.return ()

let match_end state =
  Printf.printf "Match end\n%!";
  lwt () = stop state in
  Printf.printf "Stop\n%!";
  lwt () = lcd_message ~state ~line:4 ~text:"Match end !" in
  lwt () = Lwt_unix.sleep 0.1 in
  Printf.printf "die !\n%!";
  exit 0

let do_homologation () =
  lwt state = make () in
  try_lwt
    lwt state = wait_for_jack ~state ~jack_state:In in
    lwt state = reset_odometry ~state in
    lwt state = wait_for_jack ~state ~jack_state:Out in
    lwt state = reset_odometry ~state in
    do_homologation_run state (get_team state)
  with Match_end_exn ->
    match_end state

let direct_homologation () =
  Printf.printf "go ?\n%!";
  lwt state = make () in
  Printf.printf "state ready\n%!";
  try
    lwt state = reset_odometry ~state in
    Printf.printf "start\n%!";
    do_homologation_run state (get_team state)
  with Match_end_exn ->
    match_end state

let homologation args =
  lwt state = make () in
  match args with
  | [|"yellow"|] -> do_homologation_run state Yellow
  | [|"green"|] -> do_homologation_run state Green
  | _ ->
    Printf.printf "homologation: wrong number of arguments: %i, expected 1 (green or yellow)\n%!"
      (Array.length args);
    exit 1

(* GOTO clap *)

let do_given_clap_run state clap =
  let open Krobot_config in
  let side = if clap.left_side then
      Krobot_world_update.Left
    else
      Krobot_world_update.Right
  in
  Printf.printf "Approach clap\n%!";
  lwt state = retry_goto ~state
      ~destination:{ x = clap.approach_pos; y = clap_y } in
  Printf.printf "turn to position\n%!";
  lwt state = retry_turn ~state ~orientation:clap.dir in
  Printf.printf "arm out\n%!";
  lwt (state, ()) = Krobot_mover_control.clap ~state ~side ~status:Clap_out in
  Printf.printf "push clap\n%!";
  lwt state = retry_move ~state
      ~destination:{ x = clap.after_pos; y = clap_y }
      ~ignore_fixed_obstacles:false in
  Printf.printf "arm in\n%!";
  lwt (state, ()) = Krobot_mover_control.clap ~state ~side ~status:Clap_in in
  Printf.printf "escape a bit\n%!";
  lwt state = retry_move ~state
      ~destination:{ x = clap.after_pos; y = clap_y +. 0.1 }
      ~ignore_fixed_obstacles:false in
  Printf.printf "Done !\n%!";
  Lwt.return state

type clap_num = Clap2 | Clap3

let team_clap team clap =
  let open Krobot_config in
  match team, clap with
  | Krobot_bus.Yellow, Clap2 -> yellow_clap_2
  | Yellow, Clap3 -> yellow_clap_3
  | Green, Clap2 -> green_clap_2
  | Green, Clap3 -> green_clap_3

let do_clap_run state team clapn =
  let clap = team_clap team clapn in
  do_given_clap_run state clap

let direct_clap () =
  Printf.printf "direct clap strategy\n%!";
  lwt state = make () in
  lwt () = Lwt_unix.sleep 1. in
  lwt state = update ~state in
  let team = get_team state in
  let () = match team with
    | Green -> Printf.printf "Green\n%!"
    | Yellow -> Printf.printf "Yellow\n%!"
  in
  Printf.printf "state ready\n%!";
  try
    lwt state = reset_odometry ~state in
    Printf.printf "exit home\n%!";
    lwt state = out_of_start_zone state team in
    lwt state = do_clap_run state team Clap2 in
    lwt state = do_clap_run state team Clap3 in
    lwt state = retry_goto ~state ~destination:(out_of_start_zone_pos team) in
    Lwt.return ()
  with Match_end_exn ->
    match_end state

let yellow_clap_1, yellow_clap_2, yellow_clap_3 = Krobot_config.yellow_clap_positions
let goto_first_yellow_clap_position =
  { x = yellow_clap_2 -. 0.1; y = Krobot_config.robot_radius +. 0.03 }
let goto_first_yellow_clap_position_after =
  { x = yellow_clap_2 +. 0.05; y = Krobot_config.robot_radius +. 0.03 }

let do_clap_hand_run state (team:Krobot_bus.team) =
  let swap pos = match team with
    | Yellow -> pos
    | Green -> mirror pos
  in
  let flip dir = match team with
    | Yellow -> dir
    | Green -> flip dir
  in
  let side = match team with
    | Yellow -> Krobot_world_update.Right
    | Green -> Krobot_world_update.Left
  in
  lwt state = out_of_start_zone state team in
  lwt state = retry_goto ~state ~destination:(swap goto_first_yellow_clap_position) in
  Printf.printf "turn !\n%!";
  lwt state = retry_turn ~state ~orientation:(flip (pi/.2.)) in
  Printf.printf "clap before !\n%!";
  lwt (state, ()) = clap ~state ~side ~status:Clap_out in
  Printf.printf "clap 1 !\n%!";
  lwt state = retry_move ~state ~destination:(swap goto_first_yellow_clap_position_after)
      ~ignore_fixed_obstacles:false in
  Printf.printf "clap 2 !\n%!";
  lwt (state, ()) = clap ~state ~side ~status:Clap_in in
  Printf.printf "clap 3 !\n%!";
  Lwt.return ()

let direct_clap_hand () =
  Printf.printf "clap !\n%!";
  lwt state = make () in
  lwt () = Lwt_unix.sleep 1. in
  lwt state = update ~state in
  let () = match get_team state with
    | Green -> Printf.printf "Green\n%!"
    | Yellow -> Printf.printf "Yellow\n%!"
  in
  Printf.printf "state ready\n%!";
  try
    lwt state = reset_odometry ~state in
    Printf.printf "start\n%!";
    do_clap_hand_run state (get_team state)
  with Match_end_exn ->
    match_end state

let do_clap_stuff state (team:Krobot_bus.team) =
  Printf.printf "clap before !\n%!";
  lwt (state, ()) = clap ~state ~side:Krobot_world_update.Left ~status:Clap_out in
  Printf.printf "clap 1 !\n%!";
  lwt (state, ()) = clap ~state ~side:Krobot_world_update.Left ~status:Clap_in in
  Printf.printf "clap 2 !\n%!";
  lwt (state, ()) = clap ~state ~side:Krobot_world_update.Right ~status:Clap_out in
  Printf.printf "clap 3 !\n%!";
  lwt (state, ()) = clap ~state ~side:Krobot_world_update.Left ~status:Clap_out in
  Printf.printf "clap 4 !\n%!";
  lwt (state, ()) = clap ~state ~side:Krobot_world_update.Left ~status:Clap_in in
  Printf.printf "clap 5 !\n%!";
  lwt (state, ()) = clap ~state ~side:Krobot_world_update.Right ~status:Clap_in in
  Printf.printf "clap 6 !\n%!";
  Lwt.return ()

let stuff_clap () =
  Printf.printf "clap !\n%!";
  lwt state = make () in
  Printf.printf "state ready\n%!";
  do_clap_stuff state (get_team state)

(****** Command_line ************)

let () = if Array.length Sys.argv < 2 then Printf.printf "Missing argument\n%!"
lwt () =
  let rest = Array.sub Sys.argv 2 (Array.length Sys.argv - 2) in
  match Sys.argv.(1) with
  | "stand" ->
    goto_closest_stand rest
  | "goto" ->
    command_goto rest
  | "turn" ->
    command_turn rest
  | "demo" ->
    demo_loop ()
  | "run_homologation" ->
    homologation rest
  | "homologation" ->
    do_homologation ()
  | "test" ->
    direct_homologation ()
  | "clap" ->
    direct_clap ()
  | "claphand" ->
    direct_clap_hand ()
  | "sclap" ->
    stuff_clap ()
  | cmd ->
    Printf.printf "unknown command %s\n%!" cmd;
    exit 1
