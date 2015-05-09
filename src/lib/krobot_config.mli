(*
 * krobot_config.mli
 * -----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** Parameters *)

val world_width : float
  (** The width of the board. *)

val world_height : float
  (** The height of the board. *)

val green_fixed_beacons : Krobot_geom.vertice list
  (** Positions of the fixed beacons for the green team *)

val yellow_fixed_beacons : Krobot_geom.vertice list
  (** Positions of the fixed beacons for the yellow team *)

(* val robot_length : float *)
(* val robot_width : float *)

val safety_margin : float
(** The distance between the robot and the border/an object must
    always be greater than this vlaue. *)

val wheels_diameter : float
(** The diameter of the wheels. *)

val wheels_distance : float
  (** The distance between the two wheels. *)

val wheels_position : float
  (** The distance between the axe of the wheels and the back of the
      robot. *)

val robot_radius : float
  (** distance between the point between the wheel and the farthest point *)

val rotary_beacon_index_pos : float
  (** The angle of the rotary beacon index angle with respect to the
      robot's front *)

val beacon_radius : float
  (** Radius of the ennemy. *)

val fire_radius : float
  (** Bounding box radius of fires. *)

val green_initial_position : Krobot_geom.vertice * float
  (** position and angle of the robot as green *)

val yellow_initial_position : Krobot_geom.vertice * float
  (** position and angle of the robot as yellow *)


val fixed_obstacles : Krobot_geom.rect_obj list

val test_obstacles : Krobot_geom.rect_obj list

(* val initial_fires : (Krobot_geom.vertice * float) list *)

(* val initial_torches : Krobot_geom.vertice list *)

val urg_up_position : Krobot_geom.vertice
val urg_down_position : Krobot_geom.vertice

val urg_min_distance : float
  (** distance below which urg points are discarded *)

val urg_up_id : string
val urg_down_id : string

val urg_up_angles : float array
val urg_down_angles : float array
(* angles (in radiant) for each index of urg messages *)

val urg_up_filter : bool array
val urg_down_filter : bool array
(* filterd points *)


(** Motor limits *)

(* on normal moves *)

type motor_limits = {
  v_lin_max : float; (* m/s *)
  v_rot_max : float; (* rad/s *)
  a_lin_max : float; (* m/s^2 *)
  a_rot_max : float; (* rad/s^2 *)
  torque_limit : int; (* in [0, 3600] arbitrary unit *)
}

val normal_limits : motor_limits

val constrained_limits : motor_limits
(* Limits when the robot is moving in a situation where
   collisions are expected *)

val extract_number_of_pointneeded_for_obstacle : int
(** number of points needed for the extractor to consider an obstacle *)
