(*
 * krobot_message.ml
 * -----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Krobot_can
open Printf
open Lwt
open Lwt_react

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type direction = Forward | Backward

type simulation_mode = Sim_no | Sim_normal | Sim_HIL

type t =
  | Battery1_voltages of float * float * float * float
  | Battery2_voltages of float * float * float * float
  | Beacon_position of float * float * float * float
  | Beacon_lowlevel_position of float * float * int
  | Beacon_angles of float * float * float * float
  | Beacon_widths of float * float * float * float
  | Switch1_status of bool * bool * bool * bool * bool * bool * bool * bool
  | Switch2_status of bool * bool * bool * bool * bool * bool * bool * bool
  | Switch_request of int * bool
  | Adc1_values of int * int * int * int
  | Adc2_values of int * int * int * int
  | Ax12_State of int * int * int * int
  | Ax12_Request_State of int
  | Ax12_Goto of int * int * int
  | Ax12_Goto_Reg of int * int * int
  | Ax12_Action of int
  | Ax12_Status_Return_Level of int * int
  | Ax12_Set_Torque_Enable of int * bool
  | Encoder_position_direction_1_2 of int * direction * int * direction
  | Encoder_position_direction_3_4 of int * direction * int * direction
  | Encoder_position_speed_2 of float * float
  | Encoder_position_speed_3 of float * float
  | Encoder_position_speed_4 of float * float
  | Controller_activation of int * bool
  | Drive_activation of bool
  | Torque_limit of int * int
  | Drive_torque_limit of int
  | Control_error of int * int
  | Motor_status of bool * bool * bool * bool
  | Motor_move of float * float * float
  | Motor_move_x of float * float * float
  | Motor_move_y of float * float * float
  | Motor_turn of float * float * float
  | Lock_target of float * float * float
  | Unlock_target
  | Lock_status of float * bool
  | Motor_bezier of float * float * float * float * float * float
  | Motor_command of int * int
  | Motor_activation of int * bool
  | Motor_stop of float * float
  | Motor_bezier_limits of float * float * float * float
  | Odometry of float * float * float
  | Odometry_indep of float * float * float
  | Odometry_ghost of float * float * float * int * bool
  | Set_odometry of float * float * float
  | Set_odometry_indep of float * float * float
  | Set_simulation_mode of simulation_mode
  | Elevator_command of float * float
  | Pump_command of int * int
  | Elevator_encoders of int * direction * int * direction
  | Pump_state of int * int
  | Effector_status of bool * bool * bool * bool
  | Elevator_positions of float * float
  | Homing_status of bool * bool
  | Homing_command of float * float
  | Req_motor_status
  | Motor_omni_limits of float * float * float * float
  | Motor_omni_goto of float * float * float
  | LCD_clear
  | LCD_backlight of bool
  | LCD_refresh_line of int
  | LCD_message_part of int * string
  | Unknown of Krobot_can.frame

(* +-----------------------------------------------------------------+
   | Message --> string                                              |
   +-----------------------------------------------------------------+ *)

let string_of_direction = function
  | Forward -> "Forward"
  | Backward -> "Backward"

let string_of_simulation_mode = function
  | Sim_no -> "Normal mode"
  | Sim_normal -> "Simulation mode"
  | Sim_HIL -> "HIL mode"

let to_string = function
  | Battery1_voltages(elem1, elem2, elem3, elem4) ->
      sprintf
        "Battery1_voltages(%f, %f, %f, %f)"
        elem1
        elem2
        elem3
        elem4
  | Battery2_voltages(elem1, elem2, elem3, elem4) ->
      sprintf
        "Battery2_voltages(%f, %f, %f, %f)"
        elem1
        elem2
        elem3
        elem4
  | Beacon_position(angle1, angle2, distance1, distance2) ->
      sprintf
        "Beacon_position(%f, %f, %f, %f)"
        angle1
        angle2
        distance1
        distance2
  | Beacon_lowlevel_position(angle, width, period) ->
      sprintf
        "Beacon_lowlevel_position(%f, %f, %d)"
        angle
        width
        period
  | Beacon_angles(a1, a2, a3, a4) ->
      sprintf
        "Beacon_angles(%f, %f, %f, %f)"
        a1 a2 a3 a4
  | Beacon_widths(w1, w2, w3, w4) ->
      sprintf
        "Beacon_widths(%f, %f, %f, %f)"
        w1 w2 w3 w4
  | Switch1_status(s1, s2, s3, s4, s5, s6, s7, s8) ->
      sprintf
        "Switch1_status(%B, %B, %B, %B, %B, %B, %B, %B)"
        s1 s2 s3 s4 s5 s6 s7 s8
  | Switch2_status(s1, s2, s3, s4, s5, s6, s7, s8) ->
      sprintf
        "Switch2_status(%B, %B, %B, %B, %B, %B, %B, %B)"
        s1 s2 s3 s4 s5 s6 s7 s8
  | Switch_request(switch, status) ->
      sprintf
        "Switch_request(%d, %s)"
        switch
        (if status then "ON" else "OFF")
  | Adc1_values(v1, v2, v3, v4) ->
      sprintf
        "Adc1_values(%d, %d, %d, %d)"
        v1 v2 v3 v4
  | Adc2_values(v1, v2, v3, v4) ->
      sprintf
        "Adc2_values(%d, %d, %d, %d)"
        v1 v2 v3 v4
  | Ax12_State(addr, position, speed, torque) ->
      sprintf
        "Ax12_State(%d, %d, %d, %d)"
        addr position speed torque
  | Ax12_Request_State(addr) ->
      sprintf
        "Ax12_Request_State(%d)"
        addr
  | Ax12_Goto(addr, position, speed) ->
      sprintf
        "Ax12_Goto(%d, %d, %d)"
        addr position speed
  | Ax12_Goto_Reg(addr, position, speed) ->
      sprintf
        "Ax12_Goto_Reg(%d, %d, %d)"
        addr position speed
  | Ax12_Action(addr) ->
      sprintf
        "Ax12_Action(%d)"
        addr
  | Ax12_Status_Return_Level(addr, level) ->
      sprintf
        "Ax12_Status_Return_Level(%d, %d)"
        addr level
  | Ax12_Set_Torque_Enable(addr, state) ->
      sprintf
        "Ax12_Set_Torque_Enable(%d, %B)"
        addr state
  | Encoder_position_direction_1_2(pos1, dir1, pos2, dir2) ->
      sprintf
        "Encoder_position_direction_1_2(%d, %s, %d, %s)"
        pos1 (string_of_direction dir1)
        pos2 (string_of_direction dir2)
  | Encoder_position_direction_3_4(pos3, dir3, pos4, dir4) ->
      sprintf
        "Encoder_position_direction_3_4(%d, %s, %d, %s)"
        pos3 (string_of_direction dir3)
        pos4 (string_of_direction dir4)
  | Encoder_position_speed_2(pos, speed) ->
      sprintf
        "Encoder_position_speed_2(%f, %f)"
        pos speed
  | Encoder_position_speed_3(pos, speed) ->
      sprintf
        "Encoder_position_speed_3(%f, %f)"
        pos speed
  | Encoder_position_speed_4(pos, speed) ->
      sprintf
        "Encoder_position_speed_4(%f, %f)"
        pos speed
  | Motor_status(m1, m2, m3, m4) ->
      sprintf
        "Motor_status(%B, %B, %B, %B)"
        m1 m2 m3 m4
  | Controller_activation(motor, activate) ->
      sprintf
        "Controller_activation(%d, %B)"
        motor activate
  | Drive_activation(activation) ->
      sprintf
        "Drive_activation(%B)"
        activation
  | Torque_limit(motor, value) ->
      sprintf
        "Torque_Limit(%d,%d)"
        motor value
  | Drive_torque_limit(value) ->
      sprintf
        "Drive_torque_limit(%d)"
        value
  | Control_error(e1, e2) ->
      sprintf
        "Control_error(%d, %d)"
        e1 e2
  | Motor_move(dist, speed, acc) ->
      sprintf
        "Motor_move(%f, %f, %f)"
        dist speed acc
  | Motor_move_x(dist, speed, acc) ->
      sprintf
        "Motor_move_x(%f, %f, %f)"
        dist speed acc
  | Motor_move_y(dist, speed, acc) ->
      sprintf
        "Motor_move_y(%f, %f, %f)"
        dist speed acc
  | Motor_turn(angle, speed, acc) ->
      sprintf
        "Motor_turn(%f, %f, %f)"
        angle speed acc
  | Lock_target(x, y, theta) ->
      sprintf
        "Lock_target(%f, %f, %f)"
        x y theta
  | Unlock_target ->
      "Unlock_target"
  | Lock_status(error, enabled) ->
      sprintf
        "Lock_status(%f, %B)"
        error enabled
  | Motor_bezier(x, y, d1, d2, theta, v) ->
      sprintf
        "Motor_bezier(%f, %f, %f, %f, %f, %f)"
        x y d1 d2 theta v
  | Motor_stop(lin_acc, rot_acc) ->
      sprintf
        "Motor_stop(%f, %f)"
        lin_acc rot_acc
  | Motor_bezier_limits(v_max, omega_max, a_tan_max, a_rad_max) ->
      sprintf
        "Motor_bezier_limits(%f, %f, %f, %f)"
        v_max omega_max a_tan_max a_rad_max
  | Motor_command(motor_id, speed) ->
      sprintf
        "Motor_command(%d, %d)"
        motor_id speed
  | Motor_activation (motor_id, active) ->
      sprintf
        "Motor_command (%d, %B)"
        motor_id active
  | Odometry(x, y, theta) ->
      sprintf
        "Odometry(%f, %f, %f)"
        x y theta
  | Odometry_ghost(x, y, theta, u, following) ->
      sprintf
        "Odometry_ghost(%f, %f, %f, %d, %B)"
        x y theta u following
  | Odometry_indep(x, y, theta) ->
      sprintf
        "Odometry_indep(%f, %f, %f)"
        x y theta
  | Set_odometry(x, y, theta) ->
      sprintf
        "Set_odometry(%f, %f, %f)"
        x y theta
  | Set_odometry_indep(x, y, theta) ->
      sprintf
        "Set_odometry_indep(%f, %f, %f)"
        x y theta
  | Set_simulation_mode m ->
      sprintf
        "Set_simulation_mode(%s)"
        (string_of_simulation_mode m)
  | Elevator_command(p1, p2) ->
      sprintf
        "Elevator_command(%f, %f)"
        p1 p2
  | Pump_command(i1, i2) ->
      sprintf
        "Pump_command(%d, %d)"
        i1 i2
  | Elevator_encoders(pos1, dir1, pos2, dir2) ->
      sprintf
        "Elevator_encoders(%d, %s, %d, %s)"
        pos1 (string_of_direction dir1)
        pos2 (string_of_direction dir2)
  | Pump_state(i1, i2) ->
      sprintf
        "Pump_state(%d, %d)"
        i1 i2
  | Effector_status(m1, m2, m3, m4) ->
      sprintf
        "Effector_status(%B, %B, %B, %B)"
        m1 m2 m3 m4
  | Elevator_positions(p1, p2) ->
      sprintf
        "Elevator_positions(%f, %f)"
        p1 p2
  | Homing_status(l1, l2) ->
      sprintf
        "Homing_status(%B, %B)"
        l1 l2
  | Homing_command(l1, l2) ->
      sprintf
        "Homing_command(%f, %f)"
        l1 l2
  | Req_motor_status ->
      "Req_motor_status"
  | Motor_omni_limits (v_lin_max, v_rot_max, a_lin_max, a_rot_max) ->
    Printf.sprintf "Motor_omni_limits(v_lin_max=%f,v_rot_max=%f,a_lin_max=%f,a_rot_max=%f)"
      v_lin_max v_rot_max a_lin_max a_rot_max
  | Motor_omni_goto (x_end, y_end, theta_end) ->
    Printf.sprintf "Motor_omni_goto(x_end=%f, y_end=%f, theta_end=%f)"
      x_end y_end theta_end
  | LCD_clear ->
    "LCD_clear"
  | LCD_refresh_line line ->
    Printf.sprintf "LCD_refresh_line %i" line
  | LCD_message_part (part, s) ->
    Printf.sprintf "LCD_message_part(%i,%s)" part s
  | LCD_backlight b ->
    Printf.sprintf "LCD_backlight %b" b

  | Unknown frame ->
      sprintf "Unknown%s" (Krobot_can.string_of_frame frame)

(* +-----------------------------------------------------------------+
   | Encoding                                                        |
   +-----------------------------------------------------------------+ *)

let pi = 4. *. atan 1.

external encode_bezier : int * int * int * int * int * int -> string = "krobot_message_encode_bezier"
external encode_omni_goto : int * int * int -> string = "krobot_message_encode_omni_goto"

let encode = function
  | Encoder_position_direction_1_2(pos1, dir1, pos2, dir2) ->
      let data = Bytes.create 6 in
      put_uint16 data 0 pos1;
      put_uint16 data 2 pos2;
      put_uint8 data 4 (match dir1 with Forward -> 0 | Backward -> 1);
      put_uint8 data 5 (match dir2 with Forward -> 0 | Backward -> 1);
      frame
        ~identifier:99
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Encoder_position_direction_3_4(pos3, dir3, pos4, dir4) ->
      let data = Bytes.create 6 in
      put_uint16 data 0 pos3;
      put_uint16 data 2 pos4;
      put_uint8 data 4 (match dir3 with Forward -> 0 | Backward -> 1);
      put_uint8 data 5 (match dir4 with Forward -> 0 | Backward -> 1);
      frame
        ~identifier:100
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Encoder_position_speed_2(pos, speed) ->
      let data = Bytes.create 8 in
      put_float32 data 0 pos;
      put_float32 data 4 speed;
      frame
        ~identifier:108
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Encoder_position_speed_3(pos, speed) ->
      let data = Bytes.create 8 in
      put_float32 data 0 pos;
      put_float32 data 4 speed;
      frame
        ~identifier:101
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Encoder_position_speed_4(pos, speed) ->
      let data = Bytes.create 8 in
      put_float32 data 0 pos;
      put_float32 data 4 speed;
      frame
        ~identifier:102
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Motor_status(m1, m2, m3, m4) ->
      let data = Bytes.create 1 in
      let x = 0 in
      let x = if m1 then x lor 1 else x in
      let x = if m2 then x lor 2 else x in
      let x = if m3 then x lor 4 else x in
      let x = if m4 then x lor 8 else x in
      put_uint8 data 0 x;
      frame
        ~identifier:103
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Controller_activation(motor, activation) ->
      let data = Bytes.create 2 in
      put_uint8 data 0 motor;
      put_uint8 data 1 (if activation then 1 else 0);
      frame
        ~identifier:210
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Drive_activation(activation) ->
      let data = Bytes.create 1 in
      put_uint8 data 0 (if activation then 1 else 0);
      frame
        ~identifier:211
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Torque_limit(motor, value) ->
      let data = Bytes.create 3 in
      put_uint8 data 0 motor;
      put_uint16 data 1 value;
      frame
        ~identifier:212
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Drive_torque_limit(value) ->
      let data = Bytes.create 2 in
      put_uint16 data 0 value;
      frame
        ~identifier:213
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Control_error(e1, e2) ->
      let data = Bytes.create 2 in
      put_uint8 data 0 e1;
      put_uint8 data 1 e2;
      frame
        ~identifier:106
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Odometry(x, y, theta) ->
      let data = Bytes.create 6 in
      put_sint16 data 0 (truncate (x *. 1000.));
      put_sint16 data 2 (truncate (y *. 1000.));
      put_sint16 data 4 (truncate (theta *. 10000.));
      frame
        ~identifier:104
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Odometry_ghost(x, y, theta, u, following) ->
      let data = Bytes.create 8 in
      put_sint16 data 0 (truncate (x *. 1000.));
      put_sint16 data 2 (truncate (y *. 1000.));
      put_sint16 data 4 (truncate (theta *. 10000.));
      put_uint8 data 6 u;
      put_uint8 data 7 (if following then 1 else 0);
      frame
        ~identifier:105
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Odometry_indep(x, y, theta) ->
      let data = Bytes.create 6 in
      put_sint16 data 0 (truncate (x *. 1000.));
      put_sint16 data 2 (truncate (y *. 1000.));
      put_sint16 data 4 (truncate (theta *. 10000.));
      frame
        ~identifier:107
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Motor_move(dist, speed, acc) ->
      let data = Bytes.create 8 in
      put_sint32 data 0 (truncate (dist *. 1000.));
      put_uint16 data 4 (truncate (speed *. 1000.));
      put_uint16 data 6 (truncate (acc *. 1000.));
      frame
        ~identifier:201
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Motor_turn(angle, speed, acc) ->
      let data = Bytes.create 8 in
      put_sint32 data 0 (truncate (angle *. 10000.));
      put_uint16 data 4 (truncate (speed *. 1000.));
      put_uint16 data 6 (truncate (acc *. 1000.));
      frame
        ~identifier:202
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Motor_bezier(x, y, d1, d2, theta, v) ->
      let x = int_of_float (x *. 1000.)
      and y = int_of_float (y *. 1000.)
      and d1 = int_of_float (d1 *. 100.)
      and d2 = int_of_float (d2 *. 100.)
      and theta = int_of_float (theta *. 100.)
      and v = int_of_float (v *. 1000.) in
      frame
        ~identifier:206
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data:(encode_bezier (x, y, d1, d2, theta, v))
  | Motor_command(motor_id, speed) ->
      let data = Bytes.create 5 in
      put_uint8 data 0 motor_id;
      put_sint32 data 1 speed;
      frame
        ~identifier:208
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Motor_activation (motor_id, active) ->
      let data = Bytes.create 2 in
      put_uint8 data 0 motor_id;
      put_uint8 data 1 (if active then 1 else 0);
      frame
        ~identifier:214
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Motor_move_x(dist, speed, acc) ->
      let data = Bytes.create 8 in
      put_sint32 data 0 (truncate (dist *. 1000.));
      put_uint16 data 4 (truncate (speed *. 1000.));
      put_uint16 data 6 (truncate (acc *. 1000.));
      frame
        ~identifier:215
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Motor_move_y(dist, speed, acc) ->
      let data = Bytes.create 8 in
      put_sint32 data 0 (truncate (dist *. 1000.));
      put_uint16 data 4 (truncate (speed *. 1000.));
      put_uint16 data 6 (truncate (acc *. 1000.));
      frame
        ~identifier:216
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Set_odometry(x, y, theta) ->
      let data = Bytes.create 6 in
      put_sint16 data 0 (truncate (x *. 1000.));
      put_sint16 data 2 (truncate (y *. 1000.));
      put_sint16 data 4 (truncate (theta *. 10000.));
      frame
        ~identifier:203
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Set_odometry_indep(x, y, theta) ->
      let data = Bytes.create 6 in
      put_sint16 data 0 (truncate (x *. 1000.));
      put_sint16 data 2 (truncate (y *. 1000.));
      put_sint16 data 4 (truncate (theta *. 10000.));
      frame
        ~identifier:209
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Beacon_position(angle1, angle2, distance1, distance2) ->
      let data = Bytes.create 8 in
      put_uint16 data 0 (truncate (angle1 *. 10000.));
      put_uint16 data 2 (truncate (angle2 *. 10000.));
      put_uint16 data 4 (truncate (distance1 *. 1000.));
      put_uint16 data 6 (truncate (distance2 *. 1000.));
      frame
        ~identifier:301
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Beacon_lowlevel_position(angle, width, period) ->
      let data = Bytes.create 8 in
      put_uint16 data 0 (truncate (angle *. 10000.));
      put_uint16 data 2 (truncate (width *. 10000.));
      put_uint32 data 4 period;
      frame
        ~identifier:302
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Beacon_angles(a1, a2, a3, a4) ->
      let data = Bytes.create 8 in
      put_uint16 data 0 (truncate (a1 *. 10000.));
      put_uint16 data 2 (truncate (a2 *. 10000.));
      put_uint16 data 4 (truncate (a3 *. 10000.));
      put_uint16 data 6 (truncate (a4 *. 10000.));
      frame
        ~identifier:304
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Beacon_widths(w1, w2, w3, w4) ->
      let data = Bytes.create 8 in
      put_uint16 data 0 (truncate (w1 *. 100000.));
      put_uint16 data 2 (truncate (w2 *. 100000.));
      put_uint16 data 4 (truncate (w3 *. 100000.));
      put_uint16 data 6 (truncate (w4 *. 100000.));
      frame
        ~identifier:305
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Switch1_status(sw1, sw2, sw3, sw4, sw5, sw6, sw7, sw8) ->
      let data = Bytes.create 8 in
      put_uint8 data 0 (if sw1 then 1 else 0);
      put_uint8 data 1 (if sw2 then 1 else 0);
      put_uint8 data 2 (if sw3 then 1 else 0);
      put_uint8 data 3 (if sw4 then 1 else 0);
      put_uint8 data 4 (if sw5 then 1 else 0);
      put_uint8 data 5 (if sw6 then 1 else 0);
      put_uint8 data 6 (if sw7 then 1 else 0);
      put_uint8 data 7 (if sw8 then 1 else 0);
      frame
        ~identifier:311
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data:data
  | Switch2_status(sw1, sw2, sw3, sw4, sw5, sw6, sw7, sw8) ->
      let data = Bytes.create 8 in
      put_uint8 data 0 (if sw1 then 1 else 0);
      put_uint8 data 1 (if sw2 then 1 else 0);
      put_uint8 data 2 (if sw3 then 1 else 0);
      put_uint8 data 3 (if sw4 then 1 else 0);
      put_uint8 data 4 (if sw5 then 1 else 0);
      put_uint8 data 5 (if sw6 then 1 else 0);
      put_uint8 data 6 (if sw7 then 1 else 0);
      put_uint8 data 7 (if sw8 then 1 else 0);
      frame
        ~identifier:312
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data:data
  | Switch_request(switch, status) ->
      let data = Bytes.create 2 in
      put_uint8 data 0 switch;
      put_uint8 data 1 (if status then 1 else 0);
      frame
        ~identifier:313
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data:data
  | Adc1_values(v1, v2, v3, v4) ->
      let data = Bytes.create 8 in
      put_uint16 data 0 v1;
      put_uint16 data 2 v2;
      put_uint16 data 4 v3;
      put_uint16 data 6 v4;
      frame
        ~identifier:321
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data:data
  | Adc2_values(v1, v2, v3, v4) ->
      let data = Bytes.create 8 in
      put_uint16 data 0 v1;
      put_uint16 data 2 v2;
      put_uint16 data 4 v3;
      put_uint16 data 6 v4;
      frame
        ~identifier:322
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data:data
  | Battery1_voltages(elem1, elem2, elem3, elem4) ->
      let data = Bytes.create 8 in
      put_uint16 data 0 (truncate (elem1 *. 10000.));
      put_uint16 data 2 (truncate (elem2 *. 10000.));
      put_uint16 data 4 (truncate (elem3 *. 10000.));
      put_uint16 data 5 (truncate (elem4 *. 10000.));
      frame
        ~identifier:331
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Battery2_voltages(elem1, elem2, elem3, elem4) ->
      let data = Bytes.create 8 in
      put_uint16 data 0 (truncate (elem1 *. 10000.));
      put_uint16 data 2 (truncate (elem2 *. 10000.));
      put_uint16 data 4 (truncate (elem3 *. 10000.));
      put_uint16 data 5 (truncate (elem4 *. 10000.));
      frame
        ~identifier:332
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Ax12_State(address, position, speed, torque) ->
      let data = Bytes.create 7 in
      put_uint8 data 0 address;
      put_uint16 data 1 position;
      put_uint16 data 3 speed;
      put_uint16 data 5 torque;
      frame
        ~identifier:341
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Ax12_Request_State(address) ->
      let data = Bytes.create 1 in
      put_uint8 data 0 address;
      frame
        ~identifier:342
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Ax12_Goto(address, position, speed) ->
      let data = Bytes.create 5 in
      put_uint8 data 0 address;
      put_uint16 data 1 position;
      put_uint16 data 3 speed;
      frame
        ~identifier:343
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Ax12_Goto_Reg(address, position, speed) ->
      let data = Bytes.create 5 in
      put_uint8 data 0 address;
      put_uint16 data 1 position;
      put_uint16 data 3 speed;
      frame
        ~identifier:346
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Ax12_Action(address) ->
      let data = Bytes.create 1 in
      put_uint8 data 0 address;
      frame
        ~identifier:347
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Ax12_Status_Return_Level(address, level) ->
      let data = Bytes.create 2 in
      put_uint8 data 0 address;
      put_uint8 data 1 level;
      frame
        ~identifier:348
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Ax12_Set_Torque_Enable(address, state) ->
      let data = Bytes.create 2 in
      put_uint8 data 0 address;
      put_uint8 data 1 (if state then 1 else 0);
      frame
        ~identifier:345
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Motor_stop(lin_acc, rot_acc) ->
      let data = Bytes.create 8 in
      put_float32 data 0 lin_acc;
      put_float32 data 4 rot_acc;
      frame
        ~identifier:204
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Motor_bezier_limits(v_max, omega_max, a_tan_max, a_rad_max) ->
      let v_max = v_max *. 1000. in
      let omega_max = omega_max *. 1000. in
      let a_tan_max = a_tan_max *. 1000. in
      let a_rad_max = a_rad_max *. 1000. in
      let data = Bytes.create 8 in
      put_uint16 data 0 (int_of_float v_max);
      put_uint16 data 2 (int_of_float omega_max);
      put_uint16 data 4 (int_of_float a_tan_max);
      put_uint16 data 6 (int_of_float a_rad_max);
      frame
        ~identifier:207
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Set_simulation_mode m ->
      frame
        ~identifier:205
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data:(match m with
                 | Sim_no -> "\x00"
                 | Sim_normal -> "\x01"
                 | Sim_HIL -> "\x02")
  | Elevator_command(p1, p2) ->
      let data = Bytes.create 8 in
      put_float32 data 0 p1;
      put_float32 data 4 p2;
      frame
        ~identifier:231
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Pump_command(i1, i2) ->
    let data = Bytes.create 4 in
    put_sint16 data 0 i1;
    put_sint16 data 2 i2;
    frame
      ~identifier:232
      ~kind:Data
      ~remote:false
      ~format:F29bits
      ~data
  | Elevator_encoders(pos1, dir1, pos2, dir2) ->
      let data = Bytes.create 6 in
      put_uint16 data 0 pos1;
      put_uint16 data 2 pos2;
      put_uint8 data 4 (match dir1 with Forward -> 0 | Backward -> 1);
      put_uint8 data 5 (match dir2 with Forward -> 0 | Backward -> 1);
      frame
        ~identifier:131
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Pump_state(i1, i2) ->
    let data = Bytes.create 4 in
    put_sint16 data 0 i1;
    put_sint16 data 2 i2;
    frame
      ~identifier:132
      ~kind:Data
      ~remote:false
      ~format:F29bits
      ~data
  | Effector_status(m1, m2, m3, m4) ->
      let data = Bytes.create 1 in
      let x = 0 in
      let x = if m1 then x lor 1 else x in
      let x = if m2 then x lor 2 else x in
      let x = if m3 then x lor 4 else x in
      let x = if m4 then x lor 8 else x in
      put_uint8 data 0 x;
      frame
        ~identifier:133
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Elevator_positions(left, right) ->
      let data = Bytes.create 8 in
      put_float32 data 0 left;
      put_float32 data 4 right;
      frame
        ~identifier:134
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Homing_status(left, right) ->
      let data = Bytes.create 1 in
      let x = 0 in
      let x = if right then x lor 1 else x in
      let x = if left then x lor 2 else x in
      put_uint8 data 0 x;
      frame
        ~identifier:135
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Homing_command(left, right) ->
      let data = Bytes.create 8 in
      put_float32 data 0 left;
      put_float32 data 4 right;
      frame
        ~identifier:233
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Req_motor_status ->
      frame
        ~identifier:103
        ~kind:Data
        ~remote:true
        ~format:F29bits
        ~data:""

  | Motor_omni_limits(v_max, v_rot_max, a_lin_max, a_rot_max) ->
      let v_max = v_max *. 1000. in
      let v_rot_max = v_rot_max *. 1000. in
      let a_lin_max = a_lin_max *. 1000. in
      let a_rot_max = a_rot_max *. 1000. in
      let data = Bytes.create 8 in
      put_uint16 data 0 (int_of_float v_max);
      put_uint16 data 2 (int_of_float v_rot_max);
      put_uint16 data 4 (int_of_float a_lin_max);
      put_uint16 data 6 (int_of_float a_rot_max);
      frame
        ~identifier:217
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data

  | Motor_omni_goto(x_end, y_end, theta_end) ->
      let x_end = int_of_float (x_end *. 1000.)
      and y_end = int_of_float (y_end *. 1000.)
      and theta_end = int_of_float (theta_end *. 100.) in
      frame
        ~identifier:218
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data:(encode_omni_goto (x_end, y_end, theta_end))

  | Lock_target(x, y, theta) ->
      let data = Bytes.create 6 in
      put_sint16 data 0 (truncate (x *. 1000.));
      put_sint16 data 2 (truncate (y *. 1000.));
      put_sint16 data 4 (truncate (theta *. 10000.));
      frame
        ~identifier:219
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data

  | Unlock_target ->
      frame
        ~identifier:220
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data:""

  | Lock_status(error, enabled) ->
      let data = Bytes.create 5 in
      put_float32 data 0 error;
      put_uint8 data 4 (if enabled then 1 else 0);
      frame
        ~identifier:221
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | LCD_clear ->
    frame
      ~identifier:351
      ~kind:Data
      ~remote:false
      ~format:F29bits
      ~data:""
  | LCD_backlight enabled ->
    let data = Bytes.create 1 in
    put_uint8 data 0 (if enabled then 1 else 0);
    frame
      ~identifier:352
      ~kind:Data
      ~remote:false
      ~format:F29bits
      ~data
  | LCD_refresh_line line ->
    let data = Bytes.create 1 in
    let line = if line < 1 then 1 else if line > 4 then 4 else line in
    put_uint8 data 0 line;
    frame
      ~identifier:353
      ~kind:Data
      ~remote:false
      ~format:F29bits
      ~data
  | LCD_message_part (part, txt) ->
    let data = Bytes.create 8 in
    put_uint8 data 0 part;
    Bytes.blit_string txt 0 data 1 (min 7 (String.length txt));
    frame
      ~identifier:354
      ~kind:Data
      ~remote:false
      ~format:F29bits
      ~data

  | Unknown frame ->
      frame

(* +-----------------------------------------------------------------+
   | Decoding                                                        |
   +-----------------------------------------------------------------+ *)

exception Invalid_frame of Krobot_can.frame

let () =
  Printexc.register_printer
    (function
       | Invalid_frame frame ->
           Some(Printf.sprintf "Invalid_frame%s" (Krobot_can.string_of_frame frame))
       | _ ->
           None)

external decode_bezier : string -> int * int * int * int * int * int = "krobot_message_decode_bezier"
external decode_omni_goto : string -> int * int * int = "krobot_message_decode_omni_goto"

let decode frame =
  try
    if frame.remote then
      match frame.identifier with
        | 103 ->
          Req_motor_status
        | _ ->
            Unknown frame
    else
      match frame.identifier with
        | 99 ->
            Encoder_position_direction_1_2
              (get_uint16 frame.data 0,
               (if get_uint8 frame.data 4 = 0 then Forward else Backward),
               get_uint16 frame.data 2,
               (if get_uint8 frame.data 5 = 0 then Forward else Backward))
        | 100 ->
            Encoder_position_direction_3_4
              (get_uint16 frame.data 0,
               (if get_uint8 frame.data 4 = 0 then Forward else Backward),
               get_uint16 frame.data 2,
               (if get_uint8 frame.data 5 = 0 then Forward else Backward))
        | 101 ->
            Encoder_position_speed_3
              (get_float32 frame.data 0,
               get_float32 frame.data 4)
        | 102 ->
            Encoder_position_speed_4
              (get_float32 frame.data 0,
               get_float32 frame.data 4)
        | 103 ->
            let x = get_uint8 frame.data 0 in
            Motor_status(x land 1 <> 0,
                         x land 2 <> 0,
                         x land 4 <> 0,
                         x land 8 <> 0)
        | 104 ->
            Odometry
              (float (get_sint16 frame.data 0) /. 1000.,
               float (get_sint16 frame.data 2) /. 1000.,
               float (get_sint16 frame.data 4) /. 10000.)
        | 105 ->
            Odometry_ghost
              (float (get_sint16 frame.data 0) /. 1000.,
               float (get_sint16 frame.data 2) /. 1000.,
               float (get_sint16 frame.data 4) /. 10000.,
               get_uint8 frame.data 6,
               get_uint8 frame.data 7 <> 0)
        | 106 ->
            Control_error
              (get_uint8 frame.data 0,
               get_uint8 frame.data 1)
        | 107 ->
            Odometry_indep
              (float (get_sint16 frame.data 0) /. 1000.,
               float (get_sint16 frame.data 2) /. 1000.,
               float (get_sint16 frame.data 4) /. 10000.)
        | 108 ->
            Encoder_position_speed_2
              (get_float32 frame.data 0,
               get_float32 frame.data 4)
        | 131 ->
            Elevator_encoders
              (get_uint16 frame.data 0,
               (if get_uint8 frame.data 4 = 0 then Forward else Backward),
               get_uint16 frame.data 2,
               (if get_uint8 frame.data 5 = 0 then Forward else Backward))
        | 132 ->
            Pump_state
              (get_sint16 frame.data 0,
               get_sint16 frame.data 2)
        | 133 ->
            let x = get_uint8 frame.data 0 in
            Effector_status(x land 1 <> 0,
                            x land 2 <> 0,
                            x land 4 <> 0,
                            x land 8 <> 0)
        | 134 ->
            Elevator_positions
              (get_float32 frame.data 0,
               get_float32 frame.data 4)
        | 135 ->
            let x = get_uint8 frame.data 0 in
            Homing_status(x land 2 <> 0,
                          x land 1 <> 0)
        | 201 ->
            Motor_move
              (float (get_sint32 frame.data 0) /. 1000.,
               float (get_uint16 frame.data 4) /. 1000.,
               float (get_uint16 frame.data 6) /. 1000.)
        | 202 ->
            Motor_turn
              (float (get_sint32 frame.data 0) /. 10000.,
               float (get_uint16 frame.data 4) /. 1000.,
               float (get_uint16 frame.data 6) /. 1000.)
        | 203 ->
            Set_odometry
              (float (get_sint16 frame.data 0) /. 1000.,
               float (get_sint16 frame.data 2) /. 1000.,
               float (get_sint16 frame.data 4) /. 10000.)
        | 204 ->
            Motor_stop
              (get_float32 frame.data 0,
               get_float32 frame.data 4)
        | 205 ->
            Set_simulation_mode
              (let v = get_uint8 frame.data 0 in
               if v == 1 then Sim_normal else if v == 2 then Sim_HIL else Sim_no)
        | 206 ->
            let x, y, d1, d2, theta, v = decode_bezier frame.data in
            Motor_bezier(float x /. 1000.,
                         float y /. 1000.,
                         float d1 /. 100.,
                         float d2 /. 100.,
                         float theta /. 100.,
                         float v /. 1000.)
        | 207 ->
            Motor_bezier_limits
              (float (get_uint16 frame.data 0) /. 1000.,
               float (get_uint16 frame.data 2) /. 1000.,
               float (get_uint16 frame.data 4) /. 1000.,
               float (get_uint16 frame.data 6) /. 1000.)
        | 208 ->
            Motor_command
              (get_uint8 frame.data 0,
               get_sint32 frame.data 1)
        | 209 ->
            Set_odometry_indep
              (float (get_sint16 frame.data 0) /. 1000.,
               float (get_sint16 frame.data 2) /. 1000.,
               float (get_sint16 frame.data 4) /. 10000.)
        | 210 ->
            Controller_activation
              (get_uint8 frame.data 0,
               get_uint8 frame.data 1 <> 0)
        | 211 ->
            Drive_activation
              (get_uint8 frame.data 0 <> 0)
        | 212 ->
            Torque_limit
              (get_uint8 frame.data 0,
               get_uint16 frame.data 1)
        | 213 ->
            Drive_torque_limit
              (get_uint16 frame.data 0)
        | 214 ->
            Motor_activation
              (get_uint8 frame.data 0,
               get_sint8 frame.data 1 <> 0)
        | 215 ->
            Motor_move_x
              (float (get_sint32 frame.data 0) /. 1000.,
               float (get_uint16 frame.data 4) /. 1000.,
               float (get_uint16 frame.data 6) /. 1000.)
        | 216 ->
            Motor_move_y
              (float (get_sint32 frame.data 0) /. 1000.,
               float (get_uint16 frame.data 4) /. 1000.,
               float (get_uint16 frame.data 6) /. 1000.)
        | 217 ->
            Motor_omni_limits
              (float (get_uint16 frame.data 0) /. 1000.,
               float (get_uint16 frame.data 2) /. 1000.,
               float (get_uint16 frame.data 4) /. 1000.,
               float (get_uint16 frame.data 6) /. 1000.)
        | 218 ->
            let x_end, y_end, theta_end = decode_omni_goto frame.data in
            Motor_omni_goto(float x_end /. 1000.,
                            float y_end /. 1000.,
                            float theta_end /. 100.)
        | 219 ->
            Lock_target
              (float (get_sint16 frame.data 0) /. 1000.,
               float (get_sint16 frame.data 2) /. 1000.,
               float (get_sint16 frame.data 4) /. 10000.)
        | 220 ->
            Unlock_target
        | 221 ->
            let x = get_uint8 frame.data 4 in
            Lock_status
              (get_float32 frame.data 0,
               x <> 0)
        | 231 ->
            Elevator_command
              (get_float32 frame.data 0,
               get_float32 frame.data 4)
        | 232 ->
            Pump_command
              (get_sint16 frame.data 0,
               get_sint16 frame.data 2)
        | 233 ->
            Homing_command
              (get_float32 frame.data 0,
               get_float32 frame.data 4)
        | 301 ->
            Beacon_position
              (float (get_uint16 frame.data 0) /. 10000.,
               float (get_uint16 frame.data 2) /. 10000.,
               float (get_uint16 frame.data 4) /. 1000.,
               float (get_uint16 frame.data 6) /. 1000.)
        | 302 ->
            Beacon_lowlevel_position
              (float (get_uint16 frame.data 0),
               float (get_uint16 frame.data 2) (*/. 10000*),
               get_uint32 frame.data 4)
        | 304 ->
            Beacon_angles
              (float (get_uint16 frame.data 0) /. 10000.,
               float (get_uint16 frame.data 2) /. 10000.,
               float (get_uint16 frame.data 4) /. 10000.,
               float (get_uint16 frame.data 6) /. 10000.)
        | 305 ->
            Beacon_widths
              (float (get_uint16 frame.data 0) /. 100000.,
               float (get_uint16 frame.data 2) /. 100000.,
               float (get_uint16 frame.data 4) /. 100000.,
               float (get_uint16 frame.data 6) /. 100000.)
        | 311 ->
            Switch1_status
              (get_uint8 frame.data 0 <> 0,
               get_uint8 frame.data 1 <> 0,
               get_uint8 frame.data 2 <> 0,
               get_uint8 frame.data 3 <> 0,
               get_uint8 frame.data 4 <> 0,
               get_uint8 frame.data 5 <> 0,
               get_uint8 frame.data 6 <> 0,
               get_uint8 frame.data 7 <> 0)
        | 312 ->
            Switch2_status
              (get_uint8 frame.data 0 <> 0,
               get_uint8 frame.data 1 <> 0,
               get_uint8 frame.data 2 <> 0,
               get_uint8 frame.data 3 <> 0,
               get_uint8 frame.data 4 <> 0,
               get_uint8 frame.data 5 <> 0,
               get_uint8 frame.data 6 <> 0,
               get_uint8 frame.data 7 <> 0)
        | 313 ->
            Switch_request
              (get_uint8 frame.data 0,
               get_uint8 frame.data 1 <> 0)
        | 321 ->
            Adc1_values
              (get_uint16 frame.data 0,
               get_uint16 frame.data 2,
               get_uint16 frame.data 4,
               get_uint16 frame.data 6)
        | 322 ->
            Adc2_values
              (get_uint16 frame.data 0,
               get_uint16 frame.data 2,
               get_uint16 frame.data 4,
               get_uint16 frame.data 6)
        | 331 ->
            Battery1_voltages
              (float (get_uint16 frame.data 0) /. 10000.,
               float (get_uint16 frame.data 2) /. 10000.,
               float (get_uint16 frame.data 4) /. 10000.,
               float (get_uint16 frame.data 6) /. 10000.)
        | 332 ->
            Battery2_voltages
              (float (get_uint16 frame.data 0) /. 10000.,
               float (get_uint16 frame.data 2) /. 10000.,
               float (get_uint16 frame.data 4) /. 10000.,
               float (get_uint16 frame.data 6) /. 10000.)
        | 341 ->
            Ax12_State
              (get_uint8 frame.data 0,
               get_uint16 frame.data 1,
               get_uint16 frame.data 3,
               get_uint16 frame.data 5)
        | 342 ->
            Ax12_Request_State
              (get_uint8 frame.data 0)
        | 343 ->
            Ax12_Goto
              (get_uint8 frame.data 0,
               get_uint16 frame.data 1,
               get_uint16 frame.data 3)
        | 345 ->
            Ax12_Set_Torque_Enable
              ((get_uint8 frame.data 0),
              (if (get_uint8 frame.data 1) == 0 then false else true))
        | 346 ->
            Ax12_Goto_Reg
              (get_uint8 frame.data 0,
               get_uint16 frame.data 1,
               get_uint16 frame.data 3)
        | 347 ->
            Ax12_Action
              (get_uint8 frame.data 0)
        | 348 ->
            Ax12_Status_Return_Level
              (get_uint8 frame.data 0,
               get_uint8 frame.data 1)
        | 351 ->
          LCD_clear
        | 352 ->
          LCD_backlight
            (if (get_uint8 frame.data 0) == 0 then false else true)
        | 353 ->
          LCD_refresh_line (get_uint8 frame.data 0)
        | 354 ->
          LCD_message_part
            (get_uint8 frame.data 0,
             Bytes.sub_string frame.data 1 7)

        | _ ->
            Unknown frame
  with Invalid_argument _ ->
    raise (Invalid_frame frame)

(* +-----------------------------------------------------------------+
   | Sending/receiving messages                                      |
   +-----------------------------------------------------------------+ *)

let send bus (timestamp, msg) =
  Krobot_bus.send bus (timestamp, Krobot_bus.CAN(Krobot_bus.Info, encode msg))
let recv bus =
  E.fmap
    (fun (timestamp, message) ->
       match message with
         | Krobot_bus.CAN(_, frame) ->
             Some(timestamp, decode frame)
         | _ ->
             None)
    (Krobot_bus.recv bus)
