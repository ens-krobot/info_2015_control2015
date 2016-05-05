type transform = { x : float; y: float; th: float }
type point = Krobot_geom.vertice = { x : float; y : float }
type vector = Krobot_geom.vector = { vx : float; vy : float }
type line = { a : point; b : point }
type map = line list
let transf (t:transform) (p:point) =
  let ct = cos t.th in
  let st = sin t.th in
  { x = ct *. p.x -. st *. p.y +. t.x;
    y = ct *. p.y +. st *. p.x +. t.y }

let vector p1 p2 = { vx = p2.x -. p1.x; vy = p2.y -. p1.y }
let turn_trigo v = { vx = -. v.vy; vy = v.vx }
let scale s v = { vx = s *. v.vx; vy = s *. v.vy }
let norm v = sqrt (v.vx *. v.vx +. v.vy *.v.vy)
let normalize v = scale (1. /. norm v) v
let line_normal { a; b } = turn_trigo (normalize (vector a b))

let dot v1 v2 = v1.vx *. v2.vx +. v1.vy *. v2.vy
(* let add_pv (p:point) v = { x = p.x +. v.vx; y = p.y +. v.vy } *)
let sq x = x *. x
let point_distance p1 p2 = sqrt (sq (p2.x -. p1.x) +. sq (p2.y -. p1.y))
let min_float (f1:float) f2 = if f1 < f2 then f1 else f2

let distance_line line point =
  let ba = vector line.b line.a in
  let d1 = dot (vector line.a point) ba > 0. in
  let d2 = dot (vector line.b point) ba < 0. in
  if d1 || d2 then
    let distance_a = point_distance point line.a in
    let distance_b = point_distance point line.b in
    min_float distance_a distance_b
  else
    abs_float (dot (line_normal line) (vector line.a point))

let distance_map (map:map) point =
  match map with
  | [] -> max_float
  | h :: t ->
    let d1 = distance_line h point in
    List.fold_left (fun min line ->
        min_float (distance_line line point) min)
      d1 t

let distance_prepared (line, normal) point =
  let dir1 = vector line.b line.a in
  let d1 = dot (vector line.a point) dir1 > 0. in
  let d2 = dot (vector line.b point) dir1 < 0. in
  if d1 || d2 then
    let distance_a = point_distance point line.a in
    let distance_b = point_distance point line.b in
    min_float distance_a distance_b
  else
    abs_float (dot normal (vector line.a point))


  (* let distance_line = *)
  (*   abs_float (dot normal (vector line.a point)) *)
  (* in *)
  (* let distance_a = point_distance point line.a in *)
  (* let distance_b = point_distance point line.b in *)
  (* min_float (min_float distance_line distance_a) distance_b *)

(* let pi = 4. *. atan 1. *)

let copy_sub a init len =
  let kind = Bigarray.Array1.kind a in
  let layout = Bigarray.Array1.layout a in
  let b = Bigarray.Array1.create kind layout len in
  let a' = Bigarray.Array1.sub a init len in
  Bigarray.Array1.blit a' b;
  b

let minimization a b =
  (* minimize || A*x - b ||^2 *)
  let a = Lacaml.D.Mat.add_const 0. a in (* copy *)
  let b' = Lacaml_D.Mat.of_col_vecs [| b |] in
  let _rank = Lacaml_D.gelsd a b' in
  copy_sub (Lacaml_D.Mat.as_vec b') 1 (Bigarray.Array2.dim2 a)


let prepare_map (map:map) =
  let prepare_line line =
    line, line_normal line
  in
  List.map prepare_line map

let closest_prepared_line (prepared_map:(line * vector) list) point =
  match prepared_map with
  | [] -> assert false
  | h :: t ->
    let d1 = distance_prepared h point in
    let (_d, line) =
      List.fold_left (fun (d, best_line) line ->
          let d' = distance_prepared line point in
        if d' < d then d', line
        else (d, best_line))
        (d1, h) t
    in
    line

let closest_prepared_lines (prepared_map:(line * vector) list) points =
  List.map (fun p -> p, closest_prepared_line prepared_map p) points


let set_coefs m b i (p, (l, n)) =
  let a = l.a in
  let m1 = (p.x *. n.vy -. p.y *. n.vx) in
  let m2 = n.vx in
  let m3 = n.vy in
  let bi = (p.x -. a.x) *. n.vx +. (p.y -.a.y) *. n.vy in
  m.{i, 1} <- m1;
  m.{i, 2} <- m2;
  m.{i, 3} <- m3;
  b.{i} <- bi

let make_problem map points =
  let prep = prepare_map map in
  let closests = closest_prepared_lines prep points in
  let n = List.length points in
  let m = Bigarray.Array2.create Bigarray.Float64 Bigarray.fortran_layout n 3 in
  let b = Bigarray.Array1.create Bigarray.Float64 Bigarray.fortran_layout n in
  List.iteri (fun i v -> set_coefs m b (i+1) v) closests;
  m, b

let invtr r = { th = -.r.{1}; x = -.r.{2}; y = -.r.{3} }

let translation x y =
  Lacaml.D.Mat.of_array
    [| [| 1.; 0.; x  |];
       [| 0.; 1.; y  |];
       [| 0.; 0.; 1. |]; |]

let rotation th =
  Lacaml.D.Mat.of_array
    [| [| cos th; -. sin th; 0. |];
       [| sin th; cos th;    0. |];
       [| 0.;     0.;        1. |]; |]

let transform (tr:transform) =
  Lacaml.D.gemm (translation tr.x tr.y) (rotation tr.th)

(* let comp_tr (tr1:transform) (tr2:transform) = *)
(*   let m = Lacaml.D.gemm (transform tr1) (transform tr2) in *)
(*   { th = atan2 (-. m.{1,2}) m.{1,1}; *)
(*     x = m.{1,3}; *)
(*     y = m.{2,3} } *)

let comp_tr (tr1:transform) (tr2:transform) =
  (* aggressive approximation... seems to be ok *)
  { th = tr1.th +. tr2.th; x = tr1.x +. tr2.x; y = tr1.y +. tr2.y }

let invert_transformation (tr:transform) =
  let m = transform tr in
  let inv =
    let inv = Lacaml.D.lacpy m in
    Lacaml.D.getri inv;
    inv
  in
  { th = atan2 (-. inv.{1,2}) inv.{1,1};
    x = inv.{1,3};
    y = inv.{2,3} }

let tr_zero = { th = 0.; x = 0.; y = 0. }

let solve ?(rounds=3) ?(init=tr_zero) map points =
  let rec loop n tr =
    if n <= 0 then tr else
      let points = List.map (transf tr) points in
      let a, b = make_problem map points in
      let tr' = invtr (minimization a b) in
      loop (n-1) (comp_tr tr tr')
  in
  loop rounds init

let rsolve ?rounds ?init map points =
  let tr = solve ?rounds ?init map (Array.to_list points) in
  (* Printf.printf "solve th: %0.5f x: %0.5f y: %0.5f\n%!" tr.th tr.x tr.y; *)
  tr

let distance map point tr =
  distance_map map (transf tr point)

(* let quality map points tr = *)
(*   let acc = ref 0. in *)
(*   Array.iter (fun point -> acc := !acc +. sq (distance map point tr)) points; *)
(*   !acc /. float (Array.length points) *)

let quality map points tr =
  1. /. float (Array.length points)

let ransac_param ?rounds ?init map data =
  { Ransac.model = rsolve ?rounds ?init map ;
    data;
    subset_size = 10;
    rounds = 50;
    distance = distance map;
    filter_distance = 0.1;
    minimum_valid = 40;
    partition = Ransac.random_partition;
    error = quality map }
