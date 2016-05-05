
let random_permutation a =
  let len = Array.length a in
  for i = 0 to Array.length a - 1 do
    let n = Random.int (len - i) + i in
    let v1 = a.(i) in
    let v2 = a.(n) in
    a.(i) <- v2;
    a.(n) <- v1;
  done;
  a

let random_indices n =
  let a = Array.init n (fun i -> i) in
  random_permutation a

let random_partition n a =
  let indices = random_indices (Array.length a) in
  Array.init n (fun i -> a.(indices.(i))),
  Array.init (Array.length a - n) (fun i -> a.(indices.(i + n)))

let array_partition f a =
  let in_size = ref 0 in
  for i = 0 to Array.length a - 1 do
    let v = a.(i) in
    if f v then begin
      let after_in = !in_size in
      let v' = a.(after_in) in
      a.(i) <- v';
      a.(after_in) <- v;
      incr in_size;
    end
  done;
  Array.sub a 0 !in_size,
  Array.sub a !in_size (Array.length a - !in_size)

let set = Array.init 10 (fun i -> i)
let v, v' = random_partition 10 set
let s, _ = array_partition (fun i -> i < 4) v

type ('a, 'b) ransac =
  { model : 'a array -> 'b;
    data : 'a array;
    subset_size : int;
    rounds : int;
    distance : 'a -> 'b -> float;
    filter_distance : float;
    minimum_valid : int;
    partition : int -> 'a array -> 'a array * 'a array;
    error : 'a array -> 'b -> float }

type ('a, 'b) ransac_result =
  { model : 'b;
    in_model : 'a array;
    out_of_model : 'a array;
    error : float }

let one_round r =
  let in_subset, out_of_subset = r.partition r.subset_size r.data in
  let model = r.model in_subset in
  let filter = array_partition (fun p -> r.distance p model < r.filter_distance) in
  let fiting_out, not_fiting_out = filter out_of_subset in
  let fiting_in, not_fiting_in = filter in_subset in
  if Array.length fiting_in + Array.length fiting_out > r.minimum_valid then begin
    let input = Array.append in_subset fiting_out in
    let model = r.model input in
    (* Some { model; in_model = input; out_of_model = not_fiting_out; error = r.error input model } *)

    let filter = array_partition (fun p ->
        let d = r.distance p model in
        assert(d >= 0.);
        d < r.filter_distance) in
    let fitting, not_fitting = filter r.data in

    let model, fitting, not_fitting =
      if Array.length fitting > r.minimum_valid then
        let model = r.model fitting in
        let filter = array_partition (fun p ->
            let d = r.distance p model in
            assert(d >= 0.);
            d < r.filter_distance) in
        let fitting, not_fitting = filter r.data in
        model, fitting, not_fitting
      else
        model, fitting, not_fitting
    in

    Some { model; in_model = fitting; out_of_model = not_fitting; error = r.error input model }

  end
  else begin
    (* Printf.printf "not enought %i\n%!" (Array.length fiting_in + Array.length fiting_out); *)
    None
  end

let ransac r =
  let rec loop n best =
    if n >= r.rounds then best
    else
      let best =
        match one_round r, best with
        | res, None | None, res -> res
        | (Some { error } as new_best),
          Some { error = best_error } when error < best_error ->
          new_best
        | Some _, Some _ -> best
      in
      loop (n+1) best
  in
  loop 0 None

(* module type Element = sig *)
(*   type t *)
(*   val compare : t -> t -> int *)
(* end *)

(* module Multi(E:Element) = struct *)
(*   module ESet = Set.Make(E) *)

(*   type 'b mransac = *)
(*     { model : ESet.t -> 'b; *)
(*       data : ESet.t; *)
(*       subset_size : int; *)
(*       rounds : int; *)
(*       distance : E.t -> 'b -> float; *)
(*       filter_distance : float; *)
(*       minimum_valid : int; *)
(*       error : ESet.t -> 'b -> float; *)
(*       result_count : int; *)
(*       different_limit : int; } *)

(*   type 'b result = *)
(*     { model : 'b; *)
(*       input : ESet.t; *)
(*       error : float } *)

(*   let eset_of_array a = *)
(*     Array.fold_left (fun acc elt -> ESet.add elt acc) ESet.empty a *)
(*   let eset_to_array e = *)
(*     let n = ESet.cardinal e in *)
(*     if n = 0 then [||] *)
(*     else *)
(*       let def = ESet.choose e in *)
(*       let a = Array.make n def in *)
(*       let i = ref 0 in *)
(*       ESet.iter (fun elt -> a.(!i) <- elt; incr i) e; *)
(*       a *)

  (* let random_partition n a = *)
  (*   let indices = random_indices (Array.length a) in *)
  (*   let i = ref 0 in *)
  (*   ESet.partition  *)
  (*   Array.init n (fun i -> a.(indices.(i))), *)
  (*   Array.init (Array.length a - n) (fun i -> a.(indices.(i + n))) *)

(*   let one_round r = *)
(*     let in_subset, out_of_subset = random_partition r.subset_size r.data in *)
(*     let model = r.model in_subset in *)
(*     let fiting = array_filter (fun p -> r.distance p model < r.filter_distance) r.data in *)
(*     if Array.length fiting > r.minimum_valid then begin *)
(*       let input = Array.append in_subset fiting in *)
(*       let model = r.model input in *)
(*       Some { model; input; error = r.error input model } *)
(*     end *)
(*     else None *)

(* end *)
