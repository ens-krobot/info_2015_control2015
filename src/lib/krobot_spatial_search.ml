open Krobot_geom

let min (x:float) (y:float) =
  if x < y then x else y
let max (x:float) (y:float) =
  if x < y then y else x

type world_box = {
  min_x : float;
  min_y : float;
  max_x : float;
  max_y : float;
}

type world_boxes = {
  low_x_low_y : world_box;
  low_x_high_y : world_box;
  high_x_low_y : world_box;
  high_x_high_y : world_box;
}

let cut_world_box ~x ~y (box:world_box) : world_boxes =
  {
    low_x_low_y   = { box with max_x = x; max_y = y };
    low_x_high_y  = { box with max_x = x; min_y = y };
    high_x_low_y  = { box with min_x = x; max_y = y };
    high_x_high_y = { box with min_x = x; min_y = y };
  }

type 'a tree =
  | Leaf of ('a * bounding_box) list
  | Node of 'a node

and 'a node = {
  elements : ('a * bounding_box) list;
  low_x_low_y : 'a tree;
  low_x_high_y : 'a tree;
  high_x_low_y : 'a tree;
  high_x_high_y : 'a tree;
  x_split : float;
  y_split : float;
}

type 'a t =
  { world_box : world_box;
    tree : 'a tree;
    max_depth : int }

let box_inside_world (box:bounding_box) (world_box:world_box) =
  box.max_x >= world_box.min_x &&
  box.min_x <= world_box.max_x &&
  box.max_y >= world_box.min_y &&
  box.min_y <= world_box.max_y

let box_across_split ~x ~y (box:bounding_box) =
  (box.max_x >= x && box.min_x <= x) ||
  (box.max_y >= y && box.min_y <= y)

let world_included_in_box (box:bounding_box) (world_box:world_box) =
  box.max_x >= world_box.max_x &&
  box.min_x <= world_box.min_x &&
  box.max_y >= world_box.max_y &&
  box.min_y <= world_box.min_y

let rec add_tree depth value (box:bounding_box) tree (world_box:world_box) = match tree with
  | Leaf l ->
    if depth >= 0 && List.length l >= 2 then begin
      (* If the node is too crowded, split it *)
      let node =
        { elements = [];
          x_split = (world_box.min_x +. world_box.max_x) /. 2.;
          y_split = (world_box.min_y +. world_box.max_y) /. 2.;
          low_x_low_y = Leaf [];
          low_x_high_y = Leaf [];
          high_x_low_y = Leaf [];
          high_x_high_y = Leaf []; } in
      let depth = depth - 1 in
      let node =
        List.fold_left (fun node (value, box) -> add_node depth value box node world_box)
          node ((value, box) :: l)
      in
      Node node
    end
    else
      Leaf ((value, box) :: l)
  | Node node ->
    let depth = depth - 1 in
    Node (add_node depth value box node world_box)

and add_node depth value (box:bounding_box) node (world_box:world_box) =
  if box_across_split ~x:node.x_split ~y:node.y_split box then
    { node with elements = (value, box) :: node.elements }
  else
    let wboxes = cut_world_box ~x:node.x_split ~y:node.y_split world_box in
    { node with
      low_x_low_y   = add_option depth value box   node.low_x_low_y   wboxes.low_x_low_y;
      low_x_high_y  = add_option depth value box  node.low_x_high_y  wboxes.low_x_high_y;
      high_x_low_y  = add_option depth value box  node.high_x_low_y  wboxes.high_x_low_y;
      high_x_high_y = add_option depth value box node.high_x_high_y wboxes.high_x_high_y; }

and add_option depth value (box:bounding_box) tree (world_box:world_box) =
  if box_inside_world box world_box then
    add_tree depth value box tree world_box
  else
    tree

let add value box t =
  if box_inside_world box t.world_box then
    { t with tree = add_tree t.max_depth value box t.tree t.world_box }
  else
    t

let empty ?(max_depth=10) world_box =
  { world_box;
    tree = Leaf [];
    max_depth }

let inside_box vert (box:bounding_box) =
  vert.x >= box.min_x &&
  vert.x <= box.max_x &&
  vert.y >= box.min_y &&
  vert.y <= box.max_y

let line_box_collision ((v1, v2) as line) (box:bounding_box) =
  inside_box v1 box ||
  inside_box v2 box ||
  begin
    let box_vert_ll = { x = box.min_x; y = box.min_y } in
    let box_vert_lh = { x = box.min_x; y = box.max_y } in
    let box_vert_hl = { x = box.max_x; y = box.min_y } in
    let box_vert_hh = { x = box.max_x; y = box.max_y } in
    segment_intersect line (box_vert_ll, box_vert_lh) <> None ||
    segment_intersect line (box_vert_lh, box_vert_hh) <> None ||
    segment_intersect line (box_vert_hh, box_vert_hl) <> None ||
    segment_intersect line (box_vert_hl, box_vert_ll) <> None
  end

let inside_world_box vert (box:world_box) =
  vert.x >= box.min_x &&
  vert.x <= box.max_x &&
  vert.y >= box.min_y &&
  vert.y <= box.max_y

let line_world_box_collision ((v1, v2) as line) (box:world_box) =
  inside_world_box v1 box ||
  inside_world_box v2 box ||
  begin
    let box_vert_ll = { x = box.min_x; y = box.min_y } in
    let box_vert_lh = { x = box.min_x; y = box.max_y } in
    let box_vert_hl = { x = box.max_x; y = box.min_y } in
    let box_vert_hh = { x = box.max_x; y = box.max_y } in
    segment_intersect line (box_vert_ll, box_vert_lh) <> None ||
    segment_intersect line (box_vert_lh, box_vert_hh) <> None ||
    segment_intersect line (box_vert_hh, box_vert_hl) <> None ||
    segment_intersect line (box_vert_hl, box_vert_ll) <> None
  end

let rec add_collisions line tree world_box acc =
  if line_world_box_collision line world_box
  then
    match tree with
    | Leaf l ->
      List.filter (fun (_, box) -> line_box_collision line box) l @
      acc
    | Node node ->
      let wboxes = cut_world_box ~x:node.x_split ~y:node.y_split world_box in
      node.elements @ acc
      |> add_collisions line node.low_x_low_y   wboxes.low_x_low_y
      |> add_collisions line node.low_x_high_y  wboxes.low_x_high_y
      |> add_collisions line node.high_x_low_y  wboxes.high_x_low_y
      |> add_collisions line node.high_x_high_y wboxes.high_x_high_y
  else
    acc

let segment_collisions line t =
  add_collisions line t.tree t.world_box []
