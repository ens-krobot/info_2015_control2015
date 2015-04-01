(* #use "topfind";;
   #require "krobot";; *)

open Krobot_geom

module Vertice = struct
  type t = vertice
  let compare v1 v2 =
    let c = compare v1.x v2.x in
    if c <> 0 then c
    else compare v1.y v2.y
end
module VerticeSet = Set.Make(Vertice)
module VerticeMap = Map.Make(Vertice)

type obstacle = vertice * vertice
type segment = vertice * vertice
type distance = float

type world =
  {
    obstacles : obstacle list;
    dst : vertice;
  }

type graph =
  {
    vertices : VerticeSet.t;
    blocking : segment list;
  }

type state =
  {
    path : distance option;
    (* the length of the best known path from src to dst *)
    best : distance VerticeMap.t;
    (* best.(v) is the best known distance from src to the vertice v *)
    edges : vertice list VerticeMap.t;
    (* The known edges of the graph.
       L'absence de voisin est représenté par la liste vide, si aucune arete n'est
       associé au sommet [v] dans la map, il faut les calculer (en utilisant [neighbors]). *)
  }

let graph_vertices : world -> graph = assert false
(** list graph vertices: points where it is allowed to go.
    i.e. les sommets des rectangles étendus par le rayon du robot *)

let neighbors : src:vertice -> graph -> vertice list = assert false
(** Les voisins du sommet src: l'ensemble des sommets [s] de [graph] pour
    lesquels il n'y a pas d'intersection entre le segment [src, s] et un
    segment de graph.blocking *)

let evaluate : state -> graph -> vertice -> state = assert false

let next_to_evaluate : state -> vertice = assert false


let find_path ~src ~dst ~obstacles =
  Printf.eprintf "FAUX!";
  [dst]
