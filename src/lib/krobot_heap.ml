
module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type HEAP = sig
  type elt
  type t
  val empty : t
  val is_empty : t -> bool
  val insert : elt -> t -> t
  val merge : t -> t -> t
  val find_min : t -> elt (* raises Not_found if heap is empty *)
  val delete_min : t -> t (* raises Not_found if heap is empty *)
end

module Make (M : OrderedType) : (HEAP with type elt = M.t) =
struct

  type elt = M.t

  type t =
    | E
    | T of int * elt * t * t

  let rank = function
    | E -> 0
    | T (r,_,_,_) -> r

  let makeT x a b =
    if rank a >= rank b then
      T (rank b + 1, x, a, b)
    else
      T (rank a + 1, x, b, a)

  let empty = E

  let is_empty h = h == E

  let rec merge h1 h2 = match h1, h2 with
    | _, E -> h1
    | E, _ -> h2
    | T (_, x, a1, b1), T (_, y, a2, b2) ->
      if M.compare x y <= 0 then
        makeT x a1 (merge b1 h2)
      else
        makeT y a2 (merge h1 b2)

  let insert x h = merge (T (1, x, E, E)) h

  let find_min = function
    | E -> raise Not_found
    | T (_, x, _, _) -> x

  let delete_min = function
    | E -> raise Not_found
    | T (_, _, a, b) -> merge a b

end
