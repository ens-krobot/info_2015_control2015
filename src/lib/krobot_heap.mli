
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

module Make (M : OrderedType) : (HEAP with type elt = M.t)
