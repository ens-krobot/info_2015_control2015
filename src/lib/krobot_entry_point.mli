
module type S = sig
  val name : string
  val options : (Arg.key * Arg.spec * Arg.doc) list
  val main : Krobot_bus.t -> (float * Krobot_bus.message) React.event -> unit Lwt.t
end

module type Main = sig end

module Make (S:S) : Main

type iterator

val iterator_from_react : (float * Krobot_bus.message) React.event -> iterator

type elt =
  | Message of float * Krobot_bus.message
  | Timeout

val next : timeout:float -> iterator -> elt Lwt.t
