
module type S = sig
  val name : string
  val options : (Arg.key * Arg.spec * Arg.doc) list
  val main : Krobot_bus.t -> (float * Krobot_bus.message) React.event -> unit Lwt.t
end

type iterator = (float * Krobot_bus.message) Lwt_stream.t

let iterator_from_react = Lwt_react.E.to_stream

type elt =
  | Message of float * Krobot_bus.message
  | Timeout

let next ~timeout iter =
  lwt v =
    try_lwt
      Lwt.choose [
        Lwt_unix.timeout timeout;
        Lwt_stream.peek iter]
    with
      Lwt_unix.Timeout -> Lwt.return None
  in
  match v with
  | None ->
    Lwt.return Timeout
  | Some (t,m) ->
    lwt () = Lwt_stream.junk iter in
    Lwt.return (Message (t,m))

module type Main = sig end

module Make (S:S) : Main = struct

  let fork = ref true

  let options =
    Arg.align (
      S.options @
      [ "-no-fork", Arg.Clear fork, " Run in foreground" ]
    )

  let usage = Printf.sprintf "\
Usage: krobot-%s [options]\n\
options are:"
      S.name

  let () = Arg.parse options ignore usage

  let t =
    (* Display all informative messages. *)
    Lwt_log.append_rule "*" Lwt_log.Info;

    (* Open the krobot bus. *)
    lwt bus = Krobot_bus.get () in

    (* Fork if not prevented. *)
    if !fork then Krobot_daemon.daemonize bus;

    (* Kill any running vm. *)
    lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill S.name) in

    let ev = Krobot_bus.recv bus in
    let () =
      Lwt_react.E.keep
        (React.E.map (function
           | (_, Krobot_bus.Kill killed) when killed = S.name ->
             Printf.printf "%s dying\n%!" S.name;
             exit 0
           | _ -> ())
           ev)
    in
    S.main bus ev

  let () = Lwt_main.run t
end
