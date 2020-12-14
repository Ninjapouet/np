open Np.Io

include Make(Run)

let ro_stream : 'a Lwt_stream.t -> ('a, _, r) t = fun stream ->
  let read () = Lwt_stream.next stream in
  ro read

let wo_stream : unit -> (_, 'a, w) t * 'a Lwt_stream.t = fun () ->
  let stream, push = Lwt_stream.create () in
  let write a = push (Some a); Lwt.return_unit in
  let close () = push None; Lwt.return_unit in
  let out = wo write ~close in
  out, stream
