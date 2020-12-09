(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2020 Julien Blond <julien.blond@gmail.com>              *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)


open Np

module Run : Run.run with type 'a t = 'a Lwt.t = struct
  type 'a t = 'a Lwt.t
  let error e = Lwt.fail e
  let return = Lwt.return
  let bind = Lwt.bind
  let prod = Lwt.both
end

include Io.Make(Run)

let ro_stream : 'a Lwt_stream.t -> ('a, _, Io.r) t = fun stream ->
  let read () = Lwt_stream.next stream in
  ro read

let wo_stream : unit -> (_, 'a, Io.w) t * 'a Lwt_stream.t * (unit -> unit run) = fun () ->
  let stream, push = Lwt_stream.create () in
  let write a = push (Some a); Lwt.return_unit in
  let out = wo write in
  let close () = push None; Lwt.return_unit in
  out, stream, close
