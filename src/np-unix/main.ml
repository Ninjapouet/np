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

open Np.Run.Std
open Np.Io

type ('a, 'b) reader = in_channel -> 'a run
type ('a, 'b) writer = out_channel -> 'a -> unit run

let ic : ('a, 'b) reader -> in_channel -> ('a, _, r) t =
  fun read ic -> ro (fun () -> read ic)

let oc : ('a, 'b) writer -> out_channel -> (_, 'a, w) t =
  fun write oc -> wo (write oc)

let io : ('a, 'c) reader -> ('b, 'c) writer -> in_channel -> out_channel -> ('a, 'b, rw) t =
  fun read write ic oc -> rw (fun () -> read ic) (write oc)

type sockopt = [
  | `bool of Unix.socket_bool_option * bool
]

let apply sock (opt : sockopt) = match opt with
  | `bool (o, b) -> Unix.setsockopt sock o b

module UDP = struct

  let protocol ?(flags = []) ?(bufsize = 1024) ?(opts = []) sockaddr =
    let domain = Unix.domain_of_sockaddr sockaddr in
    let sock = Unix.socket domain Unix.SOCK_DGRAM 0 in
    List.iter (apply sock) opts;
    let buffer = Bytes.create bufsize in
    let read () = Unix.recvfrom sock buffer 0 bufsize flags in
    let write (n, peer) =
      let rec loop off len = match Unix.sendto sock buffer 0 n flags peer with
        | size when size >= n -> ()
        | size -> loop (off + size) (len - size) in
      loop 0 n in
    let io = rw read write in
    sock, buffer, io

  let server ?flags ?bufsize ?opts sockaddr callback =
    let sock, buffer, io = protocol ?flags ?bufsize ?opts sockaddr in
    let close () = try Unix.close sock with _ -> () in
    Unix.bind sock sockaddr;
    let _ = Thread.create
        (fun () ->
           try callback buffer io |> ignore; close ()
           with _ -> close ())
        () in
    close

  let client ?flags ?bufsize ?opts sockaddr callback =
    let sock, buffer, io = protocol ?flags ?bufsize ?opts sockaddr in
    let res = try callback buffer io with e -> Fmt.pr "client: %a@." Fmt.exn e; return () in
    Unix.close sock;
    res


end

module TCP = struct

  let protocol ?(flags = []) ?(bufsize = 1024) ?(opts = []) sockaddr =
    let domain = Unix.domain_of_sockaddr sockaddr in
    let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
    List.iter (apply sock) opts;
    let make sock =
      let buffer = Bytes.create bufsize in
      let read () = Unix.recv sock buffer 0 bufsize flags in
      let write n =
        let rec loop off len = match Unix.send sock buffer 0 n flags with
          | size when size >= n -> ()
          | size -> loop (off + size) (len - size) in
        loop 0 n in
      let io = rw read write in
      buffer, io in
    sock, make

  let server ?flags ?bufsize ?opts ?(pending = 10) sockaddr callback =
    let sock, make = protocol ?flags ?bufsize ?opts sockaddr in
    Unix.bind sock sockaddr;
    Unix.listen sock pending;
    let rec loop () =
      try
        let connected, peeraddr = Unix.accept sock in
        let buffer, io = make connected in
        let service () =
          try callback peeraddr buffer io |> ignore
          with e -> Fmt.epr "tcp thread %i: %a@." Thread.(self () |> id) Fmt.exn e in
        Thread.create service () |> ignore;
        loop ()
      with
      | Unix.Unix_error (Unix.EINVAL, "accept", _) ->
          (* connection was closed *)
          ()
      | e ->
          Fmt.epr "tcp thread %i (server): %a@." Thread.(self () |> id) Fmt.exn e in
    let th = Thread.create loop () in
    let close () =
      Unix.shutdown sock Unix.SHUTDOWN_ALL;
      Unix.close sock;
      Thread.join th;
    in
    close


  let client ?flags ?bufsize ?opts sockaddr callback =
    let sock, make = protocol ?flags ?bufsize ?opts sockaddr in
    Unix.connect sock sockaddr;
    let buffer, io = make sock in
    let res = try callback buffer io with e -> Fmt.pr "client: %a@." Fmt.exn e; return () in
    Unix.close sock;
    res

end
