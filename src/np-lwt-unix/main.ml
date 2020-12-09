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

open Np_lwt.Main
open Np.Run.Syntax(Np_lwt.Main.Run)


type sockopt = Np_unix.Main.sockopt

let apply sock opt = match opt with
  | `bool (o, b) -> Lwt_unix.setsockopt sock o b

module UDP = struct

  let protocol ?(flags = []) ?(bufsize = 1024) ?(opts = []) sockaddr =
    let domain = Unix.domain_of_sockaddr sockaddr in
    let sock = Lwt_unix.socket domain Unix.SOCK_DGRAM 0 in
    List.iter (apply sock) opts;
    let buffer = Bytes.create bufsize in
    let read () = Lwt_unix.recvfrom sock buffer 0 bufsize flags in
    let write (n, peer) =
      let rec loop off len =
        let* size = Lwt_unix.sendto sock buffer 0 n flags peer in
        if size >= n then Lwt.return_unit
        else loop (off + size) (len - size) in
      loop 0 n in
    let io = rw read write in
    sock, buffer, io

  let server ?flags ?bufsize ?opts sockaddr callback =
    let sock, buffer, io = protocol ?flags ?bufsize ?opts sockaddr in
    let close () = Lwt.catch (fun () -> Lwt_unix.close sock) (fun _ -> Lwt.return_unit) in
    let* () = Lwt_unix.bind sock sockaddr in
    Lwt.async (fun () ->
        Lwt.catch
          (fun () ->
             let* res = callback buffer io in
             let* () = close () in
             Lwt.return res)
          (fun _ -> close ()));
    Lwt.return close

  let client ?flags ?bufsize ?opts sockaddr callback =
    let sock, buffer, io = protocol ?flags ?bufsize ?opts sockaddr in
    let* res =
      try callback buffer io
      with e -> Fmt.pr "client: %a@." Fmt.exn e; Lwt.return_unit in
    let* () = Lwt_unix.close sock in
    Lwt.return res

end

module TCP = struct

  let protocol ?(flags = []) ?(bufsize = 1024) ?(opts = []) sockaddr =
    let domain = Unix.domain_of_sockaddr sockaddr in
    let sock = Lwt_unix.socket domain Unix.SOCK_STREAM 0 in
    List.iter (apply sock) opts;
    let make sock =
      let buffer = Bytes.create bufsize in
      let read () = Lwt_unix.recv sock buffer 0 bufsize flags in
      let write n =
        let rec loop off len =
          let* size = Lwt_unix.send sock buffer 0 n flags in
          if size >= n then Lwt.return_unit
          else loop (off + size) (len - size) in
        loop 0 n in
      let io = rw read write in
      buffer, io in
    sock, make


  let server ?flags ?bufsize ?opts ?(pending = 10) sockaddr callback =
    let sock, make = protocol ?flags ?bufsize ?opts sockaddr in
    let* () = Lwt_unix.bind sock sockaddr in
    Lwt_unix.listen sock pending;
    let rec loop () =
      let* connected, peer = Lwt_unix.accept sock in
      let buffer, io = make connected in
      let service () = Lwt.catch
          (fun () -> callback peer buffer io)
          (fun _ -> Lwt.return_unit) in
      Lwt.async service;
      loop () in
    Lwt.async (fun () ->
        Lwt.catch
          (fun () -> loop ())
          (function
            | Unix.Unix_error (Unix.EINVAL, "accept", _)
            | Unix.Unix_error (Unix.EBADF, _, _) ->
                (* connection was closed *)
                Lwt.return_unit
            | e ->
                Lwt.fail e));
    let close () =
      Lwt_unix.shutdown sock Unix.SHUTDOWN_ALL;
      Lwt_unix.close sock in
    Lwt.return close


  let client ?flags ?bufsize ?opts sockaddr callback =
    let sock, make = protocol ?flags ?bufsize ?opts sockaddr in
    let* () = Lwt_unix.connect sock sockaddr in
    let buffer, io = make sock in
    let* res = callback buffer io in
    let* () = Lwt_unix.close sock in
    Lwt.return res

end
