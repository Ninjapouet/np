open Np.Io
open Np.Net
open Np_lwt
open Np.Run.Syntax(Run)


module Socket = struct

  open Np_unix.Net.Socket

  type 'a run = 'a Run.t

  type ('a, 'b, 'c) t = Lwt_unix.file_descr

  type 'a address = Lwt_unix.sockaddr


  let domain = domain

  let make : type a b. a domain -> b protocol -> (a, b, rw) t = fun domain protocol ->
    let domain = unix_domain domain in
    let protocol = unix_socket_type protocol in
    let socket = Lwt_unix.socket domain protocol 0 in
    let close s = Lwt.catch (fun () -> Lwt_unix.close s) (fun _ -> Lwt.return_unit) in
    Lwt_gc.finalise_or_exit (fun s -> close s) socket;
    socket

  let set : type a b. (a, b, _) t -> (a, b) sockopt -> unit = fun s opt ->
    match opt with
    | Reuseaddr b -> Lwt_unix.setsockopt s Unix.SO_REUSEADDR b
    | TCP_no_delay b -> Lwt_unix.setsockopt s Unix.TCP_NODELAY b

  let accept s = Lwt_unix.accept s

  let bind s a = Lwt_unix.bind s a

  let connect s a = Lwt_unix.connect s a

  let listen s i = Lwt_unix.listen s i; Lwt.return_unit

  let shutdown : type a b. (a, b) close -> _ -> _ = fun mode s ->
    begin
      try
        match mode with
          | R -> Lwt_unix.(shutdown s SHUTDOWN_RECEIVE)
          | W -> Lwt_unix.(shutdown s Unix.SHUTDOWN_SEND)
          | RW -> Lwt_unix.(shutdown s Unix.SHUTDOWN_ALL)
      with
      | Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()
    end;
    Lwt.return s

  let close = Lwt_unix.close

  type nonrec flag = flag

  let recv = Lwt_unix.recv
  let recvfrom = Lwt_unix.recvfrom
  let send = Lwt_unix.send
  let sendto = Lwt_unix.sendto


end

module Net = Np.Net.Make(Run)(Io)(Socket)

open Lwt_react

module UDP = struct
  open Net.UDP

  let server ?flags ?bufsize ?opts address k =
    let kill, send_kill = E.create () in
    Lwt.async (fun () -> Lwt.pick [
        E.next kill;
        server ?flags ?bufsize ?opts address k;
      ]);
    Lwt.return send_kill

  let client = client


end

module TCP = struct
  open Net.TCP

  let server ?flags ?bufsize ?opts ?pending address k =
    let kill, send_kill = E.create () in
    let* close, run = server ?flags ?bufsize ?opts ?pending address
        (fun peer io ->
           Lwt.async (fun () -> Lwt.pick [E.next kill; k peer io]);
           Lwt.return_unit) in
    (* wrappers *)
    let close () = Lwt.catch
        (fun () -> close ())
        (function
          | Unix.Unix_error (EBADF, _, _) -> Lwt.return_unit (* already closed *)
          | e -> Lwt.fail e) in
    let run () =
      Lwt.catch
        (fun () -> run ())
        (function
          | Unix.Unix_error (EBADF, _, _) -> Lwt.return_unit (* closed socket *)
          | e -> Fmt.pr "lwt:tcp:server: %a@." Fmt.exn e; Lwt.return_unit) in
    let terminate () =
      let* () = E.next kill in
      close () in
    Lwt.async (fun () -> Lwt.pick [terminate (); run ()]);
    Lwt.return send_kill


  let client ?flags ?bufsize ?opts ?(retries = 3) ?(delay = 1.0) address k =
    let client = client ?flags ?bufsize ?opts address in
    let rec loop n =
      Lwt.catch
        (fun () -> client k)
        (function
          | (Unix.Unix_error (Unix.ECONNREFUSED, _, _)) as e ->
              if n <= 0 then raise e else begin
                let* () = Lwt_unix.sleep delay in
                loop (n - 1)
              end
          | e -> Lwt.fail e) in
    loop retries



end
