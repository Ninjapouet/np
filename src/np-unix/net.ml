
open Np
open Run
open Io
open Net

module Socket = struct
  type 'a run = 'a Std.t
  type (_, _, _) t = Unix.file_descr
  type _ address = Unix.sockaddr

  let domain : 'a address -> 'a domain = fun a ->
    match Unix.domain_of_sockaddr a with
    | PF_UNIX -> Obj.magic Unix
    | PF_INET -> Obj.magic IPv4
    | PF_INET6 -> Obj.magic IPv6

  let unix_domain : type a. a domain -> Unix.socket_domain = function
    | Unix -> Unix.PF_UNIX
    | IPv4 -> Unix.PF_INET
    | IPv6 -> Unix.PF_INET6

  let unix_socket_type : type a. a protocol -> Unix.socket_type = function
    | UDP -> Unix.SOCK_DGRAM
    | TCP -> Unix.SOCK_STREAM

  let make : type a b. a domain -> b protocol -> (a, b, rw) t = fun domain protocol ->
    let domain = unix_domain domain in
    let protocol = unix_socket_type protocol in
    let socket = Unix.socket domain protocol 0 in
    let close () = try Unix.close socket with _ -> () in
    at_exit close;
    socket

  let set : type a b. (a, b, _) t -> (a, b) sockopt -> unit = fun s opt ->
    match opt with
    | Reuseaddr b -> Unix.setsockopt s Unix.SO_REUSEADDR b
    | TCP_no_delay b -> Unix.setsockopt s Unix.TCP_NODELAY b

  let accept s = Unix.accept s

  let bind s a = Unix.bind s a

  let connect s a = Unix.connect s a

  let listen s i = Unix.listen s i

  let shutdown : type a b. (a, b) close -> _ -> _ = fun mode s ->
    begin
      try
        match mode with
          | R -> Unix.shutdown s Unix.SHUTDOWN_RECEIVE
          | W -> Unix.shutdown s Unix.SHUTDOWN_SEND
          | RW -> Unix.shutdown s Unix.SHUTDOWN_ALL
      with
      | Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()
    end;
    s

  let close = Unix.close

  type flag = Unix.msg_flag

  let recv = Unix.recv
  let recvfrom = Unix.recvfrom
  let send = Unix.send
  let sendto = Unix.sendto
end

module Net = Make(Std)(Io)(Socket)

module UDP = struct
  open Net.UDP

  let server ?flags ?bufsize ?opts address k =
    (* The mutex/condition allows to wait the thread start in order
       be able to send requests mostly when the server returns *)
    let m = Mutex.create () in
    let cond = Condition.create () in
    Mutex.lock m;
    let th = Thread.create
        (fun () ->
           Condition.signal cond;
           Mutex.unlock m;
           server ?flags ?bufsize ?opts address k)
        () in
    Condition.wait cond m;
    th

  let client = client

end

module TCP = struct
  open Net.TCP

  let server ?flags ?bufsize ?opts address k =
    let close, run = server ?flags ?bufsize ?opts address
        (fun peer io -> Thread.create (k peer) io |> ignore) in
    let run () = match run () with
      | () -> ()
      | exception Unix.Unix_error (EINVAL, _, _) -> () (* connection closed *)
      | exception Unix.Unix_error (ECONNABORTED, "accept", _) -> () (* connection closed *)
      | exception e -> Fmt.pr "unix:tcp:server: %a@." Fmt.exn e in
    let _ = Thread.create run () in
    close


  let client ?flags ?bufsize ?opts ?(retries = 3) ?(delay = 1.) sockaddr k =
    let client = client ?flags ?bufsize ?opts sockaddr in
    let rec loop n =
      try client k with (Unix.Unix_error (Unix.ECONNREFUSED, _, _)) as e ->
        if n <= 0 then raise e else begin
          Unix.sleepf delay;
          loop (n - 1)
        end in
    loop retries

end
