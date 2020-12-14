
open Run
open Io

type udp
type tcp

type 'a protocol =
  | UDP : udp protocol
  | TCP : tcp protocol

type unix
type ipv4
type ipv6

type 'a domain =
  | Unix : unix domain
  | IPv4 : ipv4 domain
  | IPv6 : ipv6 domain

type ('a, 'b) sockopt =
  | Reuseaddr : bool -> (_, _) sockopt
  | TCP_no_delay : bool -> (_, tcp) sockopt

module type socket = sig
  type 'a run
  type ('a, 'b, 'c) t
  type 'a address
  val domain : 'a address -> 'a domain
  val make : 'a domain -> 'b protocol -> ('a, 'b, rw) t
  val set : ('a, 'b, rw) t -> ('a, 'b) sockopt -> unit
  val accept : ('a, tcp, rw) t -> (('a, tcp, rw) t * 'a address) run
  val bind : ('a, 'b, rw) t -> 'a address -> unit run
  val connect : ('a, tcp, rw) t -> 'a address -> unit run
  val listen : ('a, tcp, rw) t -> int -> unit run
  val shutdown : ('a, 'b) close -> ('c, 'd, 'a) t -> ('c, 'd, 'b) t run
  val close : ('a, 'b, 'c) t -> unit run

  type flag
  val recv : ('a, tcp, rw) t -> bytes -> int -> int -> flag list -> int run
  val recvfrom : ('a, udp, rw) t -> bytes -> int -> int -> flag list -> (int * 'a address) run
  val send : ('a, tcp, rw) t -> bytes -> int -> int -> flag list -> int run
  val sendto : ('a, udp, rw) t -> bytes -> int -> int -> flag list -> 'a address -> int run
end

module type net = sig
  type 'a run
  type 'a address
  type flag
  type ('a, 'b, 'c) io

  module UDP : sig

    val server :
      ?flags:flag list -> ?bufsize:int -> ?opts:('a, udp) sockopt list -> 'a address ->
      ((bytes * int * 'a address, bytes * int * 'a address, rw) io -> 'b run) ->
      'b run

    val client :
      ?flags:flag list -> ?bufsize:int -> ?opts:('a, udp) sockopt list -> 'a address ->
      ((bytes * int * 'a address, bytes * int * 'a address, rw) io -> 'b run) ->
      'b run

  end

  module TCP : sig

    val server :
      ?flags:flag list -> ?bufsize:int -> ?opts:('a, tcp) sockopt list -> ?pending:int ->
      'a address ->
      ('a address -> (bytes * int, bytes * int, rw) io -> unit run) ->
      ((unit -> unit run) * (unit -> unit run)) run

    val client :
      ?flags:flag list -> ?bufsize:int -> ?opts:('a, tcp) sockopt list -> 'a address ->
      ((bytes * int, bytes * int, rw) io -> unit run) -> unit run

  end

end

module Make
    (R : run)
    (IO : io with type 'a run = 'a R.t)
    (S : socket with type 'a run = 'a R.t) :
  net with type 'a run = 'a R.t
       and type 'a address = 'a S.address
       and type flag = S.flag
       and type ('a, 'b, 'c) io = ('a, 'b, 'c) IO.t =
struct
  open R
  open Syntax(R)
  open IO

  type 'a run = 'a R.t
  type 'a address = 'a S.address
  type flag = S.flag
  type ('a, 'b, 'c) io = ('a, 'b, 'c) IO.t

  let rec send_loop max send off len =
    let* n = send off len in
    if n >= max then return ()
    else send_loop max send (off + n) (len - n)

  module UDP = struct

    let io ?(flags = []) ?(bufsize = 1024) socket =
      let buffer = Bytes.create bufsize in
      let read () =
        let* n, peer = S.recvfrom socket buffer 0 bufsize flags in
        return (buffer, n, peer) in
      let write (buffer, n, peer) =
        send_loop n (fun off len -> S.sendto socket buffer off len flags peer) 0 n in
      rw
        read
        ~close_read:(fun () -> let* _ = S.shutdown R socket in return ())
        write
        ~close_write:(fun () -> let* _ = S.shutdown W socket in return ())

    let init ?flags ?bufsize ?(opts = []) address =
      let domain = S.domain address in
      let socket = S.make domain UDP in
      List.iter (S.set socket) opts;
      let io = io ?flags ?bufsize socket in
      let close () =
        let* socket = S.shutdown RW socket in
        S.close socket in
      socket, io, close

    let server ?flags ?bufsize ?opts address k =
      let socket, io, close = init ?flags ?bufsize ?opts address in
      let* () = S.bind socket address in
      let* res = k io in
      let* () = close () in
      return res

    let client ?flags ?bufsize ?opts address k =
      let _, io, close = init ?flags ?bufsize ?opts address in
      let* res = k io in
      let* () = close () in
      return res


  end

  module TCP = struct

    let io ?(flags = []) ?(bufsize = 1024) socket =
      let buffer = Bytes.create bufsize in
      let read () =
        let* n = S.recv socket buffer 0 bufsize flags in
        return (buffer, n) in
      let write (buffer, n) =
        send_loop n (fun off len -> S.send socket buffer off len flags) 0 n in
      rw
        read
        ~close_read:(fun () -> let* _ = S.shutdown R socket in return ())
        write
        ~close_write:(fun () -> let* _ = S.shutdown W socket in return ())

    let init ?(opts = []) address =
      let domain = S.domain address in
      let socket = S.make domain TCP in
      List.iter (S.set socket) opts;
      let close () =
        let* socket = S.shutdown RW socket in
        S.close socket in
      socket, close

    let server ?flags ?bufsize ?opts ?(pending = 10) address k =
      let socket, close = init ?opts address in
      let* () = S.bind socket address in
      let* () = S.listen socket pending in
      let rec loop () =
        let* connected, peer = S.accept socket in
        let io = io ?flags ?bufsize connected in
        let* () = k peer io in
        loop () in
      return (close, loop)

    let client ?flags ?bufsize ?opts address =
      let socket, close = init ?opts address in
      let io = io ?flags ?bufsize socket in
      fun k ->
        let* () = S.connect socket address in
        let* res = k io in
        let* () = close () in
        return res

  end

end
