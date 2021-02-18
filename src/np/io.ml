open React

open Component

module Readable = struct

  class virtual ['a, 'e, 's] interface = object
    inherit ['e, 's] Component.interface
    method virtual read : 's -> 'a event
  end

  module type S = sig
    type data
    type error
    type t
    val interface : (data, error, t) interface
  end

  let make : type a b c. (a, b, c) #interface ->
    (module S with type data = a
               and type error = b
               and type t = c) = fun iface ->
    let module M = struct
      type data = a
      type error = b
      type t = c
      let interface = (iface :> (data, error, t) interface)
    end in
    (module M)

end


open Run

exception EOI

type yes
type no
type r = yes * no
type w = no * yes
type 'a readable = yes * 'a
type 'a writable = 'a * yes
type rw = yes * yes
type closed = no * no
type 'a mode = R : r mode | W : w mode | RW : rw mode

type ('a, 'b) close =
  | R : ('a readable, no * 'a) close
  | W : ('a writable, 'a * no) close
  | RW : (rw, closed) close

module type io = sig
  type 'a run

  type ('a, 'b, 'c) t

  val ro : ?close:(unit -> unit run) -> (unit -> 'a run) -> ('a, _, r) t
  val wo : ?close:(unit -> unit run) -> ('a -> unit run) -> (_, 'a, w) t
  val rw :
    ?close_read:(unit -> unit run) -> (unit -> 'a run) ->
    ?close_write:(unit -> unit run) -> ('b -> unit run) -> ('a, 'b, rw) t

  val read : ('a, _, _ readable) t -> 'a run
  val write : (_, 'a, _ writable) t -> 'a -> unit run
  val close : ('a, 'b) close -> ('c, 'd, 'a) t -> ('c, 'd, 'b) t run

  val map : ('a -> 'b run) -> ('c -> 'd run) -> ('a, 'd, 'e) t -> ('b, 'c, 'e) t

  val pack : ('a, _, _ readable) t -> (_, 'b, _ writable) t -> ('a, 'b, rw) t

  val pipe : unit -> ('a, 'b, rw) t * ('b, 'a, rw) t

end

module Make (R : run) : io with type 'a run = 'a R.t = struct
  include R
  include Syntax(R)

  type 'a run = 'a t

  type ('a, 'b, 'c) t = {
    read : unit -> 'a run;
    close_read : unit -> unit run;
    write : 'b -> unit run;
    close_write : unit -> unit run;
  }

  let nothing () = return ()
  let dummy_read () = assert false
  let dummy_write _ = assert false

  let ro ?(close = nothing) read =
    {read; close_read = close; write = dummy_write; close_write = nothing}

  let wo ?(close = nothing) write =
    {read = dummy_read; close_read = nothing; write; close_write = close}

  let rw ?(close_read = nothing) read ?(close_write = nothing) write =
    {read; close_read; write; close_write}

  let read r = r.read ()
  let write w v = w.write v

  let close : type a b. (a, b) close -> ('c, 'd, a) t -> ('c, 'd, b) t run = fun c io ->
    match c with
    | R ->
        let* () = io.close_read () in
        return (Obj.magic io)
    | W ->
        let* () = io.close_write () in
        return (Obj.magic io)
    | RW ->
        let* () = io.close_read () in
        let* () = io.close_write () in
        return (Obj.magic io)

  let map decode encode io =
    let read () = let* v = io.read () in decode v in
    let write v = let* v = encode v in io.write v in
    {io with read; write}

  let pack a b = {
    read = a.read;
    close_read = a.close_read;
    write = b.write;
    close_write = b.close_write;
  }

  let pipe () =
    let l2r = Queue.create () in
    let r2l = Queue.create () in
    let read q () = match Queue.pop q with
      | a -> return a
      | exception Queue.Empty -> error EOI in
    let write q a = Queue.push a q; return () in
    let left = rw (read r2l) (write l2r) in
    let right = rw (read l2r) (write r2l) in
    left, right

end

open Std

include Make(Std)

type ('a, 'b, 'c) io = ('a, 'b, 'c) t

let in_channel : in_channel -> (in_channel -> 'a) -> ('a, _, r) io = fun ic read ->
  let read () = read ic in
  let close () = close_in ic in
  ro read ~close

let out_channel : out_channel -> (out_channel -> 'a -> unit) -> (_, 'a, w) io = fun oc write ->
  let write a = write oc a in
  let close () = close_out oc in
  wo write ~close

let b64 ?padding ?alphabet io =
  let decode a = return @@ Base64.decode_exn ?pad:padding ?alphabet a in
  let encode d = return @@ Base64.encode_exn ?pad:padding ?alphabet d in
  map decode encode io

let marshal ?(flags = []) io =
  let decode a = return @@ Marshal.from_string a 0 in
  let encode a = return @@ Marshal.to_string a flags in
  map decode encode io
