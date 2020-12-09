open Run

type yes
type no
type r = yes * no
type w = no * yes
type rw = yes * yes
type 'a readable = yes * 'a
type 'a writable = 'a * yes

exception EOI

module type io = sig
  type 'a run

  type ('a, 'b, 'c) t

  val ro : (unit -> 'a run) -> ('a, _, r) t
  val wo : ('a -> unit run) -> (_, 'a, w) t
  val rw : (unit -> 'a run) -> ('b -> unit run) -> ('a, 'b, rw) t
  val pack : ('a, _, _ readable) t -> (_, 'b, _ writable) t -> ('a, 'b, rw) t
  val restrict_ro : ('a, 'b, rw) t -> ('a, 'b, r) t
  val restrict_wo : ('a, 'b, rw) t -> ('a, 'b, w) t

  val read : ('a, _, _ readable) t -> 'a run
  val write : (_, 'a, _ writable) t -> 'a -> unit run

  val map : ('a -> 'b run) -> ('d -> 'e run) -> ('a, 'e, 'f) t -> ('b, 'd, 'f) t
  val pipe : unit -> ('a, 'b, rw) t * ('b, 'a, rw) t

end

module Make (R : run) : io with type 'a run = 'a R.t = struct
  include R
  include Syntax(R)

  type 'a run = 'a t

  type ('a, 'b, 'c) t = {
    read : unit -> 'a run;
    write : 'b -> unit run;
  }

  let ro read = {read; write = fun _ -> assert false}
  let wo write = {read = (fun () -> assert false); write}
  let rw read write = {read; write}

  let pack : ('a, _, _ readable) t -> (_, 'b, _ writable) t -> ('a, 'b, rw) t = fun r w ->
    {read = r.read; write = w.write}

  let restrict_ro : ('a, 'b, rw) t -> ('a, 'b, r) t = fun io ->
    {read = io.read; write = io.write}

  let restrict_wo : ('a, 'b, rw) t -> ('a, 'b, w) t = fun io ->
    {read = io.read; write = io.write}

  let read io = io.read ()
  let write io a = io.write a


  let map : ('a -> 'b run) -> ('d -> 'e run) -> ('a, 'e, 'f) t -> ('b, 'd, 'f) t =
    fun decode encode io ->
    let read () =
      let* a = read io in
      let* b = decode a in
      return b in
    let write d =
      let* e = encode d in
      let* () = write io e in
      return () in
    {read; write}


  let pipe : unit -> ('a, 'b, rw) t * ('b, 'a, rw) t = fun () ->
    let l2r = Queue.create () in
    let r2l = Queue.create () in
    let read q = match Queue.pop q with
      | a -> return a
      | exception Queue.Empty -> error EOI in
    let write q a = Queue.push a q; return () in
    let left = rw (fun () -> read r2l) (write l2r) in
    let right = rw (fun () -> read l2r) (write r2l) in
    left, right

end

open Std

include Make(Std)

let b64 : ?padding:bool -> ?alphabet:Base64.alphabet ->
  (string, string, 'c) t -> (string, string, 'c) t =
  fun ?padding ?alphabet io ->
  let decode a = return @@ Base64.decode_exn ?pad:padding ?alphabet a in
  let encode d = return @@ Base64.encode_exn ?pad:padding ?alphabet d in
  map decode encode io

let marshal : ?flags:Marshal.extern_flags list -> (string, string, 'c) t -> ('a, 'a, 'c) t =
  fun ?(flags = []) io ->
  let decode a = return @@ Marshal.from_string a 0 in
  let encode a = return @@ Marshal.to_string a flags in
  map decode encode io
