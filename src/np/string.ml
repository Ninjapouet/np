include Stdlib.String

open Core

let chars s =
  let index = ref 0 in
  let next () = match get s !index with
    | c -> incr index; Some c
    | exception _ -> None in
  Stream.from_direct next
