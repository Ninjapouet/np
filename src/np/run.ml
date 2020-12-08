
module type run = sig
  type 'a t
  val return : 'a -> 'a t
  val error : exn -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val prod : 'a t -> 'b t -> ('a * 'b) t
end

module Syntax (R : run) = struct
  let (let*) = R.bind
  let (and*) = R.prod
end

module Std : run with type 'a t = 'a = struct
  type 'a t = 'a
  let return a = a
  let error = raise
  let bind v f = f v
  let prod a b = a, b
end
