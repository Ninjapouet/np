open React

module Component = struct

  class virtual ['e, 's] interface = object
    method virtual start : ?step:step -> 's -> unit
    method virtual stop : ?step:step -> 's -> unit
    method virtual error : 's -> 'e event
  end

  module type S = sig
    type error
    type t
    val interface : (error, t) interface
  end

  let make : type a b. (a, b) #interface ->
    (module S with type t = b and type error = a) = fun iface ->
    let module M = struct
      type error = a
      type t = b
      let interface = (iface :> (error, t) interface)
    end in
    (module M)

  module Syntax (C : S) = struct
    let start = C.interface#start
    let stop = C.interface#stop
    let error = C.interface#error
  end

end
