

module Equivalence = struct

  class virtual ['s] interface = object(self)
    method virtual equal : 's -> 's -> bool
    method different : 's -> 's -> bool = fun a b -> not (self#equal a b)
  end

  module type S = sig
    type t
    val interface : t interface
  end

  let make : type a. a #interface -> (module S with type t = a) = fun iface ->
    let module M = struct
      type t = a
      let interface = (iface :> t interface)
    end in
    (module M)

  module Syntax (E : S) = struct
    let equal = E.interface#equal
    let ( = ) = equal
    let different = E.interface#different
    let ( <> ) = different
  end

end

module Partial_order = struct

  class virtual ['s] interface = object(self)
    inherit ['s] Equivalence.interface
    method virtual leq : 's -> 's -> bool
    method equal a b = self#leq a b && self#leq b a
    method geq a b = self#leq b a
  end

  module type S = sig
    type t
    val interface : t interface
  end

  let make : type a. a #interface -> (module S with type t = a) = fun iface ->
    let module M = struct
      type t = a
      let interface = (iface :> t interface)
    end in
    (module M)

  module Syntax (Po : S) = struct
    include Equivalence.Syntax(val Equivalence.make Po.interface)
    let leq = Po.interface#leq
    let ( <= ) = leq
    let geq = Po.interface#geq
    let ( >= ) = geq
  end
end

module Total_order = struct

  type cmp = Lt | Eq | Gt

  class virtual ['s] interface = object(self)
    inherit ['s] Partial_order.interface
    method virtual compare : 's -> 's -> cmp
    method! equal a b = self#compare a b = Eq
    method leq a b = match self#compare a b with Lt | Eq -> true | Gt -> false
  end

  module type S = sig
    type t
    val interface : t interface
  end

  let make : type a. a #interface -> (module S with type t = a) = fun iface ->
    let module M = struct
      type t = a
      let interface = (iface :> t interface)
    end in
    (module M)

  module Syntax (To : S) = struct
    include Partial_order.Syntax(val Partial_order.make To.interface)
    let compare = To.interface#compare
  end
end
