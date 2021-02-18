
(* module Datatype = struct
 *
 *   class virtual ['s] interface = object
 *     inherit ['s] Relations.Total_order.interface
 *
 *     method compare a b = match Stdlib.compare a b with
 *       | 0 -> Eq
 *       | n when n < 0 -> Lt
 *       | _ -> Gt
 *
 *   end
 *
 *   module type S = sig
 *     type t
 *     val interface : t interface
 *   end
 *
 *   let make : type a. a #interface -> (module S with type t = a) = fun iface ->
 *     let module M = struct
 *       type t = a
 *       let interface = (iface :> t interface)
 *     end in
 *     (module M)
 *
 * end
 *
 *
 * module Int = struct
 *
 *   type t = int
 *
 *   class interface = object
 *     inherit [int] Datatype.interface
 *   end
 *
 *   let interface = new interface
 * end
 *
 * module String = struct
 *
 *   type t = string
 *
 *   type error = [`eoi]
 *
 *   class interface = object
 *     inherit [string] Datatype.interface
 *     inherit [char, error, string] Io.Readable.interface
 *   end
 *
 *   let interface = new interface
 *
 * end *)
