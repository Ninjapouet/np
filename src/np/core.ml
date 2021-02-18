
include Lwt_react

module Stream = struct
  include Lwt_stream
end

type 'a stream = 'a Stream.t

let pipe () =
  let a, send_a = E.create () in
  let b, send_b = E.create () in
  let a_effect = E.map (fun x -> send_b x) a in
  let b_effect = E.map (fun x -> send_a x) b in
  Lwt_gc.finalise_or_exit (fun a ->
      E.stop a_effect;
      E.stop a;
      Lwt.return_unit
    ) a;
  Lwt_gc.finalise_or_exit (fun b ->
      E.stop b_effect;
      E.stop b;
      Lwt.return_unit)
    b;
  a, send_a, b, send_b
