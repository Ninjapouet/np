
type 'a t = 'a Lwt.t
let error e = Lwt.fail e
let return = Lwt.return
let bind = Lwt.bind
let prod = Lwt.both
