
open Core

module Net = struct

  type address = Unix.inet_addr
  type port = int

  type group_op = Join | Leave

  let make :
    ?bufsize:int ->
    ?buffer:bytes ->
    ?reuseaddr:bool ->
    ?groups:(group_op * address) event ->
    address ->
    port ->
    (address * port * string) event ->
    (address * port * string) event = fun
    ?(bufsize = 2048)
    ?(buffer = Bytes.create bufsize)
    ?(reuseaddr = true)
    ?(groups = E.never)
    address
    port
    output ->
    let bind_address = Unix.ADDR_INET (address, port) in
    let domain = Unix.domain_of_sockaddr bind_address in
    let sock = Unix.socket domain Unix.SOCK_DGRAM 0 in
    Unix.setsockopt sock Unix.SO_REUSEADDR reuseaddr;
    Unix.bind sock bind_address;
    let lwt_sock = Lwt_unix.of_unix_file_descr sock in

    let mcast_effect = E.map (function
        | Join, group ->
            Lwt_unix.mcast_add_membership lwt_sock ~ifname:address group
        | Leave, group ->
            Lwt_unix.mcast_drop_membership lwt_sock ~ifname:address group)
        groups in

    let send_effect = E.map_s (fun (dest, p, msg) ->
        let buf = Bytes.unsafe_of_string msg in
        match%lwt
          Lwt_unix.sendto lwt_sock buf 0 (Bytes.length buf) [] Unix.(ADDR_INET (dest, p))
        with
        | _ -> Lwt.return_unit
        | exception _ -> Lwt.return_unit) output in

    let rec read () =
      match%lwt Lwt_unix.recvfrom lwt_sock buffer 0 bufsize [] with
      | len, Unix.ADDR_INET (peer, peer_port) ->
          Lwt.return (peer, peer_port, Bytes.sub_string buffer 0 len)
      | _ ->
          read () in

    let inputs = E.from read in

    Lwt_gc.finalise_or_exit (fun i ->
        E.stop i;
        E.stop mcast_effect;
        E.stop send_effect;
        Lwt_unix.close lwt_sock;%lwt
        Lwt.return_unit)
      inputs;

    inputs



end
