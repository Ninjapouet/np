
open Np.Io
open Np_unix.Main

let read_k io k = let v = read io in k v; v

let read_string io = read_k io (Fmt.pr "%s@.")

let read_int io = read_k io (Fmt.pr "%i@.")

let () =
  (* pipe *)
  let left, right = pipe () in
  write left "ping";
  read_string right |> ignore;
  write right "pong";
  read_string left |> ignore

let () =
  (* base64 *)
  let left, right = pipe () in
  let b64 = b64 left in
  write b64 "Hello World!";
  let res = read_string right in
  write right res;
  read_string b64 |> ignore

let () =
  (* marshal *)
  let left, right = pipe () in
  let m = marshal (b64 left) in
  write m 42;
  let res = read_string right in
  write right res;
  read_int m |> ignore

let () =
  (* tcp *)
  let sockaddr = Unix.(ADDR_INET (inet_addr_loopback, 12345)) in
  let close = TCP.server sockaddr
      (fun _ buffer io ->
         let n = read_int io in
         let msg = Bytes.sub_string buffer 0 n in
         Fmt.pr "%s@." msg;
         Bytes.blit_string "pong" 0 buffer 0 4;
         write io 4) in
  (* Unix.sleep 1; *)
  let _ = TCP.client sockaddr
      (fun buffer io ->
         Bytes.blit_string "ping" 0 buffer 0 4;
         write io 4 |> ignore;
         let n = read_int io in
         Fmt.pr "%s@." (Bytes.sub_string buffer 0 n)) in
  (* Unix.sleep 1; *)
  close ()(* ;
   * Unix.unlink file *)

let () =
  (* udp *)
  let sockaddr = Unix.(ADDR_INET (inet_addr_loopback, 12346)) in

  let read io = read_k io (fun (i, _) -> Fmt.pr "%i@." i) in
  let close = UDP.server sockaddr
      (fun buffer io ->
         let n, peer = read io in
         let msg = Bytes.sub_string buffer 0 n in
         Fmt.pr "%s@." msg;
         Bytes.blit_string "pong" 0 buffer 0 4;
         write io (4, peer);
         ()) in
  let _ = UDP.client sockaddr
      (fun buffer io ->
         Bytes.blit_string "ping" 0 buffer 0 4;
         write io (4, sockaddr) |> ignore;
         let n, _ = read io in
         Fmt.pr "%s@." (Bytes.sub_string buffer 0 n);
         ()) in

  close ()


let () =
  (* lwt tests *)
  let open Np_lwt.Main in
  let open Np.Run.Syntax(Run) in
  Lwt_main.run begin

    (* streams *)
    let wo, stream, close = wo_stream () in
    let ro = ro_stream stream in
    let* () = write wo "Coucou!" in
    let* msg = read ro in
    Fmt.pr "%s@." msg;
    let* () = close () in

    (* udp *)
    let open Np_lwt_unix.Main in
    let sockaddr = Unix.(ADDR_INET (inet_addr_loopback, 12346)) in
    let* close = UDP.server sockaddr
        (fun buffer io ->
           let* n, peer = read io in
           let msg = Bytes.sub_string buffer 0 n in
           Fmt.pr "%s@." msg;
           Bytes.blit_string "pong" 0 buffer 0 4;
           let* () = write io (4, peer) in
           Lwt.return_unit) in
    let* () = UDP.client sockaddr
        (fun buffer io ->
           Bytes.blit_string "ping" 0 buffer 0 4;
           let* () = write io (4, sockaddr) in
           let* n, _ = read io in
           Fmt.pr "%s@." (Bytes.sub_string buffer 0 n);
           Lwt.return_unit) in
    let* () = close () in

    (* tcp *)
    let sockaddr = Unix.(ADDR_INET (inet_addr_loopback, 12346)) in
    let* close = TCP.server sockaddr
        (fun _ buffer io ->
           let* n = read io in
           let msg = Bytes.sub_string buffer 0 n in
           Fmt.pr "%s@." msg;
           Bytes.blit_string "pong" 0 buffer 0 4;
           let* () = write io 4 in
           Lwt.return_unit) in
    let* () = TCP.client sockaddr
        (fun buffer io ->
           Bytes.blit_string "ping" 0 buffer 0 4;
           let* () = write io 4 in
           let* n = read io in
           Fmt.pr "%s@." (Bytes.sub_string buffer 0 n);
           Lwt.return_unit) in
    close ()

  end
