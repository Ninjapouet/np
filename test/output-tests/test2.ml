
open Np.Io
open Np_unix.Net

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
  (* udp *)
  let sockaddr = Unix.(ADDR_INET (inet_addr_loopback, 12346)) in
  let _ = UDP.server sockaddr
      (fun io ->
         let buffer, n, peer = read io in
         let msg = Bytes.sub_string buffer 0 n in
         Fmt.pr "< %s@." msg;
         let pong = Bytes.of_string "pong" in
         write io (pong, 4, peer);
         ()) in
  let () = UDP.client sockaddr
      (fun io ->
         let ping = Bytes.of_string "ping" in
         write io (ping, 4, sockaddr) |> ignore;
         let buffer, n, _ = read io in
         Fmt.pr "> %s@." (Bytes.sub_string buffer 0 n);
         ()) in
  ()

let () =
  (* tcp *)
  let sockaddr = Unix.(ADDR_INET (inet_addr_loopback, 12345)) in
  let close = TCP.server ~opts:[Reuseaddr true] sockaddr
      (fun _ io ->
         let buffer, n = read io in
         let msg = Bytes.sub_string buffer 0 n in
         Fmt.pr "< %s@." msg;
         write io (Bytes.of_string "pong", 4);
      ) in
  let _ = TCP.client sockaddr
      (fun io ->
         write io (Bytes.of_string "ping", 4);
         let buffer, n = read io in
         Fmt.pr "> %s@." (Bytes.sub_string buffer 0 n);
      ) in
  close ()

 let () =
  (* lwt tests *)
   let open Np_lwt in
   let open Np.Run.Syntax(Run) in
   let open Io in
   Lwt_main.run begin

     (* streams *)
     let wo, stream = wo_stream () in
     let ro = ro_stream stream in
     let* () = write wo "Coucou!" in
     let* msg = read ro in
     Fmt.pr "%s@." msg;

     (* udp *)
     let open Np_lwt_unix.Net in
     let sockaddr = Unix.(ADDR_INET (inet_addr_loopback, 12346)) in
     let* kill = UDP.server ~opts:[Reuseaddr true] sockaddr
         (fun io ->
            let* buffer, n, peer = read io in
            let msg = Bytes.sub_string buffer 0 n in
            Fmt.pr "< %s@." msg;
            let* () = write io (Bytes.of_string "pong", 4, peer) in
            Lwt.return_unit) in
     let* () = UDP.client sockaddr
         (fun io ->
            let* () = write io (Bytes.of_string "ping", 4, sockaddr) in
            let* buffer, n, _ = read io in
            Fmt.pr "> %s@." (Bytes.sub_string buffer 0 n);
            Lwt.return_unit) in
     kill ();


     (* tcp *)
     let sockaddr = Unix.(ADDR_INET (inet_addr_loopback, 12346)) in
     let* kill = TCP.server ~opts:[Reuseaddr true] sockaddr
         (fun _ io ->
            let* buffer, n = read io in
            let msg = Bytes.sub_string buffer 0 n in
            Fmt.pr "< %s@." msg;
            let* () = write io (Bytes.of_string "pong", 4) in
            Lwt.return_unit) in
     let* () = TCP.client sockaddr
         (fun io ->
            let* () = write io (Bytes.of_string "ping", 4) in
            let* buffer, n = read io in
            Fmt.pr "> %s@." (Bytes.sub_string buffer 0 n);
            Lwt.return_unit) in
     let () = kill () in

     Lwt.return_unit
   end
