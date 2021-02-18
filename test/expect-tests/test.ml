open Np.Core

let%expect_test "String.chars" = Lwt_main.run begin
    let chars = Np.String.chars "Hello" in
    let%lwt s = Stream.to_string chars in
    Fmt.pr "%s@." s;
    [%expect {| Hello |}];
    Lwt.return_unit
  end
