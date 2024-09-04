open! Core
open! Async
open! Util

let%expect_test "foo" =
  let%bind () = compile_and_run "\n    let () = putc 'H';\n\n" in
  [%expect {| H |}];
  return ()
;;
