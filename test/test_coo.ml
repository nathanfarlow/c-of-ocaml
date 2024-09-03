open! Core
open! Async
open! Expect_test_helpers_async

(* let compile_and_run s = *)
(*   with_temp_dir (fun temp_dir -> *)
(*       let string_in_chan = In_channel.create_string s in *)
(*       (\* capture stdout *\) *)

(*     (\* let file = temp_dir ^/ "foo.ml" in *\) *)
(*     (\* Coo.Driver. *\) *)
(*     (\* Writer.save file ~contents:s *\) *)
(*     (\* >>= fun () -> *\) *)
(*     (\* Process.run_exn ~prog:"ocaml" ~args:[ "-c"; file ] () *\) *)
(*     (\* >>= fun () -> *\) *)
(*     (\* Process.run_lines_exn ~prog:"ocaml" ~args:[ "-o"; temp_dir ^/ "foo"; file ] () *\) *)
(*     (\* >>= fun () -> Process.run_lines_exn ~prog:"./foo" ~args:[] ()) *\) *)
(* ;; *)

let%expect_test "foo" =
  print_endline "hi!";
  [%expect {| hi! |}];
  return ()
;;
