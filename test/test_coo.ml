open! Core
open! Async
open! Expect_test_helpers_async

let compile_and_run s =
  with_temp_dir (fun temp_dir ->
    let%bind () = Writer.save (temp_dir ^/ "foo.ml") ~contents:s in
    (* let%bind s = *)
    (*   Process.run_exn *)
    (*     ~prog:"bash" *)
    (*     ~args:[ "-c"; "ls -alh ../example/stdlib/.stdlib.objs/byte" ] *)
    (*     () *)
    (* in *)
    (* let __ = failwith s in *)
    let%bind () =
      Process.run_expect_no_output_exn
        ~prog:"ocamlc"
        ~args:
          [ "-nopervasives"
          ; "-nostdlib"
          ; "-g"
          ; "-I"
          ; "../example/stdlib/.stdlib.objs/byte"
          ; "-open"
          ; "Stdlib"
          ; "-c"
          ; temp_dir ^/ "foo.ml"
          ; "-o"
          ; temp_dir ^/ "foo.cmo"
          ]
        ()
    in
    let%bind () =
      Process.run_expect_no_output_exn
        ~prog:"ocamlc"
        ~args:
          [ "-g"
          ; "-I"
          ; "../example/stdlib"
          ; "../example/stdlib/stdlib.cma"
          ; "-o"
          ; temp_dir ^/ "a.out"
          ; temp_dir ^/ "foo.cmo"
          ]
        ()
    in
    let string_in_chan = In_channel.create (temp_dir ^/ "a.out") in
    let%bind runtime = Reader.file_contents "../lib/runtime/runtime.c" in
    let c = runtime ^ Coo.Driver.go string_in_chan in
    let out_file = temp_dir ^/ "a.out" in
    let%bind () =
      Process.run_expect_no_output_exn
        ~prog:"gcc"
        ~args:[ "-Og"; "-g"; "-o"; out_file; "-x"; "c"; "-" ]
        ~stdin:c
        ()
    in
    let%map d = Process.run_exn ~prog:out_file ~args:[] () in
    print_endline d)
;;

let%expect_test "foo" =
  let%bind () = compile_and_run "\n    let () = putc 'H';\n\n" in
  [%expect {| H |}];
  return ()
;;
