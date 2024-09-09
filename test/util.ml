open Core
open Async
open Expect_test_helpers_async

let compile_and_run source =
  with_temp_dir (fun temp_dir ->
    let ( ~/ ) f = temp_dir ^/ f in
    let ml = ~/"a.ml" in
    let cmo = ~/"a.cmo" in
    let bc = ~/"a.bc" in
    let exe = ~/"a.out" in
    let%bind () = Writer.save ml ~contents:source in
    let%bind () =
      Process.run_expect_no_output_exn
        ~prog:"ocamlc"
        ~args:
          [ "-nopervasives"
          ; "-nostdlib"
          ; "-g"
          ; "-I"
          ; "../runtime/stdlib/.stdlib.objs/byte"
          ; "-open"
          ; "Stdlib"
          ; "-c"
          ; ml
          ; "-o"
          ; cmo
          ]
        ()
    in
    let%bind () =
      Process.run_expect_no_output_exn
        ~prog:"ocamlc"
        ~args:
          [ "-g"
          ; "-I"
          ; "../runtime/stdlib"
          ; "../runtime/stdlib/stdlib.cma"
          ; "-o"
          ; bc
          ; cmo
          ]
        ()
    in
    let%bind runtime = Reader.file_contents "../runtime/runtime.c" in
    let string_in_chan = In_channel.create bc in
    let c_code = runtime ^ C_of_ocaml.Driver.go string_in_chan in
    let%bind () =
      Process.run_expect_no_output_exn
        ~prog:"cc"
        ~args:
          [ "-ansi"
          ; "-O0"
          ; "-g"
          ; "-o"
          ; exe
          ; "-Wall"
          ; "-Wno-unused-variable"
          ; "-Wno-unused-label"
          ; "-x"
          ; "c"
          ; "-"
          ]
        ~stdin:c_code
        ()
    in
    let%map output = Process.run_exn ~prog:exe ~args:[] () in
    print_endline output)
;;
