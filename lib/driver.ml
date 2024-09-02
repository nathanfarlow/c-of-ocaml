open! Core
open! Js_of_ocaml_compiler

let init () =
  let open Primitive in
  register "caml_int64_float_of_bits" `Pure None (Some 1);
  register "caml_ml_open_descriptor_in" `Pure None (Some 1);
  register "caml_ml_open_descriptor_out" `Pure None (Some 1);
  register "caml_fresh_oo_id" `Pure None (Some 1);
  register "caml_ml_out_channels_list" `Pure None (Some 1);
  (* we ball *)
  register "caml_ensure_stack_capacity" `Pure None (Some 1);
  register "caml_ml_flush" `Pure None (Some 1);
  register "caml_register_global" `Pure None (Some 3)
;;

let go ic =
  init ();
  let exe =
    Parse_bytecode.from_exe ~linkall:false ~link_info:false ~include_cmis:false ic
  in
  Opt.f exe.code |> Compile.f
;;
