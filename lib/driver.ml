open! Core
open! Js_of_ocaml_compiler

let go ic =
  (* TODO: handle this *)
  Primitive.register "caml_register_global" `Pure None (Some 3);
  let exe =
    Parse_bytecode.from_exe ~linkall:false ~link_info:false ~include_cmis:false ic
  in
  Compile.f (Opt.f exe.code)
;;
