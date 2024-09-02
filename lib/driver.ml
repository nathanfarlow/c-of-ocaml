open! Core
open! Js_of_ocaml_compiler

let go ic =
  let exe =
    Parse_bytecode.from_exe ~linkall:false ~link_info:false ~include_cmis:false ic
  in
  Opt.f exe.code |> Compile.f
;;
