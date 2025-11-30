open! Core
open! Js_of_ocaml_compiler

let go ic =
  (Parse_bytecode.from_exe ~linkall:false ~link_info:false ~include_cmis:false ic).code
  |> Opt.f
  |> Compile.f
;;
