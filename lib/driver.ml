open Core
open! Js_of_ocaml_compiler

let go () =
  let kind = Parse_bytecode.from_channel In_channel.stdin in
  let cmo =
    match kind with
    | `Cmo cmo -> cmo
    | _ -> assert false
  in
  let one =
    Parse_bytecode.from_cmo
      ~includes:[]
      ~include_cmis:false
      ~debug:true
      cmo
      In_channel.stdin
  in
  Opt.f one.code |> Compile.f
;;
