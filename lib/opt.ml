open Core
open Js_of_ocaml_compiler

(* This is copied from the implementation of [Driver] since it's not exposed. *)

let tailcall = Tailcall.f
let deadcode p = Deadcode.f p |> fst

let inline p =
  let p, live_vars = Deadcode.f p in
  Inline.f p live_vars
;;

let specialize_1 (p, info) =
  Specialize.f ~function_arity:(fun f -> Specialize.function_arity info f) p
;;

let specialize_js (p, info) = Specialize_js.f info p
let flow_simple p = Flow.f ~skip_param:true p
let phi = Phisimpl.f

let specialize' (p, info) =
  let p = specialize_1 (p, info) in
  let p = specialize_js (p, info) in
  p, info
;;

let specialize p = fst (specialize' p)
let eval (p, info) = Eval.f info p
let ( +> ) f g x = g (f x)
let round1 = tailcall +> inline +> deadcode +> flow_simple +> specialize' +> eval
let flow = Flow.f

let rec loop max name round i p =
  let p' = round p in
  if i >= max || Code.eq p' p then p' else loop max name round (i + 1) p'
;;

let exact_calls ~deadcode_sentinal p =
  let info = Global_flow.f ~fast:false p in
  let p = Global_deadcode.f p ~deadcode_sentinal info in
  Specialize.f ~function_arity:(fun f -> Global_flow.function_arity info f) p
;;

let o1 =
  tailcall
  +> flow_simple
  +> specialize'
  +> eval
  +> inline
  +> deadcode
  +> tailcall
  +> phi
  +> flow
  +> specialize'
  +> eval
  +> inline
  +> deadcode
  +> flow
  +> specialize'
  +> eval
  +> inline
  +> deadcode
  +> phi
  +> flow
  +> specialize
;;

let round2 = flow +> specialize' +> eval +> deadcode +> o1
let o3 = loop 10 "tailcall+inline" round1 1 +> loop 10 "flow" round2 1

let f =
  let deadcode_sentinal = Code.Var.fresh_n "undef" in
  o3 +> exact_calls ~deadcode_sentinal +> Deadcode.f +> fst
;;
