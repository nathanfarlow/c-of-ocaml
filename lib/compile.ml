open Core
open! Js_of_ocaml_compiler
open Code

type closure_info =
  { pc : Addr.t
  ; free_vars : Var.t list
  ; cont : cont
  ; params : Var.t list
  }

type ctx =
  { prog : program
  ; visited : Hash_set.M(Int).t
  ; closures : closure_info Hashtbl.M(Int).t
  }

let find_closures program =
  let free_vars = Freevars.f program in
  fold_closures program (fun _ params cont acc -> (params, cont) :: acc) []
  |> List.map ~f:(fun (params, ((pc, _) as cont)) ->
    let free_vars =
      Addr.Map.find_opt pc free_vars
      |> Option.map ~f:Var.Set.to_list
      |> Option.value ~default:[]
    in
    pc, { pc; free_vars; params; cont })
  |> Hashtbl.of_alist_exn (module Int)
;;

let rename ctx pc args =
  let block = Addr.Map.find pc ctx.prog.blocks in
  let params = block.params in
  List.map2_exn params args ~f:(fun param arg ->
    match Var.equal param arg with
    | true -> []
    | false ->
      let fresh = Var.fresh () |> Var.to_string in
      let param = Var.to_string param in
      let arg = Var.to_string arg in
      [ Printf.sprintf "value %s = %s;" fresh arg; Printf.sprintf "%s = %s;" param fresh ])
  |> List.concat
  |> String.concat_lines
;;

let rec compile_closure ctx closure =
  let all_params = closure.free_vars @ closure.params in
  let func_name = Printf.sprintf "closure_%d" closure.pc in
  let signature = Printf.sprintf "value* %s(value* env)" func_name in
  let var_decls =
    List.mapi all_params ~f:(fun i v ->
      Printf.sprintf "  value %s = env[%d];" (Var.to_string v) i)
    |> String.concat ~sep:"\n"
  in
  let renaming = rename ctx closure.pc (closure.cont |> snd) in
  let body = compile_block ctx closure.pc in
  let num_free = List.length closure.free_vars in
  let num_params = List.length closure.params in
  let comment = Printf.sprintf "// free: %d, params: %d" num_free num_params in
  Printf.printf "%s\n%s {\n%s\n%s\n%s\n}\n\n" comment signature var_decls renaming body

and compile_block ctx (pc : Addr.t) =
  if Hash_set.mem ctx.visited pc
  then ""
  else (
    Hash_set.add ctx.visited pc;
    let block = Addr.Map.find pc ctx.prog.blocks in
    let compiled_instrs = List.map block.body ~f:(compile_instr ctx) in
    let compiled_last = compile_last ctx block.branch in
    let name = Printf.sprintf "block_%d" pc in
    let body = String.concat ~sep:"\n" (compiled_instrs @ [ compiled_last ]) in
    let args_as_str = String.concat ~sep:", " (List.map block.params ~f:Var.to_string) in
    Printf.sprintf "%s: //%s \n%s" name args_as_str body)

and compile_instr ctx (instr, _) =
  match instr with
  | Let (var, Closure (params, (pc, _))) ->
    let closure_name = Printf.sprintf "closure_%d" pc in
    let var_name = Var.to_string var in
    let assignment =
      Printf.sprintf
        "value %s = caml_alloc_closure(%s, %d)\n"
        var_name
        closure_name
        (List.length params)
    in
    let free_vars = Hashtbl.find_exn ctx.closures pc |> fun c -> c.free_vars in
    let env_assignments =
      String.concat
        ~sep:"\n"
        (List.mapi free_vars ~f:(fun i v ->
           Printf.sprintf "%s->env[%d] = %s;" var_name i (Var.to_string v)))
    in
    Printf.sprintf "%s\n%s" assignment env_assignments
  | Let (var, expr) ->
    Printf.sprintf "  value %s = %s;" (Var.to_string var) (compile_expr ctx expr)
  | Assign (var1, var2) ->
    Printf.sprintf "  %s = %s;" (Var.to_string var1) (Var.to_string var2)
  | Set_field (var, n, value) ->
    Printf.sprintf "  Field(%s, %d) = %s;" (Var.to_string var) n (Var.to_string value)
  | Offset_ref (var, n) -> Printf.sprintf "  Field(%s, 0) += %d;" (Var.to_string var) n
  | Array_set (arr, idx, value) ->
    Printf.sprintf
      "  Field(%s, Long_val(%s)) = %s;"
      (Var.to_string arr)
      (Var.to_string idx)
      (Var.to_string value)

and compile_expr _ctx expr =
  match expr with
  | Apply { f; args; exact } ->
    let dbg = if exact then "// exact" else "// not exact" in
    let args_str = String.concat ~sep:", " (List.map args ~f:Var.to_string) in
    Printf.sprintf
      "caml_call%d(%s, %s) %s"
      (List.length args)
      (Var.to_string f)
      args_str
      dbg
  | Block (tag, fields, _) ->
    let fields_str =
      Array.to_list fields |> List.map ~f:Var.to_string |> String.concat ~sep:", "
    in
    Printf.sprintf "caml_alloc(%d, %d, %s)" (Array.length fields) tag fields_str
  | Field (var, n) -> Printf.sprintf "Field(%s, %d)" (Var.to_string var) n
  | Constant c -> compile_constant c
  | Prim (prim, args) -> compile_prim prim args
  | Closure _ -> assert false

and compile_last ctx (last, _) =
  let compile_branch ctx pc args =
    let renames = rename ctx pc args in
    Printf.sprintf "%s\ngoto block_%d;" renames pc
  in
  match last with
  | Return var -> Printf.sprintf "  return %s;" (Var.to_string var)
  | Raise (var, _) -> Printf.sprintf "  caml_raise(%s);" (Var.to_string var)
  | Stop -> "  return Val_unit;"
  | Branch (pc, args) ->
    let branch = compile_branch ctx pc args in
    let block = compile_block ctx pc in
    Printf.sprintf "  %s\n%s" branch block
  | Cond (var, (pc1, args1), (pc2, args2)) ->
    let true_branch = compile_branch ctx pc1 args1 in
    let false_branch = compile_branch ctx pc2 args2 in
    let true_block = compile_block ctx pc1 in
    let false_block = compile_block ctx pc2 in
    Printf.sprintf
      "  if (%s) { %s } else { %s }\n%s\n%s"
      (Var.to_string var)
      true_branch
      false_branch
      true_block
      false_block
  | Switch (var, arr) ->
    let cases =
      Array.mapi arr ~f:(fun i (pc, args) ->
        let branch = compile_branch ctx pc args in
        let block = compile_block ctx pc in
        Printf.sprintf "    case %d: %s\n%s" i branch block)
      |> Array.to_list
      |> String.concat ~sep:"\n"
    in
    Printf.sprintf "  switch (Long_val(%s)) {\n%s\n  }" (Var.to_string var) cases
  | Pushtrap _ | Poptrap _ -> "  // Exception handling not implemented"

and compile_constant c =
  match c with
  | Int i -> Printf.sprintf "Val_int(%ld)" i
  | Float f -> Printf.sprintf "caml_copy_double(%f)" f
  | String s -> Printf.sprintf "caml_copy_string(\"%s\")" s
  | NativeString ns ->
    (match ns with
     | Byte s -> Printf.sprintf "caml_copy_string(\"%s\")" s
     | Utf (Utf8 s) -> Printf.sprintf "caml_copy_string(\"%s\")" s)
  | Int64 i -> Printf.sprintf "caml_copy_int64(%Ld)" i
  | Float_array fa ->
    let elements =
      Array.to_list fa |> List.map ~f:(Printf.sprintf "%f") |> String.concat ~sep:", "
    in
    Printf.sprintf "caml_alloc_float_array(%d, (double[]){%s})" (Array.length fa) elements
  | Tuple (tag, elements, _) ->
    let elements_str =
      Array.to_list elements |> List.map ~f:compile_constant |> String.concat ~sep:", "
    in
    Printf.sprintf "caml_alloc(%d, %d, %s)" (Array.length elements) tag elements_str

and compile_prim prim args =
  match prim, args with
  | Vectlength, [ x ] -> Printf.sprintf "Long_val(%s)" (compile_prim_arg x)
  | Array_get, [ arr; idx ] ->
    Printf.sprintf "Field(%s, Long_val(%s))" (compile_prim_arg arr) (compile_prim_arg idx)
  | Extern "%undefined", _ -> "/* undefined */ Val_unit"
  | Extern name, args ->
    let args_str = List.map args ~f:compile_prim_arg |> String.concat ~sep:", " in
    Printf.sprintf "%s(%s)" name args_str
  | Not, [ x ] -> Printf.sprintf "Val_bool(!Bool_val(%s))" (compile_prim_arg x)
  | IsInt, [ x ] -> Printf.sprintf "Val_bool(Is_long(%s))" (compile_prim_arg x)
  | Eq, [ x; y ] ->
    Printf.sprintf "Val_bool(%s == %s)" (compile_prim_arg x) (compile_prim_arg y)
  | Neq, [ x; y ] ->
    Printf.sprintf "Val_bool(%s != %s)" (compile_prim_arg x) (compile_prim_arg y)
  | Lt, [ x; y ] ->
    Printf.sprintf
      "Val_bool(Long_val(%s) < Long_val(%s))"
      (compile_prim_arg x)
      (compile_prim_arg y)
  | Le, [ x; y ] ->
    Printf.sprintf
      "Val_bool(Long_val(%s) <= Long_val(%s))"
      (compile_prim_arg x)
      (compile_prim_arg y)
  | Ult, [ x; y ] ->
    Printf.sprintf
      "Val_bool((uintnat)Long_val(%s) < (uintnat)Long_val(%s))"
      (compile_prim_arg x)
      (compile_prim_arg y)
  | _ -> Printf.sprintf "/* Unhandled primitive :O */"

and compile_prim_arg = function
  | Pv var -> Var.to_string var
  | Pc const -> compile_constant const
;;

let f prog =
  let ctx =
    { prog; visited = Hash_set.create (module Int); closures = find_closures prog }
  in
  Hashtbl.iter ctx.closures ~f:(compile_closure ctx);
  Printf.printf "int main() {\n  caml_main(closure_%d);\n  return 0;\n}\n" prog.start
;;
