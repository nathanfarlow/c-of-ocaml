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
      |> Option.map ~f:Var.Set.elements
      |> Option.value ~default:[]
    in
    pc, { pc; free_vars; params; cont })
  |> Hashtbl.of_alist_exn (module Int)
;;

let rename ctx pc args ~is_fresh:_ =
  let block = Addr.Map.find pc ctx.prog.blocks in
  let params = block.params in
  List.map2_exn params args ~f:(fun param arg ->
    match Var.equal param arg with
    | true -> None
    | false ->
      let param = Var.to_string param in
      let arg = Var.to_string arg in
      Some (Printf.sprintf "%s = %s;" param arg))
  |> List.filter_opt
  |> String.concat_lines
;;

let rec compile_closure ctx closure =
  let all_params = closure.free_vars @ closure.params in
  let func_name = Printf.sprintf "closure_%d" closure.pc in
  let signature = Printf.sprintf "value %s(value* env)" func_name in
  (* Collect all block parameters in the closure *)
  let block_params = Hash_set.create (module String) in
  let rec collect_block_params pc =
    if not (Hash_set.mem ctx.visited pc)
    then (
      Hash_set.add ctx.visited pc;
      let block = Addr.Map.find pc ctx.prog.blocks in
      List.iter block.params ~f:(fun param ->
        Hash_set.add block_params (Var.to_string param));
      match block.branch with
      | Branch (next_pc, _), _ -> collect_block_params next_pc
      | Cond (_, (pc1, _), (pc2, _)), _ ->
        collect_block_params pc1;
        collect_block_params pc2
      | Switch (_, arr), _ -> Array.iter arr ~f:(fun (pc, _) -> collect_block_params pc)
      | Pushtrap ((pc1, _), _, (pc2, _)), _ ->
        collect_block_params pc1;
        collect_block_params pc2
      | Poptrap (pc, _), _ -> collect_block_params pc
      | _ -> ())
  in
  collect_block_params closure.pc;
  Hash_set.clear ctx.visited;
  (* Declare all block parameters at the top of the closure *)
  let var_decls =
    Hash_set.to_list block_params
    |> List.map ~f:(fun v -> Printf.sprintf "  value %s;" v)
    |> String.concat ~sep:"\n"
  in
  let param_assignments =
    List.mapi all_params ~f:(fun i v ->
      Printf.sprintf "  value %s = env[%d];" (Var.to_string v) i)
    |> String.concat ~sep:"\n"
  in
  let renaming = rename ctx closure.pc (closure.cont |> snd) ~is_fresh:true in
  let body = compile_block ctx closure.pc |> fst in
  let num_free = List.length closure.free_vars in
  let num_params = List.length closure.params in
  let comment = Printf.sprintf "// free: %d, params: %d" num_free num_params in
  Printf.printf
    "%s\n%s {\n%s\n%s\n%s\n%s\n}\n\n"
    comment
    signature
    var_decls
    param_assignments
    renaming
    body

and compile_block ctx (pc : Addr.t) =
  if Hash_set.mem ctx.visited pc
  then "", false
  else (
    Hash_set.add ctx.visited pc;
    let block = Addr.Map.find pc ctx.prog.blocks in
    let compiled_instrs = List.map block.body ~f:(compile_instr ctx) in
    let compiled_last = compile_last ctx block.branch in
    let name = Printf.sprintf "block_%d" pc in
    let body = String.concat ~sep:"\n" (compiled_instrs @ [ compiled_last ]) in
    let args_as_str = String.concat ~sep:", " (List.map block.params ~f:Var.to_string) in
    Printf.sprintf "%s:; //%s \n%s" name args_as_str body, true)

and compile_instr ctx (instr, _) =
  match instr with
  | Let (var, Closure (params, (pc, _))) ->
    let closure_name = Printf.sprintf "closure_%d" pc in
    let var_name = Var.to_string var in
    let free_vars = Hashtbl.find_exn ctx.closures pc |> fun c -> c.free_vars in
    let assignment =
      Printf.sprintf
        "value %s = caml_alloc_closure(%s, %d, %d);\n"
        var_name
        closure_name
        (List.length params)
        (List.length free_vars)
    in
    let env_assignments =
      List.map free_vars ~f:(fun fv ->
        Printf.sprintf "  add_arg(%s, %s);" var_name (Var.to_string fv))
      |> String.concat ~sep:"\n"
    in
    Printf.sprintf "%s%s" assignment env_assignments
  | Let (var, expr) ->
    Printf.sprintf "  value %s = %s;" (Var.to_string var) (compile_expr ctx expr)
  | Assign (var1, var2) ->
    Printf.sprintf "  %s = %s;" (Var.to_string var1) (Var.to_string var2)
  | Set_field (var, n, _, value) ->
    Printf.sprintf "  Field(%s, %d) = %s;" (Var.to_string var) n (Var.to_string value)
  | Offset_ref (var, n) -> Printf.sprintf "  Field(%s, 0) += %d;" (Var.to_string var) n
  | Array_set (arr, idx, value) ->
    Printf.sprintf
      "  Field(%s, Int_val(%s)) = %s;"
      (Var.to_string arr)
      (Var.to_string idx)
      (Var.to_string value)

and compile_expr _ctx expr =
  match expr with
  | Apply { f; args; exact } ->
    let dbg = if exact then "/* exact */" else "/* not exact */" in
    let args_str = String.concat ~sep:", " (List.map args ~f:Var.to_string) in
    Printf.sprintf
      "caml_call(%s, %d, %s) %s"
      (Var.to_string f)
      (List.length args)
      args_str
      dbg
  | Block (tag, fields, _, _) ->
    let fields_str =
      Array.to_list fields |> List.map ~f:Var.to_string |> String.concat ~sep:", "
    in
    Printf.sprintf "caml_alloc(%d, %d, %s)" (Array.length fields) tag fields_str
  | Field (var, n, _) -> Printf.sprintf "Field(%s, %d)" (Var.to_string var) n
  | Constant c -> compile_constant c
  | Prim (prim, args) -> compile_prim prim args
  | Closure _ -> assert false
  | Special special ->
    (match special with
     | Undefined ->
       "Val_unit /* aka undefined */" (* or some representation of OCaml's undefined *)
     | Alias_prim name -> sprintf "/* Alias primitive: %s */" name)

and compile_last ctx (last, _) =
  let compile_branch ctx pc args is_fresh =
    let renames = rename ctx pc args ~is_fresh in
    match is_fresh with
    | true -> renames
    | false -> Printf.sprintf "%s\ngoto block_%d;" renames pc
  in
  match last with
  | Return var -> Printf.sprintf "  return %s;" (Var.to_string var)
  | Raise (var, _) -> Printf.sprintf "  caml_raise(%s);" (Var.to_string var)
  | Stop -> "  return Val_unit;"
  | Branch (pc, args) ->
    let block, fresh = compile_block ctx pc in
    let branch = compile_branch ctx pc args fresh in
    Printf.sprintf "  %s\n%s" branch block
  | Cond (var, (pc1, args1), (pc2, args2)) ->
    let true_branch = compile_branch ctx pc1 args1 false in
    let false_branch = compile_branch ctx pc2 args2 false in
    let true_block = compile_block ctx pc1 |> fst in
    let false_block = compile_block ctx pc2 |> fst in
    Printf.sprintf
      "  if (Bool_val(%s)) { %s } else { %s }\n%s\n%s"
      (Var.to_string var)
      true_branch
      false_branch
      true_block
      false_block
  | Switch (var, arr) ->
    let cases =
      Array.mapi arr ~f:(fun i (pc, args) ->
        let branch = compile_branch ctx pc args false in
        let block = compile_block ctx pc |> fst in
        Printf.sprintf "    case %d: %s\n%s" i branch block)
      |> Array.to_list
      |> String.concat ~sep:"\n"
    in
    Printf.sprintf "  switch (Int_val(%s)) {\n%s\n  }" (Var.to_string var) cases
  | Pushtrap ((pc, args), _, (pc2, args2)) ->
    (* TODO: maybe(?) better fresh detection *)
    let branch = compile_branch ctx pc args false in
    let block = compile_block ctx pc |> fst in
    let handler = compile_branch ctx pc2 args2 false in
    let handler_block = compile_block ctx pc2 |> fst in
    Printf.sprintf
      "  %s\n\
       %s\n\
      \  caml_pushtrap();\n\
      \  if (caml_exception_pointer != NULL) { %s } else { %s }"
      branch
      block
      handler
      handler_block
  | Poptrap (pc, args) ->
    let branch = compile_branch ctx pc args false in
    let block = compile_block ctx pc |> fst in
    Printf.sprintf "  %s\n  %s\ncaml_poptrap();" branch block

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
  | Int32 _ | NativeInt _ ->
    assert false (* Should not be produced when compiling to Javascript *)

and compile_prim prim args =
  match prim, args with
  | Vectlength, [ x ] -> Printf.sprintf "Int_val(%s)" (compile_prim_arg x)
  | Array_get, [ arr; idx ] ->
    Printf.sprintf "Field(%s, Int_val(%s))" (compile_prim_arg arr) (compile_prim_arg idx)
  | Extern "%undefined", _ -> "/* undefined */ Val_unit"
  | Extern name, args -> compile_extern name args
  | Not, [ x ] -> Printf.sprintf "Val_bool(!Bool_val(%s))" (compile_prim_arg x)
  | IsInt, [ x ] -> Printf.sprintf "Val_bool(Is_long(%s))" (compile_prim_arg x)
  | Eq, [ x; y ] ->
    Printf.sprintf "Val_bool(%s == %s)" (compile_prim_arg x) (compile_prim_arg y)
  | Neq, [ x; y ] ->
    Printf.sprintf "Val_bool(%s != %s)" (compile_prim_arg x) (compile_prim_arg y)
  | Lt, [ x; y ] ->
    Printf.sprintf
      "Val_bool(Int_val(%s) < Int_val(%s))"
      (compile_prim_arg x)
      (compile_prim_arg y)
  | Le, [ x; y ] ->
    Printf.sprintf
      "Val_bool(Int_val(%s) <= Int_val(%s))"
      (compile_prim_arg x)
      (compile_prim_arg y)
  | Ult, [ x; y ] ->
    Printf.sprintf
      "Val_bool((uintnat)Int_val(%s) < (uintnat)Int_val(%s))"
      (compile_prim_arg x)
      (compile_prim_arg y)
  | _ -> Printf.sprintf "/* Unhandled primitive :O */"

and compile_extern name args =
  match name, args with
  | "%int_add", [ a; b ] ->
    Printf.sprintf
      "Val_int(Int_val(%s) + Int_val(%s))"
      (compile_prim_arg a)
      (compile_prim_arg b)
  | "%int_sub", [ a; b ] ->
    Printf.sprintf
      "Val_int(Int_val(%s) - Int_val(%s))"
      (compile_prim_arg a)
      (compile_prim_arg b)
  | "%int_mul", [ a; b ] ->
    Printf.sprintf
      "Val_int(Int_val(%s) * Int_val(%s))"
      (compile_prim_arg a)
      (compile_prim_arg b)
  | "%int_div", [ a; b ] ->
    Printf.sprintf
      "Val_int(Int_val(%s) / Int_val(%s))"
      (compile_prim_arg a)
      (compile_prim_arg b)
  | "%int_mod", [ a; b ] ->
    Printf.sprintf
      "Val_int(Int_val(%s) %% Int_val(%s))"
      (compile_prim_arg a)
      (compile_prim_arg b)
  (* TODO: these are direct, no need to use wrapper *)
  | "%direct_int_mul", [ a; b ] ->
    Printf.sprintf
      "Val_int(Int_val(%s) * Int_val(%s))"
      (compile_prim_arg a)
      (compile_prim_arg b)
  | "%direct_int_div", [ a; b ] ->
    Printf.sprintf
      "Val_int(Int_val(%s) / Int_val(%s))"
      (compile_prim_arg a)
      (compile_prim_arg b)
  | "%direct_int_mod", [ a; b ] ->
    Printf.sprintf
      "Val_int(Int_val(%s) %% Int_val(%s))"
      (compile_prim_arg a)
      (compile_prim_arg b)
  | "%int_and", [ a; b ] ->
    Printf.sprintf "(%s) & (%s)" (compile_prim_arg a) (compile_prim_arg b)
  | "%int_or", [ a; b ] ->
    Printf.sprintf "(%s) | (%s)" (compile_prim_arg a) (compile_prim_arg b)
  | "%int_xor", [ a; b ] ->
    Printf.sprintf "(%s) ^ (%s)" (compile_prim_arg a) (compile_prim_arg b)
  | "%int_lsl", [ a; b ] ->
    Printf.sprintf
      "Val_int(Int_val(%s) << Int_val(%s))"
      (compile_prim_arg a)
      (compile_prim_arg b)
  | "%int_lsr", [ a; b ] ->
    Printf.sprintf
      "Val_int((unsigned long)Int_val(%s) >> Int_val(%s))"
      (compile_prim_arg a)
      (compile_prim_arg b)
  | "%int_asr", [ a; b ] ->
    Printf.sprintf
      "Val_int(Int_val(%s) >> Int_val(%s))"
      (compile_prim_arg a)
      (compile_prim_arg b)
  | "%int_neg", [ a ] -> Printf.sprintf "Val_int(-Int_val(%s))" (compile_prim_arg a)
  | "%caml_format_int_special", [ a ] ->
    Printf.sprintf "caml_format_int(\"%s\", %s)" "%d" (compile_prim_arg a)
  | "%direct_obj_tag", [ a ] -> Printf.sprintf "Tag_val(%s)" (compile_prim_arg a)
  | "caml_array_unsafe_get", [ arr; idx ] ->
    Printf.sprintf "Field(%s, Int_val(%s))" (compile_prim_arg arr) (compile_prim_arg idx)
  | _ ->
    let args_str = List.map args ~f:compile_prim_arg |> String.concat ~sep:", " in
    Printf.sprintf "%s(%s)" name args_str

and compile_prim_arg = function
  | Pv var -> Var.to_string var
  | Pc const -> compile_constant const
;;

let f prog =
  let ctx =
    { prog; visited = Hash_set.create (module Int); closures = find_closures prog }
  in
  Hashtbl.iter ctx.closures ~f:(fun closure ->
    Printf.printf "value closure_%d(value* env);\n" closure.pc);
  Hashtbl.iter ctx.closures ~f:(compile_closure ctx);
  Printf.printf "int main() {\n  closure_%d(NULL);\n  return 0;\n}\n" prog.start
;;
