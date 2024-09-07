open Core
open! Js_of_ocaml_compiler
open Code

type closure_info =
  { free_vars : Var.t list
  ; cont : cont
  ; params : Var.t list
  }

type ctx =
  { prog : program
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
    pc, { free_vars; params; cont })
  |> Hashtbl.of_alist_exn (module Int)
;;

(** Rename continuation parameters to arguments using a sequence of statements like
    param1 = arg1;
    param2 = arg2;
    ... *)
let rename ctx pc args =
  let block = Addr.Map.find pc ctx.prog.blocks in
  List.map2_exn block.params args ~f:(fun param arg ->
    let param = Var.to_string param in
    let arg = Var.to_string arg in
    sprintf "%s = %s;" param arg)
  |> String.concat_lines
;;

let closure_name pc = sprintf "c%d" pc
let block_name pc = sprintf "b%d" pc

(** Get the parameters of the block and all its children, recursively. *)
let get_block_params ctx pc =
  Code.traverse
    { fold = Code.fold_children }
    (fun pc acc ->
      let block = Addr.Map.find pc ctx.prog.blocks in
      List.fold block.params ~init:acc ~f:(fun acc param ->
        Set.add acc (Var.to_string param)))
    pc
    ctx.prog.blocks
    (Set.empty (module String))
;;

let rec compile_closure ctx pc info =
  (* env is an array of values, where the first values are the captured
     variables, and the rest are the arguments to the closure *)
  let signature = sprintf "value %s(value* env)" (closure_name pc) in
  (* The blocks we've written down so far *)
  let visited = Hash_set.create (module Int) in
  (* Declare all block parameters at the top of the closure *)
  let var_decls =
    get_block_params ctx pc
    |> Set.to_list
    |> List.map ~f:(fun v -> sprintf "value %s;" v)
    |> String.concat ~sep:"\n"
  in
  let env_assignments =
    List.mapi (info.free_vars @ info.params) ~f:(fun i v ->
      sprintf "value %s = env[%d];" (Var.to_string v) i)
    |> String.concat ~sep:"\n"
  in
  let renaming = rename ctx pc (snd info.cont) in
  let body = compile_block ctx visited pc |> fst in
  sprintf "%s {\n%s\n%s\n%s\n%s\n}\n\n" signature var_decls env_assignments renaming body

and compile_block ctx visited pc =
  if Hash_set.mem visited pc
  then "", false
  else (
    Hash_set.add visited pc;
    let block = Addr.Map.find pc ctx.prog.blocks in
    let body =
      List.map block.body ~f:(compile_instr ctx)
      @ [ compile_last ctx visited block.branch ]
      |> String.concat ~sep:"\n"
    in
    sprintf "%s:\n%s" (block_name pc) body, true)

and compile_instr ctx (instr, _) =
  match instr with
  | Let (var, Closure (params, (pc, _))) ->
    let var_name = Var.to_string var in
    let free_vars = Hashtbl.find_exn ctx.closures pc |> fun c -> c.free_vars in
    let assignment =
      sprintf
        "value %s = caml_alloc_closure(%s, %d, %d);"
        var_name
        (closure_name pc)
        (List.length params)
        (List.length free_vars)
    in
    let env_assignments =
      List.map free_vars ~f:(fun fv ->
        sprintf "add_arg(%s, %s);" var_name (Var.to_string fv))
      |> String.concat ~sep:"\n"
    in
    sprintf "%s\n%s" assignment env_assignments
  | Let (var, expr) ->
    sprintf "value %s = %s;" (Var.to_string var) (compile_expr ctx expr)
  | Assign (var1, var2) -> sprintf "%s = %s;" (Var.to_string var1) (Var.to_string var2)
  | Set_field (var, n, value) ->
    sprintf "Field(%s, %d) = %s;" (Var.to_string var) n (Var.to_string value)
  | Offset_ref (var, n) -> sprintf "Field(%s, 0) += %d;" (Var.to_string var) n
  | Array_set (arr, idx, value) ->
    sprintf
      "Field(%s, Int_val(%s)) = %s;"
      (Var.to_string arr)
      (Var.to_string idx)
      (Var.to_string value)

and compile_expr _ctx expr =
  match expr with
  | Apply { f; args; exact } ->
    let dbg = if exact then "/* exact */" else "/* not exact */" in
    let args_str = String.concat ~sep:", " (List.map args ~f:Var.to_string) in
    sprintf "caml_call(%s, %d, %s) %s" (Var.to_string f) (List.length args) args_str dbg
  | Block (tag, fields, _, _) ->
    let fields_str =
      Array.to_list fields |> List.map ~f:Var.to_string |> String.concat ~sep:", "
    in
    sprintf "caml_alloc(%d, %d, %s)" (Array.length fields) tag fields_str
  | Field (var, n) -> sprintf "Field(%s, %d)" (Var.to_string var) n
  | Constant c -> compile_constant c
  | Prim (prim, args) -> compile_prim prim args
  | Closure _ -> assert false
  | Special special ->
    (match special with
     | Undefined ->
       "Val_unit /* aka undefined */" (* or some representation of OCaml's undefined *)
     | Alias_prim name -> sprintf "/* Alias primitive: %s */" name)

and compile_last ctx visited (last, _) =
  let compile_branch ctx pc args is_fresh =
    let renames = rename ctx pc args in
    match is_fresh with
    | true -> renames
    | false -> sprintf "%s\ngoto %s;" renames (block_name pc)
  in
  match last with
  | Return var -> sprintf "return %s;" (Var.to_string var)
  | Raise (var, _) -> sprintf "caml_raise(%s);" (Var.to_string var)
  | Stop -> "return Val_unit;"
  | Branch (pc, args) ->
    let block, fresh = compile_block ctx visited pc in
    let branch = compile_branch ctx pc args fresh in
    sprintf "%s\n%s" branch block
  | Cond (var, (pc1, args1), (pc2, args2)) ->
    let true_branch = compile_branch ctx pc1 args1 false in
    let false_branch = compile_branch ctx pc2 args2 false in
    let true_block = compile_block ctx visited pc1 |> fst in
    let false_block = compile_block ctx visited pc2 |> fst in
    sprintf
      "if (Bool_val(%s)) { %s } else { %s }\n%s\n%s"
      (Var.to_string var)
      true_branch
      false_branch
      true_block
      false_block
  | Switch (var, arr) ->
    let cases =
      Array.to_list arr
      |> List.mapi ~f:(fun i (pc, args) ->
        let branch = compile_branch ctx pc args false in
        let block = compile_block ctx visited pc |> fst in
        sprintf "case %d: %s\n%s" i branch block)
      |> String.concat ~sep:"\n"
    in
    sprintf "switch (Int_val(%s)) {\n%s\n  }" (Var.to_string var) cases
  | Pushtrap ((pc, args), _, (pc2, args2)) ->
    let branch = compile_branch ctx pc args false in
    let block = compile_block ctx visited pc |> fst in
    let handler = compile_branch ctx pc2 args2 false in
    let handler_block = compile_block ctx visited pc2 |> fst in
    sprintf
      "%s\n%s\ncaml_pushtrap();\nif (caml_exception_pointer != NULL) { %s } else { %s }"
      branch
      block
      handler
      handler_block
  | Poptrap (pc, args) ->
    let branch = compile_branch ctx pc args false in
    let block = compile_block ctx visited pc |> fst in
    sprintf "%s\n%s\ncaml_poptrap();" branch block

and compile_constant c =
  match c with
  | Int i -> sprintf "Val_int(%ld)" i
  | Float f -> sprintf "caml_copy_double(%f)" f
  | String s -> sprintf "caml_copy_string(\"%s\")" s
  | NativeString ns ->
    (match ns with
     | Byte s -> sprintf "caml_copy_string(\"%s\")" s
     | Utf (Utf8 s) -> sprintf "caml_copy_string(\"%s\")" s)
  | Int64 i -> sprintf "caml_copy_int64(%Ld)" i
  | Float_array fa ->
    let elements =
      Array.to_list fa |> List.map ~f:(sprintf "%f") |> String.concat ~sep:", "
    in
    sprintf "caml_alloc_float_array(%d, (double[]){%s})" (Array.length fa) elements
  | Tuple (tag, elements, _) ->
    let elements_str =
      Array.to_list elements |> List.map ~f:compile_constant |> String.concat ~sep:", "
    in
    sprintf "caml_alloc(%d, %d, %s)" (Array.length elements) tag elements_str

and compile_prim prim args =
  match prim, args with
  | Vectlength, [ x ] -> sprintf "Int_val(%s)" (compile_prim_arg x)
  | Array_get, [ arr; idx ] ->
    sprintf "Field(%s, Int_val(%s))" (compile_prim_arg arr) (compile_prim_arg idx)
  | Extern "%undefined", _ -> "/* undefined */ Val_unit"
  | Extern name, args -> compile_extern name args
  | Not, [ x ] -> sprintf "Val_bool(!Bool_val(%s))" (compile_prim_arg x)
  | IsInt, [ x ] -> sprintf "Val_bool(Is_int(%s))" (compile_prim_arg x)
  | Eq, [ x; y ] -> sprintf "Val_bool(%s == %s)" (compile_prim_arg x) (compile_prim_arg y)
  | Neq, [ x; y ] ->
    sprintf "Val_bool(%s != %s)" (compile_prim_arg x) (compile_prim_arg y)
  | Lt, [ x; y ] ->
    sprintf
      "Val_bool(Int_val(%s) < Int_val(%s))"
      (compile_prim_arg x)
      (compile_prim_arg y)
  | Le, [ x; y ] ->
    sprintf
      "Val_bool(Int_val(%s) <= Int_val(%s))"
      (compile_prim_arg x)
      (compile_prim_arg y)
  | Ult, [ x; y ] ->
    sprintf
      "Val_bool((uintnat)Int_val(%s) < (uintnat)Int_val(%s))"
      (compile_prim_arg x)
      (compile_prim_arg y)
  | _ -> sprintf "/* Unhandled primitive :O */"

and compile_extern name args =
  (* Helper function for binary operators *)
  let bin_op op a b =
    sprintf
      "Val_int(Int_val(%s) %s Int_val(%s))"
      (compile_prim_arg a)
      op
      (compile_prim_arg b)
  in
  match name, args with
  | "%int_add", [ a; b ] -> bin_op "+" a b
  | "%int_sub", [ a; b ] -> bin_op "-" a b
  | "%int_mul", [ a; b ] -> bin_op "*" a b
  | "%int_div", [ a; b ] -> bin_op "/" a b
  | "%int_mod", [ a; b ] -> bin_op "%" a b
  | "%direct_int_mul", [ a; b ] -> bin_op "*" a b
  | "%direct_int_div", [ a; b ] -> bin_op "/" a b
  | "%direct_int_mod", [ a; b ] -> bin_op "%" a b
  | "%int_and", [ a; b ] -> bin_op "&" a b
  | "%int_or", [ a; b ] -> bin_op "|" a b
  | "%int_xor", [ a; b ] -> bin_op "^" a b
  | "%int_lsl", [ a; b ] -> bin_op "<<" a b
  | "%int_asr", [ a; b ] -> bin_op ">>" a b
  | "%int_lsr", [ a; b ] ->
    sprintf
      "Val_int((unatint)Int_val(%s) >> Int_val(%s))"
      (compile_prim_arg a)
      (compile_prim_arg b)
  | "%int_neg", [ a ] -> sprintf "Val_int(-Int_val(%s))" (compile_prim_arg a)
  | "%caml_format_int_special", [ a ] ->
    sprintf "caml_format_int(\"%s\", %s)" "%d" (compile_prim_arg a)
  | "%direct_obj_tag", [ a ] -> sprintf "Val_int(Tag_val(%s))" (compile_prim_arg a)
  | "caml_array_unsafe_get", [ arr; idx ] ->
    sprintf "Field(%s, Int_val(%s))" (compile_prim_arg arr) (compile_prim_arg idx)
  | _ ->
    let args_str = List.map args ~f:compile_prim_arg |> String.concat ~sep:", " in
    sprintf "%s(%s)" name args_str

and compile_prim_arg = function
  | Pv var -> Var.to_string var
  | Pc const -> compile_constant const
;;

let f prog =
  let ctx = { prog; closures = find_closures prog } in
  let closures = Hashtbl.to_alist ctx.closures in
  List.map closures ~f:(fun (pc, _) -> sprintf "value closure_%d(value* env);" pc)
  @ List.map closures ~f:(fun (pc, c) -> compile_closure ctx pc c)
  @ [ sprintf "int main() { %s(NULL); return 0; }" (closure_name prog.start) ]
  |> String.concat ~sep:"\n"
;;
