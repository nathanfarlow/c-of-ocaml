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

let let_ stack var exp =
  match Hashtbl.find stack (Var.idx var) with
  | Some idx -> sprintf "bp[%d] = %s;" idx exp
  | None -> sprintf "value %s = %s;" (Var.to_string var) exp
;;

let assign stack var exp =
  match Hashtbl.find stack (Var.idx var) with
  | Some idx -> sprintf "bp[%d] = %s;" idx exp
  | None -> sprintf "%s = %s;" (Var.to_string var) exp
;;

let get stack var =
  match Hashtbl.find stack (Var.idx var) with
  | Some idx -> sprintf "bp[%d]" idx
  | None -> Var.to_string var
;;

(** Rename continuation parameters to arguments using a sequence of statements like
    param1 = arg1;
    param2 = arg2;
    ... *)
let rename ctx stack pc args =
  let block = Addr.Map.find pc ctx.prog.blocks in
  List.map2_exn block.params args ~f:(fun param arg ->
    let arg = get stack arg in
    assign stack param arg)
  |> String.concat_lines
;;

let closure_name pc = sprintf "c%d" pc
let block_name pc = sprintf "b%d" pc

let get_all_vars ctx pc =
  let vars = Hash_set.create (module Int) in
  Code.traverse
    { fold = Code.fold_children }
    (fun pc () ->
      let block = Addr.Map.find pc ctx.prog.blocks in
      List.iter block.params ~f:(fun param -> Hash_set.add vars (Var.idx param));
      List.iter block.body ~f:(fun (instr, _) ->
        match instr with
        | Let (var, _) -> Hash_set.add vars (Var.idx var)
        | _ -> ()))
    pc
    ctx.prog.blocks
    ();
  Hash_set.to_list vars
  |> List.mapi ~f:(fun i v -> v, i)
  |> Hashtbl.of_alist_exn (module Int)
;;

let rec compile_closure ctx pc info =
  (* env is an array of values, where the first values are the captured
     variables, and the rest are the arguments to the closure *)
  let signature = sprintf "value %s(value* env)" (closure_name pc) in
  (* The blocks we've written down so far *)
  let visited = Hash_set.create (module Int) in
  (* Declare all block parameters at the top of the closure *)
  let stack = get_all_vars ctx pc in
  let env_assignments =
    List.mapi (info.free_vars @ info.params) ~f:(fun i v ->
      let_ stack v (sprintf "env[%d]" i))
    |> String.concat ~sep:"\n"
  in
  let renaming = rename ctx stack pc (snd info.cont) in
  let body = compile_block ctx visited stack pc in
  sprintf
    "%s {\nvalue bp[%d];\n%s\n%s\n%s\n}\n\n"
    signature
    (Hashtbl.length stack)
    env_assignments
    renaming
    body

and compile_block ctx visited stack pc =
  match Hash_set.mem visited pc with
  | true -> ""
  | false ->
    Hash_set.add visited pc;
    let block = Addr.Map.find pc ctx.prog.blocks in
    let body =
      List.map block.body ~f:(compile_instr ctx stack)
      @ [ compile_last ctx visited stack block.branch ]
      |> String.concat ~sep:"\n"
    in
    sprintf "%s:;\n%s" (block_name pc) body

and compile_instr ctx stack (instr, _) =
  match instr with
  | Let (var, Closure (params, (pc, _))) ->
    let var_name = get stack var in
    let free_vars = Hashtbl.find_exn ctx.closures pc |> fun c -> c.free_vars in
    let exp =
      sprintf
        "caml_alloc_closure(%s, %d, %d);"
        (closure_name pc)
        (List.length params)
        (List.length free_vars)
    in
    let assignment = let_ stack var exp in
    let env_assignments =
      List.map free_vars ~f:(fun fv -> sprintf "add_arg(%s, %s);" var_name (get stack fv))
      |> String.concat ~sep:"\n"
    in
    sprintf "%s\n%s" assignment env_assignments
  | Let (var, expr) -> let_ stack var (compile_expr ctx stack expr)
  | Assign (var1, var2) -> assign stack var1 (get stack var2)
  | Set_field (var, n, value) ->
    sprintf "Field(%s, %d) = %s;" (get stack var) n (get stack value)
  | Offset_ref (var, n) -> sprintf "Field(%s, 0) += %d;" (get stack var) n
  | Array_set (arr, idx, value) ->
    sprintf
      "Field(%s, Int_val(%s)) = %s;"
      (get stack arr)
      (get stack idx)
      (get stack value)

and compile_expr _ctx stack expr =
  match expr with
  (* TODO: leverage exact flag *)
  | Apply { f; args; exact = _ } ->
    let args_str = String.concat ~sep:", " (List.map args ~f:(get stack)) in
    sprintf "caml_call(%s, %d, %s)" (get stack f) (List.length args) args_str
  | Block (tag, fields, _, _) ->
    let fields_str =
      Array.to_list fields |> List.map ~f:(get stack) |> String.concat ~sep:", "
    in
    sprintf "caml_alloc(%d, %d, %s)" tag (Array.length fields) fields_str
  | Field (var, n) -> sprintf "Field(%s, %d)" (get stack var) n
  | Constant c -> compile_constant c
  | Prim (prim, args) -> compile_prim stack prim args
  | Closure _ -> assert false
  | Special special ->
    (match special with
     | Undefined -> "/* undefined */ Val_unit"
     | Alias_prim name -> sprintf "/* Alias primitive: %s */" name)

and compile_last ctx visited stack (last, _) =
  let compile_branch ctx pc args =
    let renames = rename ctx stack pc args in
    sprintf "%s\ngoto %s;" renames (block_name pc)
  in
  match last with
  | Return var -> sprintf "return %s;" (get stack var)
  | Raise (var, _) -> sprintf "caml_raise(%s);" (get stack var)
  | Stop -> "return Val_unit;"
  | Branch (pc, args) ->
    let block = compile_block ctx visited stack pc in
    let branch = compile_branch ctx pc args in
    sprintf "%s\n%s" branch block
  | Cond (var, (pc1, args1), (pc2, args2)) ->
    let true_branch = compile_branch ctx pc1 args1 in
    let false_branch = compile_branch ctx pc2 args2 in
    let true_block = compile_block ctx visited stack pc1 in
    let false_block = compile_block ctx visited stack pc2 in
    sprintf
      "if (Bool_val(%s)) { %s } else { %s }\n%s\n%s"
      (get stack var)
      true_branch
      false_branch
      true_block
      false_block
  | Switch (var, arr) ->
    let cases =
      Array.to_list arr
      |> List.mapi ~f:(fun i (pc, args) ->
        let branch = compile_branch ctx pc args in
        let block = compile_block ctx visited stack pc in
        sprintf "case %d: %s\n%s" i branch block)
      |> String.concat ~sep:"\n"
    in
    sprintf "switch (Int_val(%s)) {\n%s\n  }" (get stack var) cases
  (* TODO: implement exceptions *)
  | Pushtrap _ -> assert false
  | Poptrap _ -> assert false

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
    sprintf "caml_alloc(%d, %d, %s)" tag (Array.length elements) elements_str

and compile_prim stack prim args =
  match prim, args with
  | Vectlength, [ x ] -> sprintf "Int_val(%s)" (compile_prim_arg stack x)
  | Array_get, [ arr; idx ] ->
    sprintf
      "Field(%s, Int_val(%s))"
      (compile_prim_arg stack arr)
      (compile_prim_arg stack idx)
  | Extern "%undefined", _ -> "/* undefined */ Val_unit"
  | Extern name, args -> compile_extern stack name args
  | Not, [ x ] -> sprintf "Val_bool(!Bool_val(%s))" (compile_prim_arg stack x)
  | IsInt, [ x ] -> sprintf "Val_bool(Is_int(%s))" (compile_prim_arg stack x)
  | Eq, [ x; y ] ->
    sprintf "Val_bool(%s == %s)" (compile_prim_arg stack x) (compile_prim_arg stack y)
  | Neq, [ x; y ] ->
    sprintf "Val_bool(%s != %s)" (compile_prim_arg stack x) (compile_prim_arg stack y)
  | Lt, [ x; y ] ->
    sprintf
      "Val_bool(Int_val(%s) < Int_val(%s))"
      (compile_prim_arg stack x)
      (compile_prim_arg stack y)
  | Le, [ x; y ] ->
    sprintf
      "Val_bool(Int_val(%s) <= Int_val(%s))"
      (compile_prim_arg stack x)
      (compile_prim_arg stack y)
  | Ult, [ x; y ] ->
    sprintf
      "Val_bool((uintnat)Int_val(%s) < (uintnat)Int_val(%s))"
      (compile_prim_arg stack x)
      (compile_prim_arg stack y)
  | _ -> sprintf "/* Unhandled primitive :O */"

and compile_extern stack name args =
  (* Helper function for binary operators *)
  let bin_op op a b =
    sprintf
      "Val_int(Int_val(%s) %s Int_val(%s))"
      (compile_prim_arg stack a)
      op
      (compile_prim_arg stack b)
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
      (compile_prim_arg stack a)
      (compile_prim_arg stack b)
  | "%int_neg", [ a ] -> sprintf "Val_int(-Int_val(%s))" (compile_prim_arg stack a)
  | "%caml_format_int_special", [ a ] ->
    sprintf "caml_format_int(\"%s\", %s)" "%d" (compile_prim_arg stack a)
  | "%direct_obj_tag", [ a ] -> sprintf "Val_int(Tag_val(%s))" (compile_prim_arg stack a)
  | "caml_array_unsafe_get", [ arr; idx ] ->
    sprintf
      "Field(%s, Int_val(%s))"
      (compile_prim_arg stack arr)
      (compile_prim_arg stack idx)
  | _ ->
    let args_str = List.map args ~f:(compile_prim_arg stack) |> String.concat ~sep:", " in
    sprintf "%s(%s)" name args_str

and compile_prim_arg stack = function
  | Pv var -> get stack var
  | Pc const -> compile_constant const
;;

let f prog =
  let ctx = { prog; closures = find_closures prog } in
  let closures = Hashtbl.to_alist ctx.closures in
  List.map closures ~f:(fun (pc, _) -> sprintf "value %s(value* env);" (closure_name pc))
  @ List.map closures ~f:(fun (pc, c) -> compile_closure ctx pc c)
  @ [ sprintf "int main() { %s(NULL); return 0; }" (closure_name prog.start) ]
  |> String.concat ~sep:"\n"
;;
