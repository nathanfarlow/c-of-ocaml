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
  ; strings : Hash_set.M(String).t
  }

let find_closures program =
  let free_vars = Freevars.f program in
  fold_closures program (fun _ params cont acc -> (params, cont) :: acc) []
  |> List.map ~f:(fun (params, ((pc, _) as cont)) ->
    let free_vars =
      Addr.Map.find_opt pc free_vars |> Option.value_map ~default:[] ~f:Var.Set.elements
    in
    pc, { free_vars; params; cont })
  |> Hashtbl.of_alist_exn (module Int)
;;

let var_name var = "v_" ^ Var.to_string var
let closure_name pc = sprintf "c%d" pc
let block_name pc = sprintf "b%d" pc
let string_name s = sprintf "s_%d" (String.hash s)

let get stack var =
  Hashtbl.find stack (Var.idx var)
  |> Option.value_map ~default:(var_name var) ~f:(sprintf "bp[%d]")
;;

let let_ stack var exp =
  Hashtbl.find stack (Var.idx var)
  |> Option.value_map
       ~default:(sprintf "value %s = %s;" (var_name var) exp)
       ~f:(fun idx -> sprintf "bp[%d] = %s;" idx exp)
;;

let assign stack var exp =
  Hashtbl.find stack (Var.idx var)
  |> Option.value_map
       ~default:(sprintf "%s = %s;" (var_name var) exp)
       ~f:(fun idx -> sprintf "bp[%d] = %s;" idx exp)
;;

let rename ctx stack pc args =
  (Addr.Map.find pc ctx.prog.blocks).params
  |> List.map2_exn ~f:(fun arg param -> assign stack param (get stack arg)) args
  |> String.concat_lines
;;

let get_all_vars ctx pc =
  let vars = Hash_set.create (module Int) in
  Code.traverse
    { fold = Code.fold_children }
    (fun pc () ->
       let block = Addr.Map.find pc ctx.prog.blocks in
       List.iter block.params ~f:(fun p -> Hash_set.add vars (Var.idx p));
       List.iter block.body ~f:(function
         | Let (v, _), _ -> Hash_set.add vars (Var.idx v)
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
  [ signature
  ; "{"
  ; sprintf "memset(bp, 1, %d * sizeof(value));" (Hashtbl.length stack)
  ; sprintf "sp += %d;" (Hashtbl.length stack)
  ; env_assignments
  ; renaming
  ; body
  ; "}"
  ]
  |> String.concat ~sep:"\n"

and closure_free_vars ctx pc = (Hashtbl.find_exn ctx.closures pc).free_vars

and compile_closure_alloc ctx stack var params pc =
  let free_vars = closure_free_vars ctx pc in
  sprintf
    "caml_alloc_closure(%s, %d, %d);"
    (closure_name pc)
    (List.length params)
    (List.length free_vars)
  |> let_ stack var

and compile_closure_fill ctx stack var pc =
  let v = get stack var in
  closure_free_vars ctx pc
  |> List.map ~f:(fun fv -> sprintf "add_arg(%s, %s);" v (get stack fv))
  |> String.concat ~sep:"\n"

and get_closure_info = function
  | Let (var, Closure (params, (pc, _))), _ -> Some (var, params, pc)
  | _ -> None

and compile_block ctx visited stack pc =
  if Hash_set.mem visited pc
  then ""
  else (
    Hash_set.add visited pc;
    let block = Addr.Map.find pc ctx.prog.blocks in
    let rec process acc = function
      | [] -> List.rev acc
      | instrs ->
        let closures, rest =
          List.split_while instrs ~f:(fun i -> Option.is_some (get_closure_info i))
        in
        (match List.filter_map closures ~f:get_closure_info with
         | [] ->
           (match rest with
            | [] -> List.rev acc
            | i :: rest' -> process (compile_instr ctx stack i :: acc) rest')
         | infos ->
           let allocs =
             List.map infos ~f:(fun (v, p, pc) -> compile_closure_alloc ctx stack v p pc)
           in
           let fills =
             List.map infos ~f:(fun (v, _, pc) -> compile_closure_fill ctx stack v pc)
           in
           process (List.rev_append (allocs @ fills) acc) rest)
    in
    sprintf
      "%s:;\n%s"
      (block_name pc)
      (process [] block.body @ [ compile_last ctx visited stack block.branch ]
       |> String.concat ~sep:"\n"))

and compile_instr ctx stack (instr, _) =
  match instr with
  | Let (var, Closure (params, (pc, _))) ->
    let alloc = compile_closure_alloc ctx stack var params pc in
    let fill = compile_closure_fill ctx stack var pc in
    sprintf "%s\n%s" alloc fill
  | Let (var, expr) ->
    let let_ = let_ stack var (compile_expr ctx stack expr) in
    (match expr with
     | Constant (Tuple _) ->
       [ "block_gc = 1;"; let_; "block_gc = 0;" ] |> String.concat ~sep:"\n"
     | _ -> let_)
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

and compile_expr ctx stack expr =
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
  | Constant c -> compile_constant ctx c
  | Prim (prim, args) -> compile_prim ctx stack prim args
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

and add_string ctx s =
  Hash_set.add ctx.strings s;
  string_name s

and compile_constant ctx = function
  | Int i -> sprintf "Val_int(%ld)" i
  | Float f -> sprintf "caml_copy_double(%f)" f
  | String s -> add_string ctx s
  | NativeString (Byte s | Utf (Utf8 s)) -> add_string ctx s
  | Int64 i -> sprintf "caml_copy_int64(%Ld)" i
  | Float_array fa ->
    sprintf
      "caml_alloc_float_array(%d, (double[]){%s})"
      (Array.length fa)
      (Array.to_list fa |> List.map ~f:(sprintf "%f") |> String.concat ~sep:", ")
  | Tuple (tag, elts, _) ->
    sprintf
      "caml_alloc(%d, %d, %s)"
      tag
      (Array.length elts)
      (Array.to_list elts |> List.map ~f:(compile_constant ctx) |> String.concat ~sep:", ")

and compile_prim ctx stack prim args =
  match prim, args with
  | Vectlength, [ x ] -> sprintf "Int_val(%s)" (compile_prim_arg ctx stack x)
  | Array_get, [ arr; idx ] ->
    sprintf
      "Field(%s, Int_val(%s))"
      (compile_prim_arg ctx stack arr)
      (compile_prim_arg ctx stack idx)
  | Extern "%undefined", _ -> "/* undefined */ Val_unit"
  | Extern name, args -> compile_extern ctx stack name args
  | Not, [ x ] -> sprintf "Val_bool(!Bool_val(%s))" (compile_prim_arg ctx stack x)
  | IsInt, [ x ] -> sprintf "Val_bool(Is_int(%s))" (compile_prim_arg ctx stack x)
  | Eq, [ x; y ] ->
    sprintf
      "Val_bool(%s == %s)"
      (compile_prim_arg ctx stack x)
      (compile_prim_arg ctx stack y)
  | Neq, [ x; y ] ->
    sprintf
      "Val_bool(%s != %s)"
      (compile_prim_arg ctx stack x)
      (compile_prim_arg ctx stack y)
  | Lt, [ x; y ] ->
    sprintf
      "Val_bool(Int_val(%s) < Int_val(%s))"
      (compile_prim_arg ctx stack x)
      (compile_prim_arg ctx stack y)
  | Le, [ x; y ] ->
    sprintf
      "Val_bool(Int_val(%s) <= Int_val(%s))"
      (compile_prim_arg ctx stack x)
      (compile_prim_arg ctx stack y)
  | Ult, [ x; y ] ->
    sprintf
      "Val_bool((uintnat)Int_val(%s) < (uintnat)Int_val(%s))"
      (compile_prim_arg ctx stack x)
      (compile_prim_arg ctx stack y)
  | _ -> sprintf "/* Unhandled primitive :O */"

and int_binops =
  [ "%int_add", "+"
  ; "%int_sub", "-"
  ; "%int_mul", "*"
  ; "%int_div", "/"
  ; "%int_mod", "%"
  ; "%direct_int_mul", "*"
  ; "%direct_int_div", "/"
  ; "%direct_int_mod", "%"
  ; "%int_and", "&"
  ; "%int_or", "|"
  ; "%int_xor", "^"
  ; "%int_lsl", "<<"
  ; "%int_asr", ">>"
  ]

and compile_extern ctx stack name args =
  let arg = compile_prim_arg ctx stack in
  match List.Assoc.find int_binops ~equal:String.equal name, args with
  | Some op, [ a; b ] -> sprintf "Val_int(Int_val(%s) %s Int_val(%s))" (arg a) op (arg b)
  | None, _ ->
    (match name, args with
     | "%int_lsr", [ a; b ] ->
       sprintf "Val_int((unatint)Int_val(%s) >> Int_val(%s))" (arg a) (arg b)
     | "%int_neg", [ a ] -> sprintf "Val_int(-Int_val(%s))" (arg a)
     | "%caml_format_int_special", [ a ] -> sprintf "caml_format_int(\"%%d\", %s)" (arg a)
     | "%direct_obj_tag", [ a ] -> sprintf "Val_int(Tag_val(%s))" (arg a)
     | "caml_array_unsafe_get", [ arr; idx ] ->
       sprintf "Field(%s, Int_val(%s))" (arg arr) (arg idx)
     | _ -> sprintf "%s(%s)" name (List.map args ~f:arg |> String.concat ~sep:", "))
  | Some _, _ -> assert false

and compile_prim_arg ctx stack = function
  | Pv var -> get stack var
  | Pc const -> compile_constant ctx const
;;

let f prog =
  let ctx =
    { prog; closures = find_closures prog; strings = Hash_set.create (module String) }
  in
  let closures = Hashtbl.to_alist ctx.closures in
  let closure_bodies = List.map closures ~f:(fun (pc, c) -> compile_closure ctx pc c) in
  let strings = Hash_set.to_list ctx.strings in
  List.map closures ~f:(fun (pc, _) -> sprintf "value %s(value* env);" (closure_name pc))
  @ List.map strings ~f:(fun s -> sprintf "value %s;" (string_name s))
  @ closure_bodies
  @ [ "int main() {" ]
  @ List.concat_map strings ~f:(fun s ->
    [ sprintf "%s = caml_copy_string(\"%s\");" (string_name s) s
    ; sprintf "*(sp++) = %s;" (string_name s)
    ])
  @ [ sprintf "bp = sp; %s(NULL); return 0;}" (closure_name prog.start) ]
  |> String.concat ~sep:"\n"
;;
