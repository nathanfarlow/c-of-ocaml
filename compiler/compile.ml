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

let find_closures prog =
  let free = Freevars.f prog in
  fold_closures prog (fun _ params cont acc -> (params, cont) :: acc) []
  |> List.map ~f:(fun (params, ((pc, _) as cont)) ->
    let free_vars =
      Addr.Map.find_opt pc free |> Option.value_map ~default:[] ~f:Var.Set.elements
    in
    pc, { free_vars; params; cont })
  |> Hashtbl.of_alist_exn (module Int)
;;

let vname v = [%string "v_%{Var.to_string v}"]
let cname pc = [%string "c%{pc#Int}"]
let bname pc = [%string "b%{pc#Int}"]
let sname s = [%string "s_%{String.hash s#Int}"]
let slot stack v = Hashtbl.find stack (Var.idx v)

let get stack v =
  slot stack v
  |> Option.value_map ~default:(vname v) ~f:(fun i -> [%string "bp[%{i#Int}]"])
;;

let set ?(decl = false) stack v exp =
  slot stack v
  |> Option.value_map
       ~f:(fun i -> [%string "bp[%{i#Int}] = %{exp};"])
       ~default:
         (if decl
          then [%string "value %{vname v} = %{exp};"]
          else [%string "%{vname v} = %{exp};"])
;;

let rename_id = ref 0

let rename ctx stack pc args =
  let id = !rename_id in
  incr rename_id;
  let params = (Addr.Map.find pc ctx.prog.blocks).params in
  let t i = [%string "t%{id#Int}_%{i#Int}"] in
  List.mapi args ~f:(fun i a -> [%string "value %{t i} = %{get stack a};"])
  @ List.mapi params ~f:(fun i p -> set stack p (t i))
  |> String.concat_lines
;;

let collect_vars ctx pc =
  let vars = Hash_set.create (module Int) in
  let add v = Hash_set.add vars (Var.idx v) in
  Code.traverse
    { fold = Code.fold_children }
    (fun pc () ->
       let b = Addr.Map.find pc ctx.prog.blocks in
       List.iter b.params ~f:add;
       List.iter b.body ~f:(function
         | Let (v, _), _ -> add v
         | _ -> ()))
    pc
    ctx.prog.blocks
    ();
  Hash_set.to_list vars
  |> List.mapi ~f:(fun i v -> v, i)
  |> Hashtbl.of_alist_exn (module Int)
;;

let rec compile_closure ctx pc info =
  let stack = collect_vars ctx pc in
  let visited = Hash_set.create (module Int) in
  let n = Hashtbl.length stack in
  [ [%string "value %{cname pc}(value* env)"]
  ; "{"
  ; [%string
      "check_stack(%{n#Int}); memset(bp, 1, %{n#Int} * sizeof(value)); sp += %{n#Int};"]
  ; List.mapi (info.free_vars @ info.params) ~f:(fun i v ->
      set ~decl:true stack v [%string "env[%{i#Int}]"])
    |> String.concat_lines
  ; rename ctx stack pc (snd info.cont)
  ; compile_block ctx visited stack pc
  ; "}"
  ]
  |> String.concat_lines

and compile_block ctx visited stack pc =
  if Hash_set.mem visited pc
  then ""
  else (
    Hash_set.add visited pc;
    let { body; branch; _ } = Addr.Map.find pc ctx.prog.blocks in
    let rec go acc = function
      | [] -> List.rev acc
      | instrs ->
        let cls, rest =
          List.split_while instrs ~f:(fun i -> Option.is_some (closure_of i))
        in
        (match List.filter_map cls ~f:closure_of with
         | [] ->
           (match rest with
            | [] -> List.rev acc
            | i :: rest -> go (compile_instr ctx stack i :: acc) rest)
         | infos ->
           let allocs =
             List.map infos ~f:(fun (v, p, pc) ->
               let fv = (Hashtbl.find_exn ctx.closures pc).free_vars in
               set
                 ~decl:true
                 stack
                 v
                 [%string
                   "caml_alloc_closure(%{cname pc}, %{List.length p#Int}, %{List.length \
                    fv#Int});"])
           in
           let fills =
             List.map infos ~f:(fun (v, _, pc) ->
               let fv = (Hashtbl.find_exn ctx.closures pc).free_vars in
               List.map fv ~f:(fun f ->
                 [%string "add_arg(%{get stack v}, %{get stack f});"])
               |> String.concat_lines)
           in
           go (List.rev_append (allocs @ fills) acc) rest)
    in
    let instrs =
      go [] body @ [ compile_last ctx visited stack branch ] |> String.concat_lines
    in
    [%string "%{bname pc}:;\n%{instrs}"])

and closure_of = function
  | Let (v, Closure (p, (pc, _))), _ -> Some (v, p, pc)
  | _ -> None

and compile_instr ctx stack (instr, _) =
  let g = get stack in
  match instr with
  | Let (v, Closure (p, (pc, _))) ->
    let fv = (Hashtbl.find_exn ctx.closures pc).free_vars in
    let alloc =
      set
        ~decl:true
        stack
        v
        [%string
          "caml_alloc_closure(%{cname pc}, %{List.length p#Int}, %{List.length fv#Int});"]
    in
    let fills = List.map fv ~f:(fun f -> [%string "add_arg(%{g v}, %{g f});"]) in
    alloc :: fills |> String.concat_lines
  | Let (v, Constant c) ->
    let preamble, expr = compile_const ctx c in
    String.concat_lines (preamble @ [ set ~decl:true stack v expr ])
  | Let (v, e) -> set ~decl:true stack v (compile_expr ctx stack e)
  | Assign (v1, v2) -> set stack v1 (g v2)
  | Set_field (v, n, x) -> [%string "Field(%{g v}, %{n#Int}) = %{g x};"]
  | Offset_ref (v, n) -> [%string "Field(%{g v}, 0) += %{n#Int};"]
  | Array_set (a, i, x) -> [%string "Field(%{g a}, Int_val(%{g i})) = %{g x};"]

and compile_expr ctx stack = function
  | Apply { f; args; _ } ->
    let a = List.map args ~f:(get stack) |> String.concat ~sep:", " in
    [%string "caml_call(%{get stack f}, %{List.length args#Int}, %{a})"]
  | Block (tag, fields, _, _) ->
    let fs =
      Array.map fields ~f:(get stack) |> Array.to_list |> String.concat ~sep:", "
    in
    [%string "caml_alloc(%{tag#Int}, %{Array.length fields#Int}, %{fs})"]
  | Field (v, n) -> [%string "Field(%{get stack v}, %{n#Int})"]
  | Constant c -> snd (compile_const ctx c)
  | Prim (p, args) -> compile_prim ctx stack p args
  | Closure _ -> assert false
  | Special Undefined -> "Val_unit"
  | Special (Alias_prim _) -> "Val_unit"

and compile_last ctx visited stack (last, _) =
  let g = get stack in
  let branch pc args = [%string "%{rename ctx stack pc args}\ngoto %{bname pc};"] in
  match last with
  | Return v -> [%string "return %{g v};"]
  | Raise (v, _) -> [%string "caml_raise(%{g v});"]
  | Stop -> "return Val_unit;"
  | Branch (pc, args) ->
    let br = branch pc args in
    let block = compile_block ctx visited stack pc in
    [%string "%{br}\n%{block}"]
  | Cond (v, (pc1, a1), (pc2, a2)) ->
    let then_branch = branch pc1 a1 in
    let else_branch = branch pc2 a2 in
    let block1 = compile_block ctx visited stack pc1 in
    let block2 = compile_block ctx visited stack pc2 in
    [%string
      "if (Bool_val(%{g v})) { %{then_branch} } else { %{else_branch} }\n\
       %{block1}\n\
       %{block2}"]
  | Switch (v, arr) ->
    let cases =
      Array.mapi arr ~f:(fun i (pc, args) ->
        let br = branch pc args in
        let block = compile_block ctx visited stack pc in
        [%string "case %{i#Int}: %{br}\n%{block}"])
    in
    let cases_str = Array.to_list cases |> String.concat_lines in
    [%string "switch (Int_val(%{g v})) {\n%{cases_str}\n}"]
  | Pushtrap _ | Poptrap _ -> assert false

and const_allocates = function
  | Int _ | String _ | NativeString _ -> false
  | Float _ | Int64 _ | Float_array _ | Tuple _ -> true

(* Returns (preamble_statements, expression) *)
and compile_const ctx c =
  match c with
  | Int i -> [], [%string "Val_int(%{i#Int32}L)"]
  | Float f -> [], [%string "caml_copy_double(%{f#Float})"]
  | String s | NativeString (Byte s | Utf (Utf8 s)) ->
    Hash_set.add ctx.strings s;
    [], sname s
  | Int64 i -> [], [%string "caml_copy_int64(%{i#Int64}LL)"]
  | Float_array fa ->
    let elts =
      Array.map fa ~f:(fun f -> [%string "%{f#Float}"])
      |> Array.to_list
      |> String.concat ~sep:", "
    in
    [], [%string "caml_alloc_float_array(%{Array.length fa#Int}, (double[]){%{elts}})"]
  | Tuple (tag, elts, _) ->
    (* If multiple elements allocate, we must push each to the stack to protect from GC *)
    let n_alloc = Array.count elts ~f:const_allocates in
    let need_stack = n_alloc > 1 in
    let check = if need_stack then [ [%string "check_stack(%{n_alloc#Int});"] ] else [] in
    let _, preambles, args =
      Array.fold elts ~init:(0, check, []) ~f:(fun (alloc_idx, preambles, args) e ->
        let preamble, expr = compile_const ctx e in
        if need_stack && const_allocates e
        then (
          let push = [ [%string "sp[0] = %{expr};"]; "sp++;" ] in
          ( alloc_idx + 1
          , preambles @ preamble @ push
          , args @ [ [%string "sp[%{alloc_idx#Int}]"] ] ))
        else alloc_idx, preambles @ preamble, args @ [ expr ])
    in
    let preamble =
      if need_stack then preambles @ [ [%string "sp -= %{n_alloc#Int};"] ] else preambles
    in
    let args_str = String.concat args ~sep:", " in
    preamble, [%string "caml_alloc(%{tag#Int}, %{Array.length elts#Int}, %{args_str})"]

and compile_prim ctx stack prim args =
  let arg = function
    | Pv v -> get stack v
    | Pc c -> snd (compile_const ctx c)
  in
  let a, b =
    match args with
    | [ x ] -> arg x, ""
    | [ x; y ] -> arg x, arg y
    | _ -> "", ""
  in
  match prim, args with
  | Vectlength, [ x ] -> [%string "Int_val(%{arg x})"]
  | Array_get, [ arr; i ] -> [%string "Field(%{arg arr}, Int_val(%{arg i}))"]
  | Extern "%undefined", _ -> "Val_unit"
  | Extern name, _ -> compile_extern name a b args arg
  | Not, [ _ ] -> [%string "Val_bool(!Bool_val(%{a}))"]
  | IsInt, [ _ ] -> [%string "Val_bool(Is_int(%{a}))"]
  | Eq, [ _; _ ] -> [%string "Val_bool(%{a} == %{b})"]
  | Neq, [ _; _ ] -> [%string "Val_bool(%{a} != %{b})"]
  | Lt, [ _; _ ] -> [%string "Val_bool(Int_val(%{a}) < Int_val(%{b}))"]
  | Le, [ _; _ ] -> [%string "Val_bool(Int_val(%{a}) <= Int_val(%{b}))"]
  | Ult, [ _; _ ] -> [%string "Val_bool((uintnat)Int_val(%{a}) < (uintnat)Int_val(%{b}))"]
  | _ -> "/* unhandled */"

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

and compile_extern name a b args arg =
  match List.Assoc.find int_binops ~equal:String.equal name with
  | Some op -> [%string "Val_int(Int_val(%{a}) %{op} Int_val(%{b}))"]
  | None ->
    (match name with
     | "%int_lsr" -> [%string "Val_int((uintnat)Int_val(%{a}) >> Int_val(%{b}))"]
     | "%int_neg" -> [%string "Val_int(-Int_val(%{a}))"]
     | "%caml_format_int_special" -> [%string {|caml_format_int("%%d", %{a})|}]
     | "%direct_obj_tag" -> [%string "Val_int(Tag_val(%{a}))"]
     | "caml_array_unsafe_get" -> [%string "Field(%{a}, Int_val(%{b}))"]
     | _ ->
       let args_str = List.map args ~f:arg |> String.concat ~sep:", " in
       [%string "%{name}(%{args_str})"])
;;

let f prog =
  let ctx =
    { prog; closures = find_closures prog; strings = Hash_set.create (module String) }
  in
  let cls = Hashtbl.to_alist ctx.closures in
  let bodies = List.map cls ~f:(fun (pc, c) -> compile_closure ctx pc c) in
  let strs = Hash_set.to_list ctx.strings in
  [ List.map cls ~f:(fun (pc, _) -> [%string "value %{cname pc}(value* env);"])
  ; List.map strs ~f:(fun s -> [%string "value %{sname s};"])
  ; bodies
  ; [ "int main() {" ]
  ; (let n = List.length strs in
     if n > 0 then [ [%string "check_stack(%{n#Int});"] ] else [])
  ; List.concat_map strs ~f:(fun s ->
      [ [%string "%{sname s} = caml_copy_string(\"%{s}\");"]
      ; [%string "*(sp++) = %{sname s};"]
      ])
  ; [ [%string "bp = sp; %{cname prog.start}(NULL); return 0;}"] ]
  ]
  |> List.concat
  |> String.concat_lines
;;
