[@@@ocaml.warning "-32"]
(* Helper modules to replace Core functions *)

module Option = struct
  let some_if cond x = if cond then Some x else None

  let bind o ~f =
    match o with
    | None -> None
    | Some x -> f x
  ;;

  let map o ~f =
    match o with
    | None -> None
    | Some x -> Some (f x)
  ;;

  let value o ~default =
    match o with
    | None -> default
    | Some x -> x
  ;;

  let iter o ~f =
    match o with
    | None -> ()
    | Some x -> f x
  ;;
end

module Char_ext = struct
  let is_whitespace = function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false
  ;;

  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false
  ;;

  let is_alpha = function
    | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false
  ;;

  let is_alphanum c = is_alpha c || is_digit c
end

module String_ext = struct
  external unsafe_get : string -> int -> char = "%string_unsafe_get"

  let sub s ~pos ~len =
    let bytes = Bytes.create len in
    for i = 0 to len - 1 do
      Bytes.unsafe_set bytes i (unsafe_get s (pos + i))
    done;
    Bytes.to_string bytes
  ;;

  let lsplit2 s ~on =
    let len = String.length s in
    let rec find i =
      if i >= len
      then None
      else if Char.equal (unsafe_get s i) on
      then Some i
      else find (i + 1)
    in
    match find 0 with
    | None -> None
    | Some idx ->
      let left = sub s ~pos:0 ~len:idx in
      let right = sub s ~pos:(idx + 1) ~len:(len - idx - 1) in
      Some (left, right)
  ;;

  let strip s =
    let len = String.length s in
    let rec find_start i =
      if i >= len
      then i
      else if Char_ext.is_whitespace (unsafe_get s i)
      then find_start (i + 1)
      else i
    in
    let rec find_end i =
      if i <= 0
      then 0
      else if Char_ext.is_whitespace (unsafe_get s (i - 1))
      then find_end (i - 1)
      else i
    in
    let start = find_start 0 in
    let stop = find_end len in
    if start >= stop then "" else sub s ~pos:start ~len:(stop - start)
  ;;

  let is_empty s = String.length s = 0

  let equal s1 s2 =
    let len1 = String.length s1 in
    let len2 = String.length s2 in
    if len1 <> len2
    then false
    else (
      let rec loop i =
        if i >= len1
        then true
        else if not (Char.equal (unsafe_get s1 i) (unsafe_get s2 i))
        then false
        else loop (i + 1)
      in
      loop 0)
  ;;

  let compare s1 s2 =
    let len1 = String.length s1 in
    let len2 = String.length s2 in
    let min_len = if len1 < len2 then len1 else len2 in
    let rec loop i =
      if i >= min_len
      then Int.compare len1 len2
      else (
        let c = Char.compare (unsafe_get s1 i) (unsafe_get s2 i) in
        if c <> 0 then c else loop (i + 1))
    in
    loop 0
  ;;
end

module Int_ext = struct
  let rec pow base exp =
    if exp = 0
    then 1
    else if exp = 1
    then base
    else (
      let half = pow base (exp / 2) in
      if exp mod 2 = 0 then half * half else half * half * base)
  ;;

  let of_string s =
    let len = String.length s in
    if len = 0
    then failwith "Int.of_string"
    else (
      let start, sign =
        if Char.equal (String_ext.unsafe_get s 0) '-'
        then 1, -1
        else if Char.equal (String_ext.unsafe_get s 0) '+'
        then 1, 1
        else 0, 1
      in
      let rec loop i acc =
        if i >= len
        then acc
        else (
          let c = String_ext.unsafe_get s i in
          if Char_ext.is_digit c
          then loop (i + 1) ((acc * 10) + (Char.code c - 48))
          else failwith "Int.of_string")
      in
      sign * loop start 0)
  ;;
end

module List_ext = struct
  let filter_map l ~f =
    let rec aux acc = function
      | [] -> List.rev acc
      | x :: xs ->
        (match f x with
         | None -> aux acc xs
         | Some y -> aux (y :: acc) xs)
    in
    aux [] l
  ;;

  let partition_map l ~f =
    let rec aux firsts seconds = function
      | [] -> List.rev firsts, List.rev seconds
      | x :: xs ->
        (match f x with
         | `First y -> aux (y :: firsts) seconds xs
         | `Second y -> aux firsts (y :: seconds) xs)
    in
    aux [] [] l
  ;;

  let concat_map l ~f =
    let rec aux acc = function
      | [] -> List.rev acc
      | x :: xs -> aux (List.rev_append (f x) acc) xs
    in
    aux [] l
  ;;

  let reduce l ~f =
    match l with
    | [] -> None
    | x :: xs -> Some (List.fold_left ~f x xs)
  ;;

  (* Simple insertion sort for small lists *)
  let sort l ~compare =
    let rec insert x = function
      | [] -> [ x ]
      | y :: ys as l -> if compare x y <= 0 then x :: l else y :: insert x ys
    in
    List.fold_left ~f:(fun acc x -> insert x acc) [] l
  ;;

  let sum l ~f = List.fold_left ~f:(fun acc x -> acc + f x) 0 l

  (* Group consecutive elements with same key *)
  let sort_and_group l ~compare =
    let sorted = sort l ~compare:(fun (k1, _) (k2, _) -> compare k1 k2) in
    let rec group acc current_key current_vals = function
      | [] ->
        (match current_vals with
         | [] -> List.rev acc
         | _ -> List.rev ((current_key, List.rev current_vals) :: acc))
      | (k, v) :: rest ->
        if compare k current_key = 0
        then group acc current_key (v :: current_vals) rest
        else (
          let acc' =
            match current_vals with
            | [] -> acc
            | _ -> (current_key, List.rev current_vals) :: acc
          in
          group acc' k [ v ] rest)
    in
    match sorted with
    | [] -> []
    | (k, v) :: rest -> group [] k [ v ] rest
  ;;
end

(* The CAS expression type *)
type t =
  | Int of int
  | Var of string
  | Add of t * t
  | Mul of t * t
  | Div of t * t
  | Pow of t * t
  | Sin of t
  | Cos of t
  | Tan of t
  | Exp of t
  | Ln of t
  | Sqrt of t

let rec equal e1 e2 =
  match e1, e2 with
  | Int n1, Int n2 -> n1 = n2
  | Var v1, Var v2 -> String_ext.equal v1 v2
  | Add (a1, b1), Add (a2, b2) -> equal a1 a2 && equal b1 b2
  | Mul (a1, b1), Mul (a2, b2) -> equal a1 a2 && equal b1 b2
  | Div (a1, b1), Div (a2, b2) -> equal a1 a2 && equal b1 b2
  | Pow (a1, b1), Pow (a2, b2) -> equal a1 a2 && equal b1 b2
  | Sin x1, Sin x2 -> equal x1 x2
  | Cos x1, Cos x2 -> equal x1 x2
  | Tan x1, Tan x2 -> equal x1 x2
  | Exp x1, Exp x2 -> equal x1 x2
  | Ln x1, Ln x2 -> equal x1 x2
  | Sqrt x1, Sqrt x2 -> equal x1 x2
  | _ -> false
;;

(* Comparison for sorting *)
let rec compare e1 e2 =
  match e1, e2 with
  | Int n1, Int n2 -> Int.compare n1 n2
  | Int _, _ -> -1
  | _, Int _ -> 1
  | Var v1, Var v2 -> String_ext.compare v1 v2
  | Var _, _ -> -1
  | _, Var _ -> 1
  | Add (a1, b1), Add (a2, b2) ->
    let c = compare a1 a2 in
    if c <> 0 then c else compare b1 b2
  | Add _, _ -> -1
  | _, Add _ -> 1
  | Mul (a1, b1), Mul (a2, b2) ->
    let c = compare a1 a2 in
    if c <> 0 then c else compare b1 b2
  | Mul _, _ -> -1
  | _, Mul _ -> 1
  | Div (a1, b1), Div (a2, b2) ->
    let c = compare a1 a2 in
    if c <> 0 then c else compare b1 b2
  | Div _, _ -> -1
  | _, Div _ -> 1
  | Pow (a1, b1), Pow (a2, b2) ->
    let c = compare a1 a2 in
    if c <> 0 then c else compare b1 b2
  | Pow _, _ -> -1
  | _, Pow _ -> 1
  | Sin x1, Sin x2 -> compare x1 x2
  | Sin _, _ -> -1
  | _, Sin _ -> 1
  | Cos x1, Cos x2 -> compare x1 x2
  | Cos _, _ -> -1
  | _, Cos _ -> 1
  | Tan x1, Tan x2 -> compare x1 x2
  | Tan _, _ -> -1
  | _, Tan _ -> 1
  | Exp x1, Exp x2 -> compare x1 x2
  | Exp _, _ -> -1
  | _, Exp _ -> 1
  | Ln x1, Ln x2 -> compare x1 x2
  | Ln _, _ -> -1
  | _, Ln _ -> 1
  | Sqrt x1, Sqrt x2 -> compare x1 x2
;;

let neg e = Mul (Int (-1), e)

let deriv var =
  let rec d = function
    | Int _ -> Int 0
    | Var v -> Int (if String_ext.equal v var then 1 else 0)
    | Add (a, b) -> Add (d a, d b)
    | Mul (a, b) -> Add (Mul (d a, b), Mul (a, d b))
    | Div (a, b) -> Div (Add (Mul (d a, b), neg (Mul (a, d b))), Mul (b, b))
    | Pow (a, b) -> Mul (Pow (a, b), Add (Mul (d b, Ln a), Div (Mul (b, d a), a)))
    | Sin x -> Mul (Cos x, d x)
    | Cos x -> neg (Mul (Sin x, d x))
    | Tan x -> Div (d x, Pow (Cos x, Int 2))
    | Exp x -> Mul (Exp x, d x)
    | Ln x -> Div (d x, x)
    | Sqrt x -> Div (d x, Mul (Int 2, Sqrt x))
  in
  d
;;

let simplify =
  let gcd a b =
    let rec g a b = if b = 0 then a else g b (a mod b) in
    Int.max 1 (g (abs a) (abs b))
  in
  (* Combine (term, count) pairs by summing counts, eliminating zeros. *)
  let combine_like_terms pairs =
    List_ext.sort_and_group pairs ~compare
    |> List_ext.filter_map ~f:(fun (key, vals) ->
      let sum = List_ext.sum vals ~f:(fun x -> x) in
      Option.some_if (sum <> 0) (key, sum))
  in
  (* Canonical order so fixpoint terminates. *)
  let sort_by_term ps = List_ext.sort ps ~compare:(fun (a, _) (b, _) -> compare a b) in
  let rec mul a b =
    match a, b with
    | Int 0, _ -> Int 0
    | _, Int 0 -> Int 0
    | Int 1, e -> e
    | e, Int 1 -> e
    | Int x, Int y -> Int (x * y)
    | _ -> Mul (a, b)
  and pow a b =
    match a, b with
    | _, Int 0 -> Int 1
    | e, Int 1 -> e
    | Int 0, _ -> Int 0
    | Int 1, _ -> Int 1
    | Int x, Int y when y > 0 -> Int (Int_ext.pow x y)
    | _ -> Pow (a, b)
  and div a b =
    match a, b with
    | Int 0, _ -> Int 0
    | _ ->
      (* Extract (base, exponent) pairs; negative exponent = denominator. *)
      let rec factors sign = function
        | Mul (a, b) -> factors sign a @ factors sign b
        | Div (a, b) -> factors sign a @ factors (-sign) b
        | Pow (base, Int n) -> [ base, sign * n ]
        | e -> [ e, sign ]
      in
      let all = factors 1 a @ factors (-1) b |> combine_like_terms in
      (* Separate integer and symbolic factors *)
      let ints, syms =
        List_ext.partition_map all ~f:(function
          | Int x, exp -> `First (x, exp)
          | base, exp -> `Second (base, exp))
      in
      (* Compute integer coefficient for num/den, then reduce by GCD. *)
      let num_coef, den_coef =
        let num_coef, den_coef =
          List.fold_left
            ~f:(fun (n, d) (x, exp) ->
              if exp > 0 then n * Int_ext.pow x exp, d else n, d * Int_ext.pow x (-exp))
            (1, 1)
            ints
        in
        let gcd = gcd num_coef den_coef in
        num_coef / gcd, den_coef / gcd
      in
      (* Partition symbolic factors by sign of exponent. *)
      let num_syms, den_syms =
        List_ext.partition_map syms ~f:(fun (base, exp) ->
          if exp > 0 then `First (base, exp) else `Second (base, -exp))
      in
      let product coef syms =
        sort_by_term syms
        |> List.map ~f:(fun (base, exp) -> pow base (Int exp))
        |> List.fold_left ~f:mul (Int coef)
      in
      let num = product num_coef num_syms in
      let den = product den_coef den_syms in
      if equal den (Int 1) then num else Div (num, den)
  in
  let add a b =
    let rec terms coef = function
      | Int n -> [ Int 1, coef * n ]
      | Add (a, b) -> terms coef a @ terms coef b
      | Mul (Int n, e) -> [ e, coef * n ]
      | Mul (e, Int n) -> [ e, coef * n ]
      | e -> [ e, coef ]
    in
    let to_expr termlist =
      sort_by_term termlist
      |> List.map ~f:(fun (base, coef) -> mul (Int coef) base)
      |> List_ext.reduce ~f:(fun a b -> Add (a, b))
      |> Option.value ~default:(Int 0)
    in
    let fractional_terms, other_terms =
      terms 1 a @ terms 1 b
      |> combine_like_terms
      |> List_ext.partition_map ~f:(function
        | Div (n, d), c -> `First (d, (n, c))
        | t -> `Second t)
    in
    (* Group fractions by denominator so x/y + z/y becomes (x+z)/y. *)
    let merged_fractional_terms =
      List_ext.sort_and_group fractional_terms ~compare
      |> List.map ~f:(fun (denom, nums) ->
        let numer =
          List_ext.concat_map nums ~f:(fun (n, c) -> terms c n)
          |> combine_like_terms
          |> to_expr
        in
        div numer denom, 1)
    in
    merged_fractional_terms @ other_terms |> to_expr
  in
  let rec step = function
    | Int _ as e -> e
    | Var _ as e -> e
    | Add (a, b) -> add (step a) (step b)
    | Mul (a, b) -> div (mul (step a) (step b)) (Int 1)
    | Div (a, b) -> div (step a) (step b)
    | Pow (a, b) ->
      (match step a, step b with
       | Pow (x, Int n), Int m -> pow x (Int (n * m))
       | Sqrt x, Int 2 -> x
       | a, b -> pow a b)
    | Sin e -> Sin (step e)
    | Cos e -> Cos (step e)
    | Tan e -> Tan (step e)
    | Sqrt e -> Sqrt (step e)
    | Exp e ->
      (match step e with
       | Ln x -> x
       | e -> Exp e)
    | Ln e ->
      (match step e with
       | Exp x -> x
       | e -> Ln e)
  in
  let rec fixpoint e =
    let e' = step e in
    if equal e e' then e else fixpoint e'
  in
  fixpoint
;;

let to_string =
  let paren outer inner s = if outer > inner then "(" ^ s ^ ")" else s in
  (* For left-associative ops, right operand of same op doesn't need parens. *)
  let right_prec prec = function
    | Add _ when prec = 1 -> 1
    | Mul _ when prec = 2 -> 2
    | _ -> prec + 1
  in
  (* Omit '*' for 2x, 2sin(x), xy, x(y+1), but keep for x * sin(y). *)
  let can_omit_times a b =
    let rec left_end = function
      | Int _ -> `Int
      | Var _ -> `Var
      | Mul (_, b) -> left_end b
      | Pow (_, exp) -> left_end exp
      | _ -> `Other
    in
    let rec right_start = function
      | Var _ -> `Var_or_paren
      | Add _ -> `Var_or_paren
      | Sin _ -> `Func
      | Cos _ -> `Func
      | Tan _ -> `Func
      | Exp _ -> `Func
      | Ln _ -> `Func
      | Sqrt _ -> `Func
      | Pow (base, _) -> right_start base
      | Mul (a, _) -> right_start a
      | _ -> `Other
    in
    match left_end a, right_start b with
    | `Int, `Var_or_paren -> true
    | `Int, `Func -> true
    | `Var, `Var_or_paren -> true
    | _ -> false
  in
  (* Extract leading negative so a + -bc prints as a - bc. Returns negated
     expression. *)
  let rec extract_neg = function
    | Mul (Int n, e) when n = -1 -> Some e
    | Mul (Int n, e) when n < 0 -> Some (Mul (Int (-n), e))
    | Mul (a, b) -> Option.map (extract_neg a) ~f:(fun a' -> Mul (a', b))
    | _ -> None
  in
  let rec emit prec = function
    | Int n -> Int.to_string n
    | Var v -> v
    | Add (a, b) ->
      (match extract_neg b with
       | Some b' -> paren prec 1 (emit 1 a ^ " - " ^ emit 2 b')
       | None -> paren prec 1 (emit 1 a ^ " + " ^ emit (right_prec 1 b) b))
    | Mul (Int n, e) when n = -1 -> paren prec 4 ("-" ^ emit 5 e)
    | Mul (a, b) when can_omit_times a b -> paren prec 2 (emit 2 a ^ emit 2 b)
    | Mul (a, b) -> paren prec 2 (emit 2 a ^ " * " ^ emit (right_prec 2 b) b)
    | Div (a, b) -> paren prec 2 (emit 2 a ^ " / " ^ emit 3 b)
    | Pow (a, b) -> paren prec 3 (emit 4 a ^ "^" ^ emit 3 b)
    | Sin x -> "sin(" ^ emit 0 x ^ ")"
    | Cos x -> "cos(" ^ emit 0 x ^ ")"
    | Tan x -> "tan(" ^ emit 0 x ^ ")"
    | Exp x -> "exp(" ^ emit 0 x ^ ")"
    | Ln x -> "ln(" ^ emit 0 x ^ ")"
    | Sqrt x -> "sqrt(" ^ emit 0 x ^ ")"
  in
  emit 0
;;

module Parser = struct
  let return x _ p = Some (x, p)
  let ( let* ) m f s p = Option.bind (m s p) ~f:(fun (x, p') -> f x s p')

  let ( <|> ) a b s p =
    match a s p with
    | None -> b s p
    | r -> r
  ;;

  let fail _ _ = None

  let ( *> ) a b =
    let* _ = a in
    b
  ;;

  let ( *>| ) a f =
    let* _ = a in
    f ()
  ;;

  let ( <* ) a b =
    let* x = a in
    let* _ = b in
    return x
  ;;

  let ( >>| ) m f =
    let* x = m in
    return (f x)
  ;;

  let choice ps = List.fold_right ~f:( <|> ) ps fail

  let take_while f s p =
    let rec go i =
      if i < String.length s && f (String_ext.unsafe_get s i) then go (i + 1) else i
    in
    go p
  ;;

  let ws = take_while Char_ext.is_whitespace

  let sat f s p =
    let p = ws s p in
    if p < String.length s && f (String_ext.unsafe_get s p)
    then Some (String_ext.unsafe_get s p, p + 1)
    else None
  ;;

  let char c = sat (Char.equal c) *> return ()

  let peek s p =
    let p = ws s p in
    Some ((if p < String.length s then Some (String_ext.unsafe_get s p) else None), p)
  ;;

  let token f s p =
    let p = ws s p in
    let p' = take_while f s p in
    if p' > p then Some (String_ext.sub s ~pos:p ~len:(p' - p), p') else None
  ;;

  let number = token Char_ext.is_digit >>| Int_ext.of_string

  let keyword kw =
    let* x = token Char_ext.is_alphanum in
    if String_ext.equal x kw then return () else fail
  ;;

  let parse p s =
    p s 0 |> Option.bind ~f:(fun (r, p) -> Option.some_if (ws s p >= String.length s) r)
  ;;
end

let of_string s =
  let open Parser in
  let parens p = char '(' *>| p <* char ')' in
  let binops operand ops =
    let op = choice (List.map ~f:(fun (c, f) -> char c *> return f) ops) in
    let* a = operand () in
    let rec loop left =
      (let* f = op in
       let* right = operand () in
       loop (f left right))
      <|> return left
    in
    loop a
  in
  let rec expr () =
    binops term [ ('+', fun a b -> Add (a, b)); ('-', fun a b -> Add (a, neg b)) ]
  and term () =
    binops factor [ ('*', fun a b -> Mul (a, b)); ('/', fun a b -> Div (a, b)) ]
  and factor () =
    let* a = power () in
    let* c = peek in
    match c with
    | Some c when Char_ext.is_alpha c || Char.equal c '(' ->
      factor () >>| fun b -> Mul (a, b)
    | _ -> return a
  and power () =
    let* base = unary () in
    char '^' *>| power >>| (fun e -> Pow (base, e)) <|> return base
  and unary () = char '-' *>| unary >>| neg <|> atom ()
  and atom () =
    let fn name ctor = keyword name *> (parens expr >>| ctor) in
    choice
      [ parens expr
      ; fn "sin" (fun x -> Sin x)
      ; fn "cos" (fun x -> Cos x)
      ; fn "tan" (fun x -> Tan x)
      ; fn "exp" (fun x -> Exp x)
      ; fn "ln" (fun x -> Ln x)
      ; fn "sqrt" (fun x -> Sqrt x)
      ; (number >>| fun n -> Int n)
      ; (sat Char_ext.is_alpha >>| fun c -> Var (String.make 1 c))
      ]
  in
  parse (expr ()) s
;;

let last = ref None

let show e =
  let result = simplify e in
  Io.puts (to_string result);
  last := Some result
;;

let eval line =
  if String_ext.equal line "q" || String_ext.equal line "quit"
  then false
  else (
    (match String_ext.lsplit2 line ~on:' ' with
     | Some ("d", var) ->
       (match !last with
        | Some e -> show (deriv (String_ext.strip var) e)
        | None -> Io.puts "No expression.")
     | _ -> Option.iter (of_string line) ~f:show);
    true)
;;

let () =
  Io.puts "Enter expression to simplify, or 'd <var>' for derivative.";
  let rec loop () =
    String.iter ~f:Io.putc "> ";
    let line = String_ext.strip (Io.gets ()) in
    let continue = String_ext.is_empty line || eval line in
    if continue then loop ()
  in
  loop ()
;;
