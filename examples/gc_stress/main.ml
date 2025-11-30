(* GC Stress Test - exercises mark-compact GC with moving objects *)

let print_int n = Int.to_string n |> Io.puts

(* Test 1: Many allocations to trigger multiple GCs *)
let test_many_allocs () =
  let rec loop n =
    if n = 0
    then 0
    else (
      let _ = n, n + 1, n + 2 in
      loop (n - 1))
  in
  print_int (loop 10000)
;;

(* Test 2: Closures with free variables - values must survive GC *)
let test_closures () =
  let x = 1, 2, 3 in
  let y = 4, 5, 6 in
  let f () =
    (* Allocate to potentially trigger GC *)
    let _ = 100, 200, 300 in
    let _ = 101, 201, 301 in
    let _ = 102, 202, 302 in
    (* Now use captured x and y - they must still be valid after GC *)
    let a, b, c = x in
    let d, e, f = y in
    a + b + c + d + e + f
  in
  (* More allocations before calling f *)
  let _ = 999, 888, 777 in
  print_int (f ())
;;

(* Test 3: Nested closures with multiple levels of capture *)
let test_nested_closures () =
  let a = 10, 20 in
  let f () =
    let b = 30, 40 in
    let g () =
      let c = 50, 60 in
      let h () =
        (* Trigger GC *)
        let _ = 1, 2, 3, 4, 5 in
        (* Use all captured values *)
        let a1, a2 = a in
        let b1, b2 = b in
        let c1, c2 = c in
        a1 + a2 + b1 + b2 + c1 + c2
      in
      h ()
    in
    g ()
  in
  print_int (f ())
;;

(* Test 4: Long-lived data across many GC cycles *)
let test_long_lived () =
  let data = 42, (43, (44, (45, 46))) in
  let rec trigger_gc n =
    if n = 0
    then ()
    else (
      (* Allocate garbage *)
      let _ = n, n + 1, n + 2 in
      let _ = n * 2, n * 3, n * 4 in
      trigger_gc (n - 1))
  in
  trigger_gc 5000;
  (* data should still be valid *)
  let a, (b, (c, (d, e))) = data in
  print_int (a + b + c + d + e)
;;

(* Test 5: String allocation and concatenation *)
let test_strings () =
  let s1 = "Hello" in
  let s2 = " " in
  let s3 = "World" in
  let rec concat_many n acc =
    if n = 0
    then acc
    else (
      let new_acc = acc ^ "!" in
      concat_many (n - 1) new_acc)
  in
  let base = s1 ^ s2 ^ s3 in
  let result = concat_many 100 base in
  print_int (String.length result)
;;

(* Test 6: Many allocations with retained data *)
let test_retained_data () =
  let data1 = 111, 222, 333 in
  let data2 = 444, 555, 666 in
  let rec alloc_garbage n =
    if n = 0
    then ()
    else (
      let _ = n, n, n in
      alloc_garbage (n - 1))
  in
  alloc_garbage 5000;
  (* data1 and data2 should still be valid *)
  let a, b, c = data1 in
  let d, e, f = data2 in
  print_int (a + b + c + d + e + f)
;;

let () =
  Io.puts "Test 1: Many allocations";
  test_many_allocs ();
  Io.puts "Test 2: Closures with free vars";
  test_closures ();
  Io.puts "Test 3: Nested closures";
  test_nested_closures ();
  Io.puts "Test 4: Long-lived data";
  test_long_lived ();
  Io.puts "Test 5: String operations";
  test_strings ();
  Io.puts "Test 6: Retained data";
  test_retained_data ();
  Io.puts "All GC stress tests passed!"
;;
