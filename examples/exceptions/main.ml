(* Comprehensive exception tests *)

let print_int n = Int.to_string n |> Io.puts

exception Stop

(* Test 1: try/with that doesn't raise *)
let test_no_raise () =
  let result =
    try 42 with
    | _ -> 0
  in
  print_int result
;;

(* Test 2: try/with that raises *)
let test_simple_raise () =
  let result =
    try raise Stop with
    | Stop -> 99
  in
  print_int result
;;

(* Test 3: Nested try/with - inner catches *)
let test_nested_inner () =
  let result =
    try
      try raise Stop with
      | Stop -> 77
    with
    | _ -> 0
  in
  print_int result
;;

(* Test 4: Nested try/with - outer catches *)
exception MyExn

let test_nested_outer () =
  let result =
    try
      try raise MyExn with
      | Stop -> 0
    with
    | MyExn -> 88
  in
  print_int result
;;

(* Test 5: Raise through function call *)
let helper_raise () = raise Stop

let test_raise_through_call () =
  let result =
    try
      let () = helper_raise () in
      0
    with
    | Stop -> 55
  in
  print_int result
;;

(* Test 6: Deep call stack with exception *)
let rec deep_raise n = if n = 0 then raise Stop else 1 + deep_raise (n - 1)

let test_deep_raise () =
  let result =
    try deep_raise 100 with
    | Stop -> 66
  in
  print_int result
;;

(* Test 7: Exception with value *)
exception WithInt of int

let test_exn_value () =
  let result =
    try raise (WithInt 123) with
    | WithInt n -> n
  in
  print_int result
;;

(* Test 8: Multiple handlers *)
exception A
exception B
exception C

let test_multiple_handlers () =
  let f exn =
    try raise exn with
    | A -> 1
    | B -> 2
    | C -> 3
  in
  print_int (f A + f B + f C)
;;

(* Test 9: Re-raise exception *)
let test_reraise () =
  let result =
    try
      try raise Stop with
      | Stop ->
        (* do some work *)
        let _ = 1 + 2 in
        raise Stop
    with
    | Stop -> 44
  in
  print_int result
;;

(* Test 10: Exception and GC interaction *)
let test_exn_gc () =
  let result =
    try
      (* allocate garbage *)
      let rec loop n =
        if n = 0
        then raise Stop
        else (
          let _ = n, n + 1, n + 2 in
          loop (n - 1))
      in
      loop 1000
    with
    | Stop -> 33
  in
  print_int result
;;

(* Test 11: Catching any exception *)
let test_catch_any () =
  let result =
    try raise (WithInt 999) with
    | _ -> 22
  in
  print_int result
;;

(* Test 12: Normal return from try block *)
let test_normal_return () =
  let f () =
    try
      let x = 10 in
      let y = 20 in
      x + y
    with
    | _ -> 0
  in
  print_int (f ())
;;

let () =
  Io.puts "Test 1: No raise";
  test_no_raise ();
  Io.puts "Test 2: Simple raise";
  test_simple_raise ();
  Io.puts "Test 3: Nested inner catch";
  test_nested_inner ();
  Io.puts "Test 4: Nested outer catch";
  test_nested_outer ();
  Io.puts "Test 5: Raise through call";
  test_raise_through_call ();
  Io.puts "Test 6: Deep raise";
  test_deep_raise ();
  Io.puts "Test 7: Exception with value";
  test_exn_value ();
  Io.puts "Test 8: Multiple handlers";
  test_multiple_handlers ();
  Io.puts "Test 9: Re-raise";
  test_reraise ();
  Io.puts "Test 10: Exception and GC";
  test_exn_gc ();
  Io.puts "Test 11: Catch any";
  test_catch_any ();
  Io.puts "Test 12: Normal return";
  test_normal_return ();
  Io.puts "All exception tests passed!"
;;
