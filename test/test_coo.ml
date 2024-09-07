open! Core
open! Async
open! Util

let%expect_test "trivial putc" =
  let%bind () = compile_and_run {|
    let () = Io.putc 'H';
|} in
  [%expect {| H |}];
  return ()
;;

let%expect_test "fib" =
  let%bind () =
    compile_and_run
      {|
let print_int n = Int.to_string n |> Io.puts
let rec fib k = if k < 2 then 1 else fib (k - 1) + fib (k - 2)

let () =
  for i = 0 to 20 do
    print_int (fib i)
  done
;;
  |}
  in
  [%expect
    {|
    1
    1
    2
    3
    5
    8
    13
    21
    34
    55
    89
    144
    233
    377
    610
    987
    1597
    2584
    4181
    6765
    10946 |}];
  return ()
;;

let%expect_test "something" =
  let%bind () =
    compile_and_run
      {|
let () =
  let l = [ Some "hello"; Some "world"; None ] in
  List.iter l ~f:(function
      | None -> Io.puts "None"
      | Some s -> Io.puts s)
|}
  in
  [%expect {|
  hello
  world
  None |}];
  return ()
;;

let%expect_test "partial application" =
  let%bind () =
    compile_and_run
      {|
let () =
  let f x y = x + y in
  let g = f 3 in
  Io.puts (Int.to_string (g 4))
|}
  in
  [%expect {| 7 |}];
  return ()
;;

let%expect_test "modules" =
  let%bind () =
    compile_and_run
      {|
module A = struct
  let x = 3
end

module B (A : sig val x : int end) = struct
  let y = A.x + 1
end

module C = B(A)

let () =
  Io.puts (Int.to_string (C.y))
|}
  in
  [%expect {| 4 |}];
  return ()
;;

let%expect_test "modules" =
  let%bind () =
    compile_and_run
      {|
(* Split a 24-bit integer into high and low 12-bit parts *)
let split_12 x =
  let low = x land 0xFFF in
  let high = (x lsr 12) land 0xFFF in
  high, low
;;

(* Multiply two 12-bit parts and return a 24-bit result *)
let mul_12 x y = x * y

let mul a b =
  (* Split both numbers into high and low parts *)
  let a_high, a_low = split_12 a in
  let b_high, b_low = split_12 b in
  (* Calculate the four partial products *)
  let p1 = a_low * b_low in
  (* low * low *)
  let p2 = a_low * b_high in
  (* low * high *)
  let p3 = a_high * b_low in
  (* high * low *)
  let p4 = a_high * b_high in
  (* high * high *)

  (* Combine the partial products *)
  let lower = p1 in
  let middle = p2 + p3 in
  let upper = p4 in
  (* Perform the final addition *)
  let result_low = lower + ((middle land 0xFFF) lsl 12) in
  let result_high =
    upper + (middle lsr 12) + (p1 lsr 24) + ((result_low lsr 24) land 0xFFF)
  in
  result_high land 0xFFFFFF, result_low land 0xFFFFFF
;;

let print_int n = Int.to_string n |> Io.puts

(* Test the function *)
let () =
  let a = 0x123456 in
  let b = 0xABCDEF in
  let high, low = mul a b in
  print_int high;
  print_int low;
  print_int (a * b);
;;
|}
  in
  [%expect {|
    800666
    5880394
    -1705395638 |}];
  return ()
;;
