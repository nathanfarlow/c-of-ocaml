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

let%expect_test "math" =
  let%bind () =
    compile_and_run
      {|
let mul a b rshift =
  let split_12 x =
    let low = x land 0xFFF in
    let high = (x lsr 12) land 0xFFF in
    high, low
  in
  let a_high, a_low = split_12 a in
  let b_high, b_low = split_12 b in
  let p1 = a_low * b_low in
  let p2 = a_low * b_high in
  let p3 = a_high * b_low in
  let p4 = a_high * b_high in
  let lower = p1 in
  let middle = p2 + p3 in
  let upper = p4 in
  let result_low = lower + ((middle land 0xFFF) lsl 12) in
  let result_high =
    upper + (middle lsr 12) + (p1 lsr 24) + ((result_low lsr 24) land 0xFFF)
  in
  let shifted_result =
    (result_low lsr rshift) lor ((result_high land ((1 lsl rshift) - 1)) lsl (24 - rshift))
  in
  shifted_result land 0xFFFFFF
;;

let print_int n = Int.to_string n |> Io.puts

let () =
  let a = 0x123456 in
  let b = 0xABCDEF in
  let result = mul a b 4 in
  print_int result;
;;
|}
  in
  [%expect {| 10853284 |}];
  return ()
;;

let%expect_test "array" =
  let%bind () =
    compile_and_run
      {|
let create_list n =
  let rec aux acc remaining =
    if remaining = 0 then acc else aux (String.make 1 'a' :: acc) (remaining - 1)
  in
  aux [] n
;;

let hd_exn l =
  match l with
  | [] -> failwith "hd_exn"
  | hd :: _ -> hd

let () =
  for _ = 0 to 100 do
    let l = create_list 10000 in
    Io.putc (hd_exn l |> fun s -> String.get s 0)
  done;
;;
|}
  in
  [%expect
    {| aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa |}];
  return ()
;;
