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
