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

let%expect_test "" =
  let a = 0x123456 in
  let b = 0xABCDEF in
  let high, low = mul a b in
  print_int high;
  print_newline ();
  print_int low;
  print_newline ();
  print_int (a * b);
  [%expect {|
    800666
    5880394
    13432952306250 |}]
;;
