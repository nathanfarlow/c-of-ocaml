let mul a b rshift =
  (* Splits a 24-bit number into two 12-bit parts *)
  let split_12 x =
    let low = x land 0xFFF in
    let high = (x lsr 12) land 0xFFF in
    high, low
  in
  (* Multiply two 12-bit numbers, ensuring the result stays within 24 bits *)
  let mul_12 x y = x * y land 0xFFFFFF in
  let a_high, a_low = split_12 a in
  let b_high, b_low = split_12 b in
  let p1 = mul_12 a_low b_low in
  let p2 = mul_12 a_low b_high in
  let p3 = mul_12 a_high b_low in
  let p4 = mul_12 a_high b_high in
  (* Combine parts, ensuring they fit within 24 bits *)
  let lower = p1 in
  let middle = (p2 + p3) land 0xFFFFFF in
  let upper = p4 in
  (* Compute result_low without using lsr 24 *)
  let result_low = (lower + ((middle land 0xFFF) lsl 12)) land 0xFFFFFF in
  (* Compute result_high without using lsr 24 *)
  let result_high = (upper + (middle lsr 12)) land 0xFFFFFF in
  (* Compute the shifted result *)
  let shifted_result =
    let shifted_low = (result_low lsr rshift) land 0xFFFFFF in
    let shifted_high =
      ((result_high land ((1 lsl rshift) - 1)) lsl (24 - rshift)) land 0xFFFFFF
    in
    shifted_low lor shifted_high land 0xFFFFFF
  in
  shifted_result
;;

let%expect_test "" =
  let a = 0x123456 in
  let b = 0xABCDEF in
  let result = mul a b 4 in
  print_int result;
  print_newline ();
  [%expect {|
    10853284 |}]
;;
