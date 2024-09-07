open Ti84ce.Os

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
  let p1 = mul_12 a_low b_low in
  (* low * low *)
  let p2 = mul_12 a_low b_high in
  (* low * high *)
  let p3 = mul_12 a_high b_low in
  (* high * low *)
  let p4 = mul_12 a_high b_high in
  (* high * high *)

  (* Combine partial products *)
  let low = p1 land 0xFFFFFF in
  (* Calculate carry and add the middle terms *)
  let middle = p2 + p3 + ((p1 lsr 12) land 0xFFF) in
  (* Combine the high term, shift properly, and add the middle carry *)
  let high = (p4 + (middle lsr 12)) land 0xFFFFFF in
  high, low
;;

let print_int n =
  Int.to_string n |> put_str_full;
  put_str_full "."
;;

let rec wait () = if get_csc () = 0 then wait () else ()

(* Test the function *)
let () =
  let a = 0x123456 in
  let b = 0xABCDEF in
  let high, low = mul a b in
  clr_home ();
  print_int high;
  print_int low;
  print_int (a * b);
  wait ()
;;

(* let () = *)
(*   clr_home (); *)
(*   let s, c = *)
(*     let open Fixed.O in *)
(*     (\* let half = !1 / !3 in *\) *)
(*     (\* let s = Fixed.sin half * !100 in *\) *)
(*     (\* let c = Fixed.cos half * !100 in *\) *)
(*     (\* s, c *\) *)
(*     !2 * !4, !2 / !3 * !4 *)
(*   in *)
(*   print_int (Fixed.to_int s); *)
(*   print_int (Fixed.to_int c); *)
(*   wait () *)
(* ;; *)
