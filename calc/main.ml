(* let print_endline s = *)
(*   String.iter s ~f:putc; *)
(*   putc '\n' *)
(* ;; *)

let print_int =
  let choices = "0123456789" in
  fun n ->
    let rec loop n =
      if n = 0
      then ()
      else (
        let digit = n mod 10 in
        loop (n / 10);
        putc choices.[digit])
    in
    if n < 0
    then (
      putc '-';
      loop (-n))
    else if n = 0
    then putc '0'
    else loop n
;;

let () =
  let open Fixed.O in
  let a = !1 / !2 in
  let s = Fixed.sin a * !1000 in
  let c = Fixed.cos a * !1000 in
  print_int (Fixed.to_int s);
  putc ' ';
  print_int (Fixed.to_int c)
;;
