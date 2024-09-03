[@@@warning "-32"]

let print_endline s =
  String.iter s ~f:putc;
  putc '\n'
;;

let print_int n =
  let lookup = "0123456789" in
  let rec print_int n =
    if n < 10
    then putc lookup.[n]
    else (
      print_int (n / 10);
      putc lookup.[n mod 10])
  in
  if n < 0
  then (
    putc '-';
    print_int (-n))
  else print_int n
;;

let () =
  let open Fixed in
  let a = of_int 1 / of_int 3 in
  let b = of_int 10000 in
  print_int (to_int (a * b))
;;
