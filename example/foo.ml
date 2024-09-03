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

let rec fib k = if k < 2 then k else fib (k - 1) + fib (k - 2)

let () =
  print_endline "Fibonacci numbers:";
  for i = 0 to 40 do
    print_int (fib i);
    putc '\n'
  done
;;
