[@@@warning "-32"]

let print_endline s =
  String.iter s ~f:putc;
  putc '\n'
;;

let print_int n = Int.to_string n |> print_endline
let rec fib k = if k < 2 then 1 else fib (k - 1) + fib (k - 2)

let () =
  let n = 60 in
  print_int (fib n)
;;
