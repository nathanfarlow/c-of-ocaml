let print_int n = Int.to_string n |> Io.puts
let rec fib k = if k < 2 then 1 else fib (k - 1) + fib (k - 2)

let () =
  for i = 0 to 20 do
    print_int (fib i)
  done
;;
