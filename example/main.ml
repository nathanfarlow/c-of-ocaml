(*
   First, run `dune build`.

   The C file will be written to _build/default/example/main.c. For convenience,
   it will also be compiled to _build/default/example/main.c.exe.
*)

let print_int n = Int.to_string n |> Io.puts

let () =
  for i = 0 to 20 do
    print_int (Fib.f i)
  done
;;
