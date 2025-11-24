(*
   Run `dune build` in this directory.

   The C file will be written to _build/default/example/main.c. For convenience,
   it will also be compiled to _build/default/example/main.c.exe.
*)

let print_int n = Int.to_string n |> Io.puts

let () =
  Io.puts "Hi! What's your name?";
  let name = Io.gets () in
  Io.puts ("Hello, " ^ name ^ "! Here are some Fibonacci numbers:");
  for i = 0 to 200 do
    print_int (Fib.f i)
  done
;;
