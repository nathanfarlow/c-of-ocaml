(* Test nested constant tuples - exercises GC safety during constant allocation *)

let print_int n = Int.to_string n |> Io.puts

(* Nested tuples - multiple allocating sub-constants *)
let nested = (1, 2), (3, 4), (5, 6)

(* Deeper nesting *)
let deep = ((1, 2), (3, 4)), ((5, 6), (7, 8))

(* Mixed with non-allocating *)
let mixed = 1, (2, 3), 4, (5, 6)

let () =
  let (a, b), (c, d), (e, f) = nested in
  print_int (a + b + c + d + e + f);
  let ((g, h), (i, j)), ((k, l), (m, n)) = deep in
  print_int (g + h + i + j + k + l + m + n);
  let p, (q, r), s, (t, u) = mixed in
  print_int (p + q + r + s + t + u);
  (* Force some allocations to potentially trigger GC *)
  for _ = 1 to 1000 do
    let _ = nested, deep, mixed in
    ()
  done;
  Io.puts "Tuple test passed!"
;;
