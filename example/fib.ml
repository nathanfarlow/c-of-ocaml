let f n =
  let rec loop i a b = if i = n then a else loop (i + 1) b (a + b) in
  loop 0 0 1
;;
