open! Ti84ce.Os

[@@@warning "-32"]

let rec fib n = if n = 0 then 0 else if n = 1 then 1 else fib (n - 1) + fib (n - 2)
let rec wait () = if get_csc () = 0 then wait () else ()

let () =
  let rec loop () =
    let _ = String.make 1 'a' in
    if get_csc () = 0 then loop () else ()
  in
  loop ()
;;
(* clr_home (); *)
(* for i = 0 to 200 do *)
(*   put_str_full (Int.to_string (fib i)); *)
(*   put_str_full " " *)
(* done; *)
(* wait () *)
