open Ti84ce.Os

let () =
  clr_home ();
  put_str_full "Hello from OCaml!";
  let rec wait () = if get_csc () = 0 then wait () else () in
  wait ()
;;
