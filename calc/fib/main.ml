open! Ti84ce.Os

let () =
  (*   let rec loop () = *)
  (*     let _ = String.make 1 'a' in *)
  (*     if get_csc () = 0 then loop () else () *)
  (*   in *)
  (*   loop () *)
  (* ;; *)
  clr_home ();
  for i = 0 to 200 do
    put_str_full (Int.to_string i);
    put_str_full " "
  done
;;

(* let rec wait () = if get_csc () = 0 then wait () else () in *)
(* wait () *)
