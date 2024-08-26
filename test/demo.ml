external print_endline : string -> unit = "weeeee"

type opt =
  | None
  | Some of string

let rec iter l ~f =
  match l with
  | [] -> ()
  | hd :: tl ->
    f hd;
    iter tl ~f
;;

let () =
  let _l = [ Some "hello"; Some "world"; None ] in
  iter _l ~f:(function
    | None -> print_endline "None"
    | Some s -> print_endline s)
;;
