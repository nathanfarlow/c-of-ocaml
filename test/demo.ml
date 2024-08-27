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
  let f = "hello" in
  let _l = [ Some f; Some "world"; None ] in
  iter _l ~f:(function
    | None -> print_endline f
    | Some s -> print_endline s)
;;
