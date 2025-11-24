external putc : char -> unit = "caml_putc"
external getc : unit -> char = "caml_getc"

let puts s =
  String.iter ~f:putc s;
  putc '\n'
;;

let gets () =
  let rec read_chars acc =
    match getc () with
    | '\n' -> List.rev acc
    | c -> read_chars (c :: acc)
  in
  let chars = read_chars [] in
  let len = List.length chars in
  let bytes = Bytes.create len in
  List.iteri ~f:(fun i c -> Bytes.unsafe_set bytes i c) chars;
  Bytes.to_string bytes
;;
