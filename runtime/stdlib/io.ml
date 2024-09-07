external putc : char -> unit = "caml_putc"
external getc : unit -> char = "caml_getc"

let puts s =
  String.iter ~f:putc s;
  putc '\n'
;;
