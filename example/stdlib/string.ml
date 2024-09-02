external length : string -> int = "%string_length"
external unsafe_get : string -> int -> char = "%string_unsafe_get"

let get s i =
  if i < 0 || i >= length s then raise (Invalid_argument "String.get") else unsafe_get s i
;;

let iter s ~f =
  let len = length s in
  for i = 0 to len - 1 do
    f (unsafe_get s i)
  done
;;
