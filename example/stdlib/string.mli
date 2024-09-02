(** [length s] is the length (number of bytes/characters) of [s]. *)
external length : string -> int = "%string_length"

(** [get s i] is the character at index [i] in [s]. This is the same
    as writing [s.[i]]. *)
val get : string -> int -> char

val iter : string -> f:(char -> unit) -> unit
