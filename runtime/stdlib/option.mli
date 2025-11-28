type 'a t = 'a option =
  | None
  | Some of 'a

val some : 'a -> 'a option
val none : 'a option
val value : 'a option -> default:'a -> 'a
val some_if : bool -> 'a -> 'a option
val bind : 'a option -> f:('a -> 'b option) -> 'b option
val map : 'a option -> f:('a -> 'b) -> 'b option
val iter : 'a option -> f:('a -> unit) -> unit
val is_some : 'a option -> bool
val is_none : 'a option -> bool
