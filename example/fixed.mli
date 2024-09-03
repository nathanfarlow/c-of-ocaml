type t

val of_int : int -> t
val to_int : t -> int
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t
val ( = ) : t -> t -> bool
val ( < ) : t -> t -> bool
val ( > ) : t -> t -> bool
