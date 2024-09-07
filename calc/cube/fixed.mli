type t

val of_int : int -> t
val to_int : t -> int
val sin : t -> t
val cos : t -> t

module O : sig
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( = ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val ( ! ) : int -> t
end
