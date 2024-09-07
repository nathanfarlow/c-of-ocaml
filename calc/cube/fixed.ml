module M (K : sig
    type t

    val of_int : int -> t
    val to_int : t -> int

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
  end) =
struct
  include K

  let factorial (n : int) =
    let rec factorial n = if n = 0 then 1 else n * factorial (n - 1) in
    factorial n |> of_int
  ;;

  let rec pow x n = if n = 0 then of_int 1 else O.( * ) x (pow x (n - 1))

  let sin x =
    let open! O in
    x - (pow x 3 / factorial 3) + (pow x 5 / factorial 5) - (pow x 7 / factorial 7)
  ;;

  let cos x =
    let open! O in
    !1 - (pow x 2 / factorial 2) + (pow x 4 / factorial 4) - (pow x 6 / factorial 6)
  ;;
end

include M (struct
    type t = int

    let fractional_bits = 8
    let scale = 1 lsl fractional_bits
    let of_int x = x lsl fractional_bits
    let to_int x = (x + (scale / 2)) lsr fractional_bits

    module O = struct
      let ( + ) x y = x + y
      let ( - ) x y = x - y
      let ( * ) x y = (x * y) lsr fractional_bits
      let ( / ) x y = ((x lsl fractional_bits) + (y / 2)) / y
      let ( = ) x y = x = y
      let ( < ) x y = x < y
      let ( > ) x y = x > y
      let ( <= ) x y = x <= y
      let ( >= ) x y = x >= y
      let ( ! ) = of_int
    end
  end)
