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

  let rec pow x n = if n = 0 then of_int 1 else O.( * ) x (pow x (n - 1))

  let sin x =
    let open! O in
    x - (pow x 3 / !6) + (pow x 5 / !120)
  ;;

  let cos x =
    let open! O in
    of_int 1 - (pow x 2 / !2) + (pow x 4 / !24)
  ;;
end

include M (struct
    type t = int

    let fractional_bits = 4
    let of_int x = x lsl fractional_bits

    let to_int x =
      let scale = 1 lsl fractional_bits in
      (x + (scale / 2)) lsr fractional_bits
    ;;

    module O = struct
      let mul a b rshift =
        let split_12 x =
          let low = x land 0xFFF in
          let high = (x lsr 12) land 0xFFF in
          high, low
        in
        let mul_12 x y = x * y land 0xFFFFFF in
        let a_high, a_low = split_12 a in
        let b_high, b_low = split_12 b in
        let p1 = mul_12 a_low b_low in
        let p2 = mul_12 a_low b_high in
        let p3 = mul_12 a_high b_low in
        let p4 = mul_12 a_high b_high in
        let lower = p1 in
        let middle = (p2 + p3) land 0xFFFFFF in
        let upper = p4 in
        let result_low = (lower + ((middle land 0xFFF) lsl 12)) land 0xFFFFFF in
        let result_high = (upper + (middle lsr 12)) land 0xFFFFFF in
        let shifted_result =
          let shifted_low = (result_low lsr rshift) land 0xFFFFFF in
          let shifted_high =
            ((result_high land ((1 lsl rshift) - 1)) lsl (24 - rshift)) land 0xFFFFFF
          in
          shifted_low lor shifted_high land 0xFFFFFF
        in
        shifted_result
      ;;

      let ( + ) x y = x + y
      let ( - ) x y = x - y
      let ( * ) x y = mul x y fractional_bits
      let ( / ) x y = ((x lsl fractional_bits) + (y / 2)) / y
      let ( = ) x y = x = y
      let ( < ) x y = x < y
      let ( > ) x y = x > y
      let ( <= ) x y = x <= y
      let ( >= ) x y = x >= y
      let ( ! ) = of_int
    end
  end)
