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
    x - (pow x 3 / !6) + (pow x 5 / !120) - (pow x 7 / !5040)
  ;;

  let cos x =
    let open! O in
    of_int 1 - (pow x 2 / !2) + (pow x 4 / !24) - (pow x 6 / !720)
  ;;
end

include M (struct
    type t = int

    let fractional_bits = 4
    let of_int x = x lsl fractional_bits
    let to_int x = (x + (fractional_bits lsr 2)) lsr fractional_bits

    module O = struct
      let extract_8bit x shift = (x lsr shift) land 0xFF
      let mul_8x8 a b = a land 0xFF * (b land 0xFF)

      let fixed_point_mul x y fractional_bits =
        let x0 = extract_8bit x 0 in
        let x1 = extract_8bit x 8 in
        let x2 = extract_8bit x 16 in
        let y0 = extract_8bit y 0 in
        let y1 = extract_8bit y 8 in
        let y2 = extract_8bit y 16 in
        let r0 = mul_8x8 x0 y0 in
        let r1 = mul_8x8 x1 y0 + mul_8x8 x0 y1 in
        let r2 = mul_8x8 x2 y0 + mul_8x8 x1 y1 + mul_8x8 x0 y2 in
        let r3 = mul_8x8 x2 y1 + mul_8x8 x1 y2 in
        let r4 = mul_8x8 x2 y2 in
        (* Combine the 48-bit result *)
        let high_bits = (r4 lsl 8) + r3 in
        let mid_bits = r2 in
        let low_bits = (r1 lsl 8) + (r0 lsr 8) in
        (* Shift right by fractional_bits *)
        let shifted_high = high_bits lsr fractional_bits in
        let shifted_mid =
          (mid_bits lsr fractional_bits) lor (high_bits lsl (24 - fractional_bits))
        in
        let shifted_low =
          (low_bits lsr fractional_bits) lor (mid_bits lsl (24 - fractional_bits))
        in
        (* Combine the final 24-bit result *)
        (shifted_high lsl 16) lor (shifted_mid land 0xFF00) lor (shifted_low land 0xFF)
      ;;

      (* result lsr (fractional_bits - 16) *)

      let ( + ) x y = x + y
      let ( - ) x y = x - y

      (* let ( * ) x y = (x * y) lsr fractional_bits *)
      let ( * ) x y = fixed_point_mul x y fractional_bits
      let ( / ) x y = ((x lsl fractional_bits) + (y / 2)) / y
      let ( = ) x y = x = y
      let ( < ) x y = x < y
      let ( > ) x y = x > y
      let ( <= ) x y = x <= y
      let ( >= ) x y = x >= y
      let ( ! ) = of_int
    end
  end)
