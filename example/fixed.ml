let fractional_bits = 8
let scale = 1 lsl fractional_bits

type t = int

let of_int x = x lsl fractional_bits
let ( + ) x y = x + y
let ( - ) x y = x - y
let ( * ) x y = (x * y) lsr fractional_bits
let ( / ) x y = (x lsl fractional_bits) / y
let ( = ) x y = x = y
let ( < ) x y = x < y
let ( > ) x y = x > y
let to_int x = (x + (scale / 2)) lsr fractional_bits
