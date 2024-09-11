type t = int

let scale = 1 lsl 7
let of_int x = x * scale
let to_int x = if x >= 0 then (x + (scale / 2)) / scale else (x - (scale / 2)) / scale

module O = struct
  let ( + ) x y = x + y
  let ( - ) x y = x - y
  let ( / ) x y = ((x * scale) + (y / 2)) / y
  let ( ! ) = of_int
  let ( ~- ) x = -x
  let ( < ) x y = x < y
  let ( * ) x y = Stdlib.(((x * y) + (scale / 2)) / scale)
end

let pi = O.(!22 / !7)

let sin =
  let open O in
  let pi2 = pi * pi in
  fun x ->
    let num = !16 * x * (pi - x) in
    let den = (!5 * pi2) - (!4 * x * (pi - x)) in
    num / den
;;

let cos =
  let open O in
  let pi2 = pi / !2 in
  fun x -> sin (x + pi2)
;;
