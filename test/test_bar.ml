open! Core

module M = struct
  type t = int

  let scale = 16
  let of_int x = x * scale
  let to_int x = if x >= 0 then (x + (scale / 2)) / scale else (x - (scale / 2)) / scale

  module O = struct
    let ( + ) x y = x + y
    let ( - ) x y = x - y
    let ( / ) x y = ((x * scale) + (y / 2)) / y
    let ( ! ) x = x * scale
    let ( ~- ) x = -x
    let ( < ) x y = x < y
    let ( * ) x y = Stdlib.(((x * y) + (scale / 2)) / scale)
  end

  let sin =
    let open O in
    let pi = !31 / !10 in
    let pi2 = pi * pi in
    fun x ->
      let bhaskara x =
        let num = !16 * x * (pi - x) in
        let den = (!5 * pi2) - (!4 * x * (pi - x)) in
        num / den
      in
      if x < pi then bhaskara x else -bhaskara (x - pi)
  ;;
end

let%expect_test _ =
  let open M.O in
  let a = !45 in
  let b = !30 in
  let c = a + b in
  printf "%d\n" (M.to_int c);
  [%expect {| 75 |}]
;;

let%expect_test _ =
  let open M.O in
  let a = !65 in
  let b = !30 in
  let c = a / b in
  printf "%d\n" (M.to_int c);
  [%expect {| 2 |}]
;;

let%expect_test _ =
  let open M.O in
  let a = !2 in
  let b = !3 in
  let c = a * b in
  printf "%d\n" (M.to_int c);
  [%expect {| 6 |}]
;;

let%expect_test _ =
  let open M.O in
  let a = !2 in
  let b = !5 in
  let c = a - b in
  printf "%d\n" (M.to_int c);
  [%expect {| -3 |}]
;;

let%expect_test _ =
  let open M.O in
  let a = !1 / !2 in
  let b = !0 - !5 in
  let c = a * b in
  printf "%d\n" (M.to_int c);
  [%expect {| -2 |}]
;;

let%expect_test _ =
  let open M.O in
  let a = !1 / !2 in
  let b = !0 - !5 in
  let c = b / a in
  printf "%d\n" (M.to_int c);
  [%expect {| -10 |}]
;;

let%expect_test _ =
  let open M.O in
  let a = !2 in
  let b = !0 - !5 in
  let c = b / a in
  printf "%d\n" (M.to_int c);
  [%expect {| -2 |}]
;;

let%expect_test _ =
  let open M.O in
  let a = !2 in
  let b = !5 in
  let c = b / a in
  printf "%d\n" (M.to_int c);
  [%expect {| 3 |}]
;;

let%expect_test _ =
  let open M.O in
  let a = !5 / !10 in
  let b = M.sin a * !100 in
  printf "%d\n" (M.to_int b);
  [%expect {|
    50 |}]
;;

let%expect_test _ =
  let open M.O in
  let a = !5 in
  let b = M.sin a * !100 in
  printf "%d\n" (M.to_int b);
  [%expect {|
    -94 |}]
;;
