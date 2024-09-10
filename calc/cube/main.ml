open Ti84ce

let go f =
  let n = 100 in
  let mid = 120 in
  let width = 320 in
  let last = ref (0, mid) in
  (* Graphx.swap_draw (); *)
  for t = 0 to n do
    let open Fixed in
    let pi = O.(!314 / !100) in
    let t = O.(!t / !100 * !2 * pi) in
    let s = f t in
    let x1, y1 = !last in
    let x2, y2 = O.(t * !width / !2 / pi, !mid - (s * !mid)) in
    let x2, y2 = to_int x2, to_int y2 in
    last := x2, y2;
    Graphx.line x1 y1 x2 y2;
    ()
  done
;;

(* Graphx.swap_draw () *)

let () =
  let open Graphx in
  begin_ ();
  set_draw_buffer ();
  go Fixed.sin;
  go Fixed.cos;
  Graphx.swap_draw ();
  let rec wait () = if Os.get_csc () = 0 then wait () else () in
  wait ();
  end_ ()
;;
