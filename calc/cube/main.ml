open Ti84ce

[@@@warning "-32-26-27"]

let vertices =
  [ -1, -1, -1; -1, -1, 1; -1, 1, -1; -1, 1, 1; 1, -1, -1; 1, -1, 1; 1, 1, -1; 1, 1, 1 ]
;;

let edges = [ 0, 1; 0, 2; 0, 4; 1, 3; 1, 5; 2, 3; 2, 6; 3, 7; 4, 5; 4, 6; 5, 7; 6, 7 ]

let rotate_and_project x y z angle =
  let open Fixed.O in
  let x_rot = (x * Fixed.cos angle) - (z * Fixed.sin angle) in
  let z_rot = (x * Fixed.sin angle) + (z * Fixed.cos angle) in
  let z_rot = z_rot + !2 in
  let x_proj = x_rot / z_rot in
  let y_proj = y / z_rot in
  (x_proj * !160) + !120, (y_proj * !120) + !120
;;

let draw_cube angle =
  let open Fixed.O in
  let draw_edge (i, j) =
    let i = List.nth_exn vertices i in
    let j = List.nth_exn vertices j in
    let x1, y1 =
      let ix, iy, iz = i in
      let ix = !ix in
      let iy = !iy in
      let iz = !iz in
      rotate_and_project ix iy iz angle
    in
    let x2, y2 =
      let jx, jy, jz = j in
      let jx = !jx in
      let jy = !jy in
      let jz = !jz in
      rotate_and_project jx jy jz angle
    in
    Graphx.line (Fixed.to_int x1) (Fixed.to_int y1) (Fixed.to_int x2) (Fixed.to_int y2)
  in
  (* List.iter edges ~f:draw_edge *)
  ()
;;

let pi = Fixed.O.(!22 / !7)
let rec wait () = if Os.get_csc () = 0 then wait () else ()

(* let go () = *)
(*   let last = ref (0, 0) in *)
(*   for i = 0 to 100 do *)
(*     let angle = Fixed.O.(!i / !100 * !2 * pi) in *)
(*     let x, y = Fixed.sin angle, Fixed.cos angle in *)
(*     let x, y = Fixed.O.((x * !160) + !160, (y * !120) + !120) in *)
(*     let x, y = Fixed.to_int x, Fixed.to_int y in *)
(*     Graphx.line (fst !last) (snd !last) x y; *)
(*     last := x, y; *)
(*     let open Fixed.O in *)
(*     let _a, _b = rotate_and_project !0 !1 !1 angle in *)
(*     dbg_print (Fixed.to_int _b |> Int.to_string) *)
(*   done; *)
(*   wait () *)
(* ;; *)

let what () =
  let vertices =
    [ -1, -1, -1; -1, -1, 1; -1, 1, -1; -1, 1, 1; 1, -1, -1; 1, -1, 1; 1, 1, -1; 1, 1, 1 ]
  in
  (* let edges = *)
  (*   [ 0, 1; 0, 2; 0, 4; 1, 3; 1, 5; 2, 3; 2, 6; 3, 7; 4, 5; 4, 6; 5, 7; 6, 7 ] *)
  (* in *)
  (* List.iter edges ~f:(fun (v1, v2) -> *)
  (*   dbg_print (Int.to_string v1 ^ ", " ^ Int.to_string v2)); *)
  List.iter vertices ~f:(fun (x, y, z) ->
    (* dbg_print (Int.to_string x ^ ", " ^ Int.to_string y ^ ", " ^ Int.to_string z)); *)
    dbg_print (Int.to_string x))
;;

(* for i = 0 to 100 do *)
(*   let angle = Fixed.O.(!i / !100 * !2 * pi) in *)
(*   () *)
(* done *)
let () =
  let open Graphx in
  begin_ ();
  what ();
  end_ ()
;;

(* Entry point *)
(* let () = main_loop Fixed.O.(!0) *)

(* let go f = *)
(*   let n = 100 in *)
(*   let mid = 120 in *)
(*   let width = 320 in *)
(*   let last = ref (0, mid) in *)
(*   (\* Graphx.swap_draw (); *\) *)
(*   for t = 0 to n do *)
(*     let open Fixed in *)
(*     let pi = O.(!314 / !100) in *)
(*     let t = O.(!t / !100 * !2 * pi) in *)
(*     let s = f t in *)
(*     let x1, y1 = !last in *)
(*     let x2, y2 = O.(t * !width / !2 / pi, !mid - (s * !mid)) in *)
(*     let x2, y2 = to_int x2, to_int y2 in *)
(*     last := x2, y2; *)
(*     Graphx.line x1 y1 x2 y2; *)
(*     () *)
(*   done *)
(* ;; *)

(* (\* Graphx.swap_draw () *\) *)

(* let () = *)
(*   let open Graphx in *)
(*   begin_ (); *)
(*   (\* set_draw_buffer (); *\) *)
(*   go Fixed.sin; *)
(*   go Fixed.cos; *)
(*   (\* Graphx.swap_draw (); *\) *)
(*   let rec wait () = if Os.get_csc () = 0 then wait () else () in *)
(*   wait (); *)
(*   end_ () *)
(* ;; *)
