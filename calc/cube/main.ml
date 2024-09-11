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
  List.iter edges ~f:draw_edge
;;

let pi = Fixed.O.(!22 / !7)
let rec wait () = if Os.get_csc () = 0 then wait () else ()

let () =
  let open Graphx in
  begin_ ();
  Graphx.set_draw_buffer ();
  for i = 0 to 100 do
    let angle = Fixed.O.(!i / !100 * !2 * pi) in
    Graphx.fill_screen 0xff;
    draw_cube angle;
    Graphx.swap_draw ()
  done;
  end_ ()
;;
