open Ti84ce

let rotate_and_project angle (x, y, z) =
  let x, y =
    let open Fixed in
    let open Fixed.O in
    let x, z = !x, !z in
    let x_rot = (x * cos angle) - (z * sin angle) in
    let z_rot = (x * sin angle) + (z * cos angle) + !3 in
    let proj d = d / z_rot * !120 |> to_int in
    proj x_rot, proj !y
  in
  x + 160, y + 120
;;

let draw_cube =
  let vertices =
    [ -1, -1, -1; -1, -1, 1; -1, 1, -1; -1, 1, 1; 1, -1, -1; 1, -1, 1; 1, 1, -1; 1, 1, 1 ]
  in
  let edges =
    [ 0, 1; 0, 2; 0, 4; 1, 3; 1, 5; 2, 3; 2, 6; 3, 7; 4, 5; 4, 6; 5, 7; 6, 7 ]
  in
  fun angle ->
    let project = rotate_and_project angle in
    List.iter edges ~f:(fun (i, j) ->
      let x1, y1 = project (List.nth_exn vertices i) in
      let x2, y2 = project (List.nth_exn vertices j) in
      Graphx.line x1 y1 x2 y2)
;;

let () =
  let open Graphx in
  begin_ ();
  set_draw_buffer ();
  let rec loop i =
    let n = 100 in
    let i = i mod n in
    let angle = Fixed.O.(!i / !n * !2 * Fixed.pi) in
    fill_screen 0xff;
    draw_cube angle;
    swap_draw ();
    match Os.get_csc () with
    | 0 -> loop (i + 1)
    | _ -> ()
  in
  loop 0;
  end_ ()
;;
