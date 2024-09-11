open Ti84ce

let project angle (x, y, z) =
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
    List.iter edges ~f:(fun (i, j) ->
      let x1, y1 = project angle (List.nth_exn vertices i) in
      let x2, y2 = project angle (List.nth_exn vertices j) in
      Graphx.line x1 y1 x2 y2)
;;

let draw_text =
  let s = "Vive OCaml !" in
  let x = (320 - Graphx.get_string_width s) / 2 in
  let y = 240 - 22 in
  fun () -> Graphx.print_string "Vive OCaml !" x y
;;

let () =
  let open Graphx in
  begin_ ();
  set_draw_buffer ();
  set_color 0xf0;
  set_text_fg_color 0xf0;
  let rec loop i =
    let n = 40 in
    let angle = Fixed.O.(!i / !n * Fixed.pi) in
    fill_screen 0x00;
    draw_cube angle;
    draw_text ();
    swap_draw ();
    if Ti84ce.Os.get_csc () = 0 then loop ((i + 1) mod (n / 2))
  in
  loop 0;
  end_ ()
;;
