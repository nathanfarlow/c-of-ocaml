open Ti84ce

let go () =
  let t = ref 0 in
  let rec loop () =
    if Os.get_csc () <> 0
    then ()
    else (
      Graphx.swap_draw ();
      incr t;
      let t = !t in
      let open Fixed in
      let s, c = O.(sin (!t / !100) * !100, cos (!t / !100) * !100) in
      let s, c = to_int s, to_int c in
      let center_x = 160 in
      let center_y = 120 in
      Graphx.line center_x center_y (center_x + s) (center_y + c);
      loop ())
  in
  loop ()
;;

let () =
  let open Graphx in
  begin_ ();
  set_draw_buffer ();
  go ();
  end_ ()
;;
