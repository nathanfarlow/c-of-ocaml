(* Sudoku solver adapted from https://blog.singleton.io/sudoku/ *)

external unsafe_chr : int -> char = "%identity"

let get = Bytes.unsafe_get
let set = Bytes.unsafe_set

let conflicts board pos =
  let digit = get board pos in
  let rec loop i =
    i < 81
    &&
    let dominated =
      pos / 9 = i / 9
      || pos mod 9 = i mod 9
      || (pos / 27 = i / 27 && pos mod 9 / 3 = i mod 9 / 3)
    in
    (pos <> i && dominated && Char.equal digit (get board i)) || loop (i + 1)
  in
  loop 0
;;

let print_board board =
  Io.putc '\n';
  for i = 0 to 80 do
    Io.putc (get board i);
    if (i + 1) mod 9 = 0 then Io.putc '\n'
  done;
  exit 0
;;

let rec solve board pos =
  if pos = 81
  then print_board board
  else if not (Char.equal (get board pos) '0')
  then solve board (pos + 1)
  else (
    for d = 1 to 9 do
      set board pos (unsafe_chr (48 + d));
      if not (conflicts board pos) then solve board (pos + 1)
    done;
    set board pos '0')
;;

let read_board () =
  let rec loop n acc = if n = 0 then acc else loop (n - 1) (acc ^ Io.gets ()) in
  Bytes.of_string (loop 9 "")
;;

let () = solve (read_board ()) 0
