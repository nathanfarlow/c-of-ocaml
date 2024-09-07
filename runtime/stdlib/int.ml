(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = int

let zero = 0
let one = 1
let minus_one = -1

external neg : int -> int = "%negint"
external add : int -> int -> int = "%addint"
external sub : int -> int -> int = "%subint"
external mul : int -> int -> int = "%mulint"
external div : int -> int -> int = "%divint"
external rem : int -> int -> int = "%modint"
external succ : int -> int = "%succint"
external pred : int -> int = "%predint"

let abs x = if x >= 0 then x else -x
let max_int = -1 lsr 1
let min_int = max_int + 1

external logand : int -> int -> int = "%andint"
external logor : int -> int -> int = "%orint"
external logxor : int -> int -> int = "%xorint"

let lognot x = logxor x (-1)

external shift_left : int -> int -> int = "%lslint"
external shift_right : int -> int -> int = "%asrint"
external shift_right_logical : int -> int -> int = "%lsrint"

let equal : int -> int -> bool = ( = )

external compare : int -> int -> int = "%compare"

let min x y : t = if x <= y then x else y
let max x y : t = if x >= y then x else y

let to_string i =
  let rec aux n acc =
    if n = 0
    then acc
    else (
      let digit = abs (n mod 10) in
      let char = Char.of_int (digit + 48) in
      aux (n / 10) (char :: acc))
  in
  if i = 0
  then "0"
  else (
    let sign = if i < 0 then "-" else "" in
    sign ^ (aux i [] |> List.map ~f:(String.make 1) |> String.concat ~sep:""))
;;
