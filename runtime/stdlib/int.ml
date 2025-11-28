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

let rec pow base exp =
  if exp = 0
  then 1
  else if exp = 1
  then base
  else (
    let half = pow base (exp / 2) in
    if exp mod 2 = 0 then half * half else half * half * base)
;;

let of_string s =
  let len = String.length s in
  if len = 0
  then failwith "Int.of_string"
  else (
    let start, sign =
      if Char.equal (String.get s 0) '-'
      then 1, -1
      else if Char.equal (String.get s 0) '+'
      then 1, 1
      else 0, 1
    in
    let rec loop i acc =
      if i >= len
      then acc
      else (
        let c = String.get s i in
        if Char.is_digit c
        then loop (i + 1) ((acc * 10) + (Char.code c - 48))
        else failwith "Int.of_string")
    in
    sign * loop start 0)
;;
