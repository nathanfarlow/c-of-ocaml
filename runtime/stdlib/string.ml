(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

external length : string -> int = "%string_length"
external unsafe_get : string -> int -> char = "%string_unsafe_get"

let get s i =
  if i < 0 || i >= length s then raise (Invalid_argument "String.get") else unsafe_get s i
;;

let iter s ~f =
  let len = length s in
  for i = 0 to len - 1 do
    f (unsafe_get s i)
  done
;;

type t = string

let concat ?(sep = "") l =
  match l with
  | [] -> ""
  | [ x ] -> x
  | x :: xs -> List.fold_left ~f:(fun acc s -> acc ^ sep ^ s) x xs
;;

let make n c =
  let s = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.unsafe_set s i c
  done;
  Bytes.unsafe_to_string s
;;

let sub s ~pos ~len =
  let bytes = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.unsafe_set bytes i (unsafe_get s (pos + i))
  done;
  Bytes.unsafe_to_string bytes
;;

let lsplit2 s ~on =
  let len = length s in
  let rec find i =
    if i >= len
    then None
    else if Char.equal (unsafe_get s i) on
    then Some i
    else find (i + 1)
  in
  match find 0 with
  | None -> None
  | Some idx ->
    let left = sub s ~pos:0 ~len:idx in
    let right = sub s ~pos:(idx + 1) ~len:(len - idx - 1) in
    Some (left, right)
;;

let strip s =
  let len = length s in
  let rec find_start i =
    if i >= len
    then i
    else if Char.is_whitespace (unsafe_get s i)
    then find_start (i + 1)
    else i
  in
  let rec find_end i =
    if i <= 0
    then 0
    else if Char.is_whitespace (unsafe_get s (i - 1))
    then find_end (i - 1)
    else i
  in
  let start = find_start 0 in
  let stop = find_end len in
  if start >= stop then "" else sub s ~pos:start ~len:(stop - start)
;;

let is_empty s = length s = 0

let equal s1 s2 =
  let len1 = length s1 in
  let len2 = length s2 in
  if len1 <> len2
  then false
  else (
    let rec loop i =
      if i >= len1
      then true
      else if not (Char.equal (unsafe_get s1 i) (unsafe_get s2 i))
      then false
      else loop (i + 1)
    in
    loop 0)
;;

let compare s1 s2 =
  let len1 = length s1 in
  let len2 = length s2 in
  let min_len = if len1 < len2 then len1 else len2 in
  let rec loop i =
    if i >= min_len
    then if len1 < len2 then -1 else if len1 > len2 then 1 else 0
    else (
      let c = Char.compare (unsafe_get s1 i) (unsafe_get s2 i) in
      if c <> 0 then c else loop (i + 1))
  in
  loop 0
;;
