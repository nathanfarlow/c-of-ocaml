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
