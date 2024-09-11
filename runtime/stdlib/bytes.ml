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

external length : bytes -> int = "%bytes_length"
external unsafe_get : bytes -> int -> char = "%bytes_unsafe_get"
external unsafe_set : bytes -> int -> char -> unit = "%bytes_unsafe_set"
external create : int -> bytes = "caml_create_bytes"
external unsafe_to_string : bytes -> string = "%bytes_to_string"
external unsafe_of_string : string -> bytes = "%bytes_of_string"

external unsafe_blit : bytes -> int -> bytes -> int -> int -> unit = "caml_blit_bytes"
[@@noalloc]

let copy s =
  let len = length s in
  let r = create len in
  unsafe_blit s 0 r 0 len;
  r
;;

let of_string s = copy (unsafe_of_string s)
let to_string b = unsafe_to_string (copy b)

type t = bytes
