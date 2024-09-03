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

(** Return the length (number of bytes) of the argument. *)
external length : bytes -> int = "%bytes_length"

external unsafe_get : bytes -> int -> char = "%bytes_unsafe_get"
external unsafe_set : bytes -> int -> char -> unit = "%bytes_unsafe_set"
external unsafe_to_string : bytes -> string = "%bytes_to_string"
external unsafe_of_string : string -> bytes = "%bytes_of_string"

(** [create n] returns a new byte sequence of length [n]. The
    sequence is uninitialized and contains arbitrary bytes.
    @raise Invalid_argument if [n < 0] or [n > ]{!Sys.max_string_length}. *)
external create : int -> bytes = "caml_create_bytes"

(** Return a new byte sequence that contains the same bytes as the
    given string. *)
val of_string : string -> bytes

(** Return a new string that contains the same bytes as the given byte
    sequence. *)
val to_string : bytes -> string

type t = bytes
