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
val get : string -> int -> char
val iter : string -> f:(char -> unit) -> unit

(** {1:concat Concatenating}

    {b Note.} The {!Stdlib.( ^ )} binary operator concatenates two
    strings. *)

val concat : ?sep:string -> string list -> string

(** [make n c] is a string of length [n] with each index holding the
    character [c].

    @raise Invalid_argument if [n < 0] or [n > ]{!Sys.max_string_length}. *)
val make : int -> char -> string

val sub : string -> pos:int -> len:int -> string
val lsplit2 : string -> on:char -> (string * string) option
val strip : string -> string
val is_empty : string -> bool
val equal : string -> string -> bool
val compare : string -> string -> int

type t = string
