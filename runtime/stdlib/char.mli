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

(** Character operations. *)

(** Return the ASCII code of the argument. *)
external code : char -> int = "%identity"

(** Return a string representing the given character,
    with special characters escaped following the lexical conventions
    of OCaml.
    All characters outside the ASCII printable range (32..126) are
    escaped, as well as backslash, double-quote, and single-quote. *)
val escaped : char -> string

(** Convert the given character to its equivalent lowercase character,
    using the US-ASCII character set.
    @since 4.03 *)
val lowercase_ascii : char -> char

(** Convert the given character to its equivalent uppercase character,
    using the US-ASCII character set.
    @since 4.03 *)
val uppercase_ascii : char -> char

(** An alias for the type of characters. *)
type t = char

(** The comparison function for characters, with the same specification as
    {!Stdlib.compare}.  Along with the type [t], this function [compare]
    allows the module [Char] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)
val compare : t -> t -> int

(** The equal function for chars.
    @since 4.03 *)
val equal : t -> t -> bool

(** Return the character with the given ASCII code.
    @raise Invalid_argument if the argument is
                            outside the range 0--255. *)
val of_int : int -> char
