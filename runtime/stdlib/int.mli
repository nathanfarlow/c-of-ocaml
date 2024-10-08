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

(** Integer values.

    Integers are {!Sys.int_size} bits wide and use two's complement
    representation. All operations are taken modulo
    2{^ [Sys.int_size]}. They do not fail on overflow.

    @since 4.08 *)

(** {1:ints Integers} *)

(** The type for integer values. *)
type t = int

(** [zero] is the integer [0]. *)
val zero : int

(** [one] is the integer [1]. *)
val one : int

(** [minus_one] is the integer [-1]. *)
val minus_one : int

(** [neg x] is [~-x]. *)
external neg : int -> int = "%negint"

(** [add x y] is the addition [x + y]. *)
external add : int -> int -> int = "%addint"

(** [sub x y] is the subtraction [x - y]. *)
external sub : int -> int -> int = "%subint"

(** [mul x y] is the multiplication [x * y]. *)
external mul : int -> int -> int = "%mulint"

(** [div x y] is the division [x / y]. See {!Stdlib.( / )} for details. *)
external div : int -> int -> int = "%divint"

(** [rem x y] is the remainder [x mod y]. See {!Stdlib.( mod )} for details. *)
external rem : int -> int -> int = "%modint"

(** [succ x] is [add x 1]. *)
external succ : int -> int = "%succint"

(** [pred x] is [sub x 1]. *)
external pred : int -> int = "%predint"

(** [abs x] is the absolute value of [x]. That is [x] if [x] is positive
    and [neg x] if [x] is negative. {b Warning.} This may be negative if
    the argument is {!min_int}. *)
val abs : int -> int

(** [max_int] is the greatest representable integer,
    [2]{^ [Sys.int_size - 1]}[-1]. *)
val max_int : int

(** [min_int] is the smallest representable integer,
    [-2]{^ [Sys.int_size - 1]}. *)
val min_int : int

(** [logand x y] is the bitwise logical and of [x] and [y]. *)
external logand : int -> int -> int = "%andint"

(** [logor x y] is the bitwise logical or of [x] and [y]. *)
external logor : int -> int -> int = "%orint"

(** [logxor x y] is the bitwise logical exclusive or of [x] and [y]. *)
external logxor : int -> int -> int = "%xorint"

(** [lognot x] is the bitwise logical negation of [x]. *)
val lognot : int -> int

(** [shift_left x n] shifts [x] to the left by [n] bits. The result
    is unspecified if [n < 0] or [n > ]{!Sys.int_size}. *)
external shift_left : int -> int -> int = "%lslint"

(** [shift_right x n] shifts [x] to the right by [n] bits. This is an
    arithmetic shift: the sign bit of [x] is replicated and inserted
    in the vacated bits. The result is unspecified if [n < 0] or
    [n > ]{!Sys.int_size}. *)
external shift_right : int -> int -> int = "%asrint"

(** [shift_right x n] shifts [x] to the right by [n] bits. This is a
    logical shift: zeroes are inserted in the vacated bits regardless
    of the sign of [x]. The result is unspecified if [n < 0] or
    [n > ]{!Sys.int_size}. *)
external shift_right_logical : int -> int -> int = "%lsrint"

(** {1:preds Predicates and comparisons} *)

(** [equal x y] is [true] if and only if [x = y]. *)
val equal : int -> int -> bool

(** [compare x y] is {!Stdlib.compare}[ x y] but more efficient. *)
val compare : int -> int -> int

(** Return the smaller of the two arguments.
    @since 4.13 *)
val min : int -> int -> int

(** Return the greater of the two arguments.
    @since 4.13 *)
val max : int -> int -> int

(** {1:convert Converting} *)

(*
   val of_string : string -> int option
   (** [of_string s] is [Some s] if [s] can be parsed to an integer
   in the range representable by the type [int] (note that this
   depends on {!Sys.int_size}) and [None] otherwise.

   The string may start with an optional ['-'] or ['+'] sign, and may
   be followed by an optional prefix that specifies the base in which
   the number is expressed. If there is not prefix or if the prefix
   is [0u] or [0U] it is expressed in decimal. If the prefix is [0x]
   or [0X] it is expressed in hexadecimal. If the prefix is [0o] or
   [0O] it is expressed in octal. If the prefix is [0b] or [0B] it is
   expressed in binary.

   When the [0u] or [0U] prefix is used, the represented number may
   exceed {!max_int} or {!min_int} in which case it wraps around
   modulo 2{^ [Sys.int_size]} like arithmetic operations do.

   The ['_'] (underscore) character can appear anywhere between two
   digits of the number. *)
*)

(** [to_string x] is the written representation of [x] in decimal. *)
val to_string : int -> string
