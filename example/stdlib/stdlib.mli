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

(** {1 Exceptions} *)

(** Raise the given exception value *)
external raise : exn -> 'a = "%raise"

(** A faster version [raise] which does not record the backtrace.
    @since 4.02 *)
external raise_notrace : exn -> 'a = "%raise_notrace"

(** Raise exception [Invalid_argument] with the given string. *)
val invalid_arg : string -> 'a

(** Raise exception [Failure] with the given string. *)
val failwith : string -> 'a

(** {1 Boolean operations} *)

(** The boolean negation. *)
external not : bool -> bool = "%boolnot"

(** The boolean 'and'. Evaluation is sequential, left-to-right:
    in [e1 && e2], [e1] is evaluated first, and if it returns [false],
    [e2] is not evaluated at all.
    Right-associative operator,  see {!Ocaml_operators} for more information. *)
external ( && ) : bool -> bool -> bool = "%sequand"

(** The boolean 'or'. Evaluation is sequential, left-to-right:
    in [e1 || e2], [e1] is evaluated first, and if it returns [true],
    [e2] is not evaluated at all.
    Right-associative operator,  see {!Ocaml_operators} for more information. *)
external ( || ) : bool -> bool -> bool = "%sequor"

(** {1 Debugging} *)

(** [__LOC__] returns the location at which this expression appears in
    the file currently being parsed by the compiler, with the standard
    error format of OCaml: "File %S, line %d, characters %d-%d".
    @since 4.02 *)
external __LOC__ : string = "%loc_LOC"

(** [__FILE__] returns the name of the file currently being
    parsed by the compiler.
    @since 4.02 *)
external __FILE__ : string = "%loc_FILE"

(** [__LINE__] returns the line number at which this expression
    appears in the file currently being parsed by the compiler.
    @since 4.02 *)
external __LINE__ : int = "%loc_LINE"

(** [__MODULE__] returns the module name of the file being
    parsed by the compiler.
    @since 4.02 *)
external __MODULE__ : string = "%loc_MODULE"

(** [__POS__] returns a tuple [(file,lnum,cnum,enum)], corresponding
    to the location at which this expression appears in the file
    currently being parsed by the compiler. [file] is the current
    filename, [lnum] the line number, [cnum] the character position in
    the line and [enum] the last character position in the line.
    @since 4.02 *)
external __POS__ : string * int * int * int = "%loc_POS"

(** [__FUNCTION__] returns the name of the current function or method, including
    any enclosing modules or classes.

    @since 4.12 *)
external __FUNCTION__ : string = "%loc_FUNCTION"

(** [__LOC_OF__ expr] returns a pair [(loc, expr)] where [loc] is the
    location of [expr] in the file currently being parsed by the
    compiler, with the standard error format of OCaml: "File %S, line
    %d, characters %d-%d".
    @since 4.02 *)
external __LOC_OF__ : 'a -> string * 'a = "%loc_LOC"

(** [__LINE_OF__ expr] returns a pair [(line, expr)], where [line] is the
    line number at which the expression [expr] appears in the file
    currently being parsed by the compiler.
    @since 4.02 *)
external __LINE_OF__ : 'a -> int * 'a = "%loc_LINE"

(** [__POS_OF__ expr] returns a pair [(loc,expr)], where [loc] is a
    tuple [(file,lnum,cnum,enum)] corresponding to the location at
    which the expression [expr] appears in the file currently being
    parsed by the compiler. [file] is the current filename, [lnum] the
    line number, [cnum] the character position in the line and [enum]
    the last character position in the line.
    @since 4.02 *)
external __POS_OF__ : 'a -> (string * int * int * int) * 'a = "%loc_POS"

(** {1 Composition operators} *)

(** Reverse-application operator: [x |> f |> g] is exactly equivalent
    to [g (f (x))].
    Left-associative operator, see {!Ocaml_operators} for more information.
    @since 4.01 *)
external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"

(** Application operator: [g @@ f @@ x] is exactly equivalent to
    [g (f (x))].
    Right-associative operator, see {!Ocaml_operators} for more information.
    @since 4.01 *)
external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"

external ( = ) : int -> int -> bool = "%equal"
external ( <> ) : int -> int -> bool = "%notequal"
external ( < ) : int -> int -> bool = "%lessthan"
external ( > ) : int -> int -> bool = "%greaterthan"
external ( <= ) : int -> int -> bool = "%lessequal"
external ( >= ) : int -> int -> bool = "%greaterequal"

(** {1 Integer arithmetic} *)

(** Integers are [Sys.int_size] bits wide.
    All operations are taken modulo 2{^ [Sys.int_size]}.
    They do not fail on overflow. *)

(** Unary negation. You can also write [- e] instead of [~- e].
    Unary operator, see {!Ocaml_operators} for more information. *)
external ( ~- ) : int -> int = "%negint"

(** Unary addition. You can also write [+ e] instead of [~+ e].
    Unary operator, see {!Ocaml_operators} for more information.
    @since 3.12 *)
external ( ~+ ) : int -> int = "%identity"

(** [succ x] is [x + 1]. *)
external succ : int -> int = "%succint"

(** [pred x] is [x - 1]. *)
external pred : int -> int = "%predint"

(** Integer addition.
    Left-associative operator, see {!Ocaml_operators} for more information. *)
external ( + ) : int -> int -> int = "%addint"

(** Integer subtraction.
    Left-associative operator, , see {!Ocaml_operators} for more information. *)
external ( - ) : int -> int -> int = "%subint"

(** Integer multiplication.
    Left-associative operator, see {!Ocaml_operators} for more information. *)
external ( * ) : int -> int -> int = "%mulint"

(** Integer division.
    Integer division rounds the real quotient of its arguments towards zero.
    More precisely, if [x >= 0] and [y > 0], [x / y] is the greatest integer
    less than or equal to the real quotient of [x] by [y].  Moreover,
    [(- x) / y = x / (- y) = - (x / y)].
    Left-associative operator, see {!Ocaml_operators} for more information.

    @raise Division_by_zero if the second argument is 0. *)
external ( / ) : int -> int -> int = "%divint"

(** Integer remainder.  If [y] is not zero, the result
    of [x mod y] satisfies the following properties:
    [x = (x / y) * y + x mod y] and
    [abs(x mod y) <= abs(y) - 1].
    If [y = 0], [x mod y] raises [Division_by_zero].
    Note that [x mod y] is negative only if [x < 0].
    Left-associative operator, see {!Ocaml_operators} for more information.

    @raise Division_by_zero if [y] is zero. *)
external ( mod ) : int -> int -> int = "%modint"

(** [abs x] is the absolute value of [x]. On [min_int] this
    is [min_int] itself and thus remains negative. *)
val abs : int -> int

(** The greatest representable integer. *)
val max_int : int

(** The smallest representable integer. *)
val min_int : int

(** {2 Bitwise operations} *)

(** Bitwise logical and.
    Left-associative operator, see {!Ocaml_operators} for more information. *)
external ( land ) : int -> int -> int = "%andint"

(** Bitwise logical or.
    Left-associative operator, see {!Ocaml_operators} for more information. *)
external ( lor ) : int -> int -> int = "%orint"

(** Bitwise logical exclusive or.
    Left-associative operator, see {!Ocaml_operators} for more information. *)
external ( lxor ) : int -> int -> int = "%xorint"

(** Bitwise logical negation. *)
val lnot : int -> int

(** [n lsl m] shifts [n] to the left by [m] bits.
    The result is unspecified if [m < 0] or [m > Sys.int_size].
    Right-associative operator, see {!Ocaml_operators} for more information. *)
external ( lsl ) : int -> int -> int = "%lslint"

(** [n lsr m] shifts [n] to the right by [m] bits.
    This is a logical shift: zeroes are inserted regardless of
    the sign of [n].
    The result is unspecified if [m < 0] or [m > Sys.int_size].
    Right-associative operator, see {!Ocaml_operators} for more information. *)
external ( lsr ) : int -> int -> int = "%lsrint"

(** [n asr m] shifts [n] to the right by [m] bits.
    This is an arithmetic shift: the sign bit of [n] is replicated.
    The result is unspecified if [m < 0] or [m > Sys.int_size].
    Right-associative operator, see {!Ocaml_operators} for more information. *)
external ( asr ) : int -> int -> int = "%asrint"

(** {1 String operations}

    More string operations are provided in module {!String}. *)

(** String concatenation.
    Right-associative operator, see {!Ocaml_operators} for more information.

    @raise Invalid_argument
      if the result is longer then
      than {!Sys.max_string_length} bytes. *)
val ( ^ ) : string -> string -> string

(** {1 Unit operations} *)

(** Discard the value of its argument and return [()].
    For instance, [ignore(f x)] discards the result of
    the side-effecting function [f].  It is equivalent to
    [f x; ()], except that the latter may generate a
    compiler warning; writing [ignore(f x)] instead
    avoids the warning. *)
external ignore : 'a -> unit = "%ignore"

(** {1 Pair operations} *)

(** Return the first component of a pair. *)
external fst : 'a * 'b -> 'a = "%field0"

(** Return the second component of a pair. *)
external snd : 'a * 'b -> 'b = "%field1"

(** {1 List operations}

    More list operations are provided in module {!List}. *)

(** [l0 @ l1] appends [l1] to [l0]. Same function as {!List.append}.
    Right-associative operator, see {!Ocaml_operators} for more information.
    @since 5.1 this function is tail-recursive. *)
val ( @ ) : 'a list -> 'a list -> 'a list

(** {1 References} *)

(** The type of references (mutable indirection cells) containing
    a value of type ['a]. *)
type 'a ref = { mutable contents : 'a }

(** Return a fresh reference containing the given value. *)
external ref : 'a -> 'a ref = "%makemutable"

(** [!r] returns the current contents of reference [r].
    Equivalent to [fun r -> r.contents].
    Unary operator, see {!Ocaml_operators} for more information. *)
external ( ! ) : 'a ref -> 'a = "%field0"

(** [r := a] stores the value of [a] in reference [r].
    Equivalent to [fun r v -> r.contents <- v].
    Right-associative operator, see {!Ocaml_operators} for more information. *)
external ( := ) : 'a ref -> 'a -> unit = "%setfield0"

(** Increment the integer contained in the given reference.
    Equivalent to [fun r -> r := succ !r]. *)
external incr : int ref -> unit = "%incr"

(** Decrement the integer contained in the given reference.
    Equivalent to [fun r -> r := pred !r]. *)
external decr : int ref -> unit = "%decr"

(** {1 Result type} *)

(** @since 4.03 *)
type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

(** {1 Operations on format strings} *)

(** Format strings are character strings with special lexical conventions
    that defines the functionality of formatted input/output functions. Format
    strings are used to read data with formatted input functions from module
    {!Scanf} and to print data with formatted output functions from modules
    {!Printf} and {!Format}.

    Format strings are made of three kinds of entities:
    - {e conversions specifications}, introduced by the special character ['%']
      followed by one or more characters specifying what kind of argument to
      read or print,
    - {e formatting indications}, introduced by the special character ['@']
      followed by one or more characters specifying how to read or print the
      argument,
    - {e plain characters} that are regular characters with usual lexical
      conventions. Plain characters specify string literals to be read in the
      input or printed in the output.

    There is an additional lexical rule to escape the special characters ['%']
    and ['@'] in format strings: if a special character follows a ['%']
    character, it is treated as a plain character. In other words, ["%%"] is
    considered as a plain ['%'] and ["%@"] as a plain ['@'].

    For more information about conversion specifications and formatting
    indications available, read the documentation of modules {!Scanf},
    {!Printf} and {!Format}. *)

(** Format strings have a general and highly polymorphic type
    [('a, 'b, 'c, 'd, 'e, 'f) format6].
    The two simplified types, [format] and [format4] below are
    included for backward compatibility with earlier releases of
    OCaml.

    The meaning of format string type parameters is as follows:

    - ['a] is the type of the parameters of the format for formatted output
      functions ([printf]-style functions);
      ['a] is the type of the values read by the format for formatted input
      functions ([scanf]-style functions).

    - ['b] is the type of input source for formatted input functions and the
      type of output target for formatted output functions.
      For [printf]-style functions from module {!Printf}, ['b] is typically
      [out_channel];
      for [printf]-style functions from module {!Format}, ['b] is typically
      {!type:Format.formatter};
      for [scanf]-style functions from module {!Scanf}, ['b] is typically
      {!Scanf.Scanning.in_channel}.

    Type argument ['b] is also the type of the first argument given to
    user's defined printing functions for [%a] and [%t] conversions,
    and user's defined reading functions for [%r] conversion.

    - ['c] is the type of the result of the [%a] and [%t] printing
      functions, and also the type of the argument transmitted to the
      first argument of [kprintf]-style functions or to the
      [kscanf]-style functions.

    - ['d] is the type of parameters for the [scanf]-style functions.

    - ['e] is the type of the receiver function for the [scanf]-style functions.

    - ['f] is the final result type of a formatted input/output function
      invocation: for the [printf]-style functions, it is typically [unit];
      for the [scanf]-style functions, it is typically the result type of the
      receiver function. *)

external putc : char -> unit = "caml_putc"
external getc : unit -> char = "caml_getc"

module Int = Stdlib__Int
module String = Stdlib__String
module List = Stdlib__List
module Char = Stdlib__Char
module Bytes = Stdlib__Bytes
