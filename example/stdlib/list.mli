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

(** List operations.

    Some functions are flagged as not tail-recursive.  A tail-recursive
    function uses constant stack space, while a non-tail-recursive function
    uses stack space proportional to the length of its list argument, which
    can be a problem with very long lists.  When the function takes several
    list arguments, an approximate formula giving stack usage (in some
    unspecified constant unit) is shown in parentheses.

    The above considerations can usually be ignored if your lists are not
    longer than about 10000 elements.

    The labeled version of this module can be used as described in the
    {!StdLabels} module. *)

(** An alias for the type of lists. *)
type 'a t = 'a list =
  | []
  | ( :: ) of 'a * 'a list (**)

(** Return the length (number of elements) of the given list. *)
val length : 'a list -> int

(** [is_empty l] is true if and only if [l] has no elements. It is equivalent to
    [compare_length_with l 0 = 0].
    @since 5.1 *)
val is_empty : 'a list -> bool

(** [cons x xs] is [x :: xs]
    @since 4.03 (4.05 in ListLabels) *)
val cons : 'a -> 'a list -> 'a list

val hd : 'a list -> 'a option
val tl : 'a list -> 'a list option

(** Return the [n]-th element of the given list.
    The first element (head of the list) is at position 0.
    Return [None] if the list is too short.
    @raise Invalid_argument if [n] is negative.
    @since 4.05 *)
(* val nth : 'a list -> int -> 'a option *)

(** List reversal. *)
val rev : 'a list -> 'a list

(** [init len f] is [[f 0; f 1; ...; f (len-1)]], evaluated left to right.
    @raise Invalid_argument if [len < 0].
    @since 4.06 *)
val init : int -> (int -> 'a) -> 'a list

(** [append l0 l1] appends [l1] to [l0].
    Same function as the infix operator [@].
    @since 5.1 this function is tail-recursive. *)
val append : 'a list -> 'a list -> 'a list

(** [rev_append l1 l2] reverses [l1] and concatenates it with [l2].
    This is equivalent to [(]{!rev}[ l1) @ l2]. *)
val rev_append : 'a list -> 'a list -> 'a list

(** Concatenate a list of lists. The elements of the argument are all
    concatenated together (in the same order) to give the result.
    Not tail-recursive
    (length of the argument + length of the longest sub-list). *)
val concat : 'a list list -> 'a list

(** {1 Iterators} *)

(** [iter ~f [a1; ...; an]] applies function [f] in turn to
    [[a1; ...; an]]. It is equivalent to
    [f a1; f a2; ...; f an]. *)
val iter : f:('a -> unit) -> 'a list -> unit

(** Same as {!iter}, but the function is applied to the index of
    the element as first argument (counting from 0), and the element
    itself as second argument.
    @since 4.00 *)
val iteri : f:(int -> 'a -> unit) -> 'a list -> unit

(** [map ~f [a1; ...; an]] applies function [f] to [a1, ..., an],
    and builds the list [[f a1; ...; f an]]
    with the results returned by [f]. *)
val map : f:('a -> 'b) -> 'a list -> 'b list

(** Same as {!map}, but the function is applied to the index of
    the element as first argument (counting from 0), and the element
    itself as second argument.
    @since 4.00 *)
val mapi : f:(int -> 'a -> 'b) -> 'a list -> 'b list

(** [rev_map ~f l] gives the same result as
    {!rev}[ (]{!map}[ ~f l)], but is more efficient. *)
val rev_map : f:('a -> 'b) -> 'a list -> 'b list

(** [fold_left ~f init [b1; ...; bn]] is
    [f (... (f (f init b1) b2) ...) bn]. *)
val fold_left : f:('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc

(** [fold_right ~f [a1; ...; an] init] is
    [f a1 (f a2 (... (f an init) ...))]. Not tail-recursive. *)
val fold_right : f:('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc

(** {1 Iterators on two lists} *)

(** [iter2 ~f [a1; ...; an] [b1; ...; bn]] calls in turn
    [f a1 b1; ...; f an bn].
    @raise Invalid_argument if the two lists are determined
                            to have different lengths. *)
val iter2 : f:('a -> 'b -> unit) -> 'a list -> 'b list -> unit

(** [map2 ~f [a1; ...; an] [b1; ...; bn]] is
    [[f a1 b1; ...; f an bn]].
    @raise Invalid_argument if the two lists are determined
                            to have different lengths. *)
val map2 : f:('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

(** [rev_map2 ~f l1 l2] gives the same result as
    {!rev}[ (]{!map2}[ ~f l1 l2)], but is more efficient. *)
val rev_map2 : f:('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

(** [fold_left2 ~f init [a1; ...; an] [b1; ...; bn]] is
    [f (... (f (f init a1 b1) a2 b2) ...) an bn].
    @raise Invalid_argument if the two lists are determined
                            to have different lengths. *)
val fold_left2 : f:('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a list -> 'b list -> 'acc

(** [fold_right2 ~f [a1; ...; an] [b1; ...; bn] init] is
    [f a1 b1 (f a2 b2 (... (f an bn init) ...))].
    @raise Invalid_argument
      if the two lists are determined
      to have different lengths. Not tail-recursive. *)
val fold_right2 : f:('a -> 'b -> 'acc -> 'acc) -> 'a list -> 'b list -> 'acc -> 'acc
