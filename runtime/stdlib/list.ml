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

(* An alias for the type of lists. *)
type 'a t = 'a list =
  | []
  | ( :: ) of 'a * 'a list

(* List operations *)

let rec length_aux len = function
  | [] -> len
  | _ :: l -> length_aux (len + 1) l
;;

let length l = length_aux 0 l
let cons a l = a :: l

let hd = function
  | [] -> None
  | a :: _ -> Some a
;;

let tl = function
  | [] -> None
  | _ :: l -> Some l
;;

let nth l n =
  if n < 0
  then invalid_arg "List.nth"
  else (
    let rec nth_aux l n =
      match l with
      | [] -> None
      | a :: l -> if n = 0 then Some a else nth_aux l (n - 1)
    in
    nth_aux l n)
;;

let nth_exn l n =
  match nth l n with
  | None -> invalid_arg "List.nth"
  | Some a -> a
;;

let append = ( @ )

let rec rev_append l1 l2 =
  match l1 with
  | [] -> l2
  | a :: l -> rev_append l (a :: l2)
;;

let rev l = rev_append l []

let[@tail_mod_cons] rec init i last f =
  if i > last
  then []
  else if i = last
  then [ f i ]
  else (
    let r1 = f i in
    let r2 = f (i + 1) in
    r1 :: r2 :: init (i + 2) last f)
;;

let init len f = if len < 0 then invalid_arg "List.init" else init 0 (len - 1) f

let is_empty = function
  | [] -> true
  | _ :: _ -> false
;;

let rec concat = function
  | [] -> []
  | l :: r -> l @ concat r
;;

let[@tail_mod_cons] rec map ~f = function
  | [] -> []
  | [ a1 ] ->
    let r1 = f a1 in
    [ r1 ]
  | a1 :: a2 :: l ->
    let r1 = f a1 in
    let r2 = f a2 in
    r1 :: r2 :: map ~f l
;;

let[@tail_mod_cons] rec mapi_aux i ~f = function
  | [] -> []
  | [ a1 ] ->
    let r1 = f i a1 in
    [ r1 ]
  | a1 :: a2 :: l ->
    let r1 = f i a1 in
    let r2 = f (i + 1) a2 in
    r1 :: r2 :: mapi_aux (i + 2) ~f l
;;

let mapi ~f l = mapi_aux 0 ~f l

let rev_map ~f l =
  let rec rmap_f accu = function
    | [] -> accu
    | a :: l -> rmap_f (f a :: accu) l
  in
  rmap_f [] l
;;

let rec iter ~f = function
  | [] -> ()
  | a :: l ->
    f a;
    iter ~f l
;;

let rec iteri_aux i ~f = function
  | [] -> ()
  | a :: l ->
    f i a;
    iteri_aux (i + 1) ~f l
;;

let iteri ~f l = iteri_aux 0 ~f l

let rec fold_left ~f accu l =
  match l with
  | [] -> accu
  | a :: l -> fold_left ~f (f accu a) l
;;

let rec fold_right ~f l accu =
  match l with
  | [] -> accu
  | a :: l -> f a (fold_right ~f l accu)
;;

let[@tail_mod_cons] rec map2 ~f l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | [ a1 ], [ b1 ] ->
    let r1 = f a1 b1 in
    [ r1 ]
  | a1 :: a2 :: l1, b1 :: b2 :: l2 ->
    let r1 = f a1 b1 in
    let r2 = f a2 b2 in
    r1 :: r2 :: map2 ~f l1 l2
  | _, _ -> invalid_arg "List.map2"
;;

let rev_map2 ~f l1 l2 =
  let rec rmap2_f accu l1 l2 =
    match l1, l2 with
    | [], [] -> accu
    | a1 :: l1, a2 :: l2 -> rmap2_f (f a1 a2 :: accu) l1 l2
    | _, _ -> invalid_arg "List.rev_map2"
  in
  rmap2_f [] l1 l2
;;

let rec iter2 ~f l1 l2 =
  match l1, l2 with
  | [], [] -> ()
  | a1 :: l1, a2 :: l2 ->
    f a1 a2;
    iter2 ~f l1 l2
  | _, _ -> invalid_arg "List.iter2"
;;

let rec fold_left2 ~f accu l1 l2 =
  match l1, l2 with
  | [], [] -> accu
  | a1 :: l1, a2 :: l2 -> fold_left2 ~f (f accu a1 a2) l1 l2
  | _, _ -> invalid_arg "List.fold_left2"
;;

let rec fold_right2 ~f l1 l2 accu =
  match l1, l2 with
  | [], [] -> accu
  | a1 :: l1, a2 :: l2 -> f a1 a2 (fold_right2 ~f l1 l2 accu)
  | _, _ -> invalid_arg "List.fold_right2"
;;

let filter_map l ~f =
  let rec aux acc = function
    | [] -> rev acc
    | x :: xs ->
      (match f x with
       | None -> aux acc xs
       | Some y -> aux (y :: acc) xs)
  in
  aux [] l
;;

let partition_map l ~f =
  let rec aux firsts seconds = function
    | [] -> rev firsts, rev seconds
    | x :: xs ->
      (match f x with
       | `First y -> aux (y :: firsts) seconds xs
       | `Second y -> aux firsts (y :: seconds) xs)
  in
  aux [] [] l
;;

let concat_map l ~f =
  let rec aux acc = function
    | [] -> rev acc
    | x :: xs -> aux (rev_append (f x) acc) xs
  in
  aux [] l
;;

let reduce l ~f =
  match l with
  | [] -> None
  | x :: xs -> Some (fold_left ~f x xs)
;;

let sort l ~compare =
  let rec insert x = function
    | [] -> [ x ]
    | y :: ys as l -> if compare x y <= 0 then x :: l else y :: insert x ys
  in
  fold_left ~f:(fun acc x -> insert x acc) [] l
;;

let sum l ~f = fold_left ~f:(fun acc x -> acc + f x) 0 l

let sort_and_group l ~compare =
  let sorted = sort l ~compare:(fun (k1, _) (k2, _) -> compare k1 k2) in
  let rec group acc current_key current_vals = function
    | [] ->
      (match current_vals with
       | [] -> rev acc
       | _ -> rev ((current_key, rev current_vals) :: acc))
    | (k, v) :: rest ->
      if compare k current_key = 0
      then group acc current_key (v :: current_vals) rest
      else (
        let acc' =
          match current_vals with
          | [] -> acc
          | _ -> (current_key, rev current_vals) :: acc
        in
        group acc' k [ v ] rest)
  in
  match sorted with
  | [] -> []
  | (k, v) :: rest -> group [] k [ v ] rest
;;
