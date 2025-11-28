type 'a t = 'a option =
  | None
  | Some of 'a

let some x = Some x
let none = None

let value o ~default =
  match o with
  | None -> default
  | Some x -> x
;;

let some_if cond x = if cond then Some x else None

let bind o ~f =
  match o with
  | None -> None
  | Some x -> f x
;;

let map o ~f =
  match o with
  | None -> None
  | Some x -> Some (f x)
;;

let iter o ~f =
  match o with
  | None -> ()
  | Some x -> f x
;;

let is_some = function
  | Some _ -> true
  | None -> false
;;

let is_none = function
  | None -> true
  | Some _ -> false
;;
