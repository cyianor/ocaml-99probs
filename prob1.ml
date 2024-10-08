let rec last l =  (* alternative: let rec last = function *)
  match l with (* not needed if function is used *)
  | [] -> None
  | h :: [] -> Some h (* alternative: [ x ] -> Some x *)
  | h :: t -> last t;; (* alternative: _ :: t -> last t *)


(* Problem 1: Tail of a List
 *
 * Write a function `last : 'a list -> 'a option` that returns the last
 * element of a list.
 *)

last ["a" ; "b" ; "c" ; "d"];;
(* expected: `- : string option = Some "d"` *)
last [];;
(* expected: `- : 'a option = None` *)
