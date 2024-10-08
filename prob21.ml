let insert_at s n l =
  let rec aux s n acc = function
    | [] -> if n > 0 then List.rev (s :: acc) else List.rev acc
    | h :: t -> if n = 0 then (aux [@tailcall]) s (n - 1) (h :: s :: acc) t
                else (aux [@tailcall]) s (n - 1) (h :: acc) t
  in
  aux s n [] l;;

(* simpler non-tail call formulation *)
let rec insert_at2 x n = function
  | [] -> [x]
  | h :: t as l -> if n = 0 then x :: l else h :: insert_at2 x (n - 1) t;;

(* Insert an element at a given position into a list
 *
 * Start counting list elements with 0. If the position is larger or equal
 * to the length of the list, insert the element at the end.
 * (The behaviour is unspecified if the position is negative.)
 *)

insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;
(* - : string list = ["a"; "alfa"; "b"; "c"; "d"] *)
insert_at "alfa" 5 ["a"; "b"];;
(* - : string list = ["a"; "b"; "alfa"] *)

insert_at "alfa" 2 [];;
