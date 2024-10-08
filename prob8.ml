let compress l =
  let rec aux acc last = function
    | [] -> last :: acc
    | h :: t -> if h = last then aux acc last t
                else aux (last :: acc) h t
  in
  match l with
  | [] -> []
  | h :: t -> List.rev (aux [] h t);;

(* alternative formulation *)
let rec compress2 = function
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | smaller -> smaller;;

(* tail recursive version *)
let compress3 l =
  let rec aux acc = function
    | [] -> acc
    | [ a ] -> a :: acc
    | a :: (b :: _ as t) -> if a = b then aux acc t else aux (a :: acc) t
  in
    List.rev (aux [] l);;

(* Eliminate duplicates
 *
 * Eliminate consecutive duplicates of list elements.
 *)

compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
compress2 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
compress3 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
(* - : string list = ["a"; "b"; "c"; "a"; "d"; "e"] *)
