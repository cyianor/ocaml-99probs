let range m n =
  let rec aux low high acc =
    if low <= high then
      aux (low + 1) high (low :: acc)
    else acc
  in
    if m <= n then List.rev (aux m n []) else aux n m [];;

(* Create a list containing all integers within a given range
 *
 * If first argument is greater than second, produce a list in decreasing order.
 *)

range 4 9;;
(* - : int list = [4; 5; 6; 7; 8; 9] *)
range 6 4;;
(* - : int list = [6; 5; 4] *)
