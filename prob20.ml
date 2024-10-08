let remove_at n l =
  let rec aux k acc = function
    | [] -> List.rev acc
    | h :: t -> if k = 0 then (List.rev acc) @ t
                else (aux [@tailcall]) (k - 1) (h :: acc) t
  in
  aux n [] l;;

(* non-tail call implementation *)
let rec remove_at2 n = function
  | [] -> []
  | h :: t -> if n = 0 then t else h :: remove_at (n - 1) t;;

(* remove the k'th element from a list
 * zero-based
 *)

remove_at 1 ["a"; "b"; "c"; "d"];;
(* - : string list = ["a"; "c"; "d"] *)
