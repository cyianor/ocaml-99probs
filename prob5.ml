let rec rev = function
  | [] -> []
  | h :: t -> (rev t) @ [h];;


(* Reverse a List *)

rev ["a"; "b"; "c"];;
(* expected: - : string list = ["c"; "b"; "a"] *)

(* @ might be expensive. Alternative formulation *)

let rev l =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: acc) t
  in
  aux [] l;;
