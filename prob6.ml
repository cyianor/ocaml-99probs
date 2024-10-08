 let rev l =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: acc) t
  in
  aux [] l;;

let is_palindrome l =
  let rec aux l1 l2 = match (l1, l2) with
    | ([], []) -> true
    | ([], _) | (_, []) -> false
    | (h1 :: t1, h2 :: t2) -> (h1 = h2) && aux t1 t2
  in
  aux l (rev l);;


(* Palindrome
 * Find out whether a list is a palindrome. A palindrome is its own reverse.
 *)

is_palindrome ["x"; "a"; "m"; "a"; "x"];;
(* expected: - : bool = true *)

not (is_palindrome ["a"; "b"]);;
(* expected: - : bool = true *)
