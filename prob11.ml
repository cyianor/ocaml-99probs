type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let encode l =
  let enc x = function
    | 1 -> One x
    | n -> Many (n, x)
  in
  let rec aux n x = function
    | [] -> [enc x n]
    | h :: t -> if h = x then aux (n + 1) h t else (enc x n) :: aux 1 h t
  in
  match l with
  | [] -> []
  | h :: t -> aux 1 h t;;
      

(* Modified Run-Length Encoding
 *
 * Modify the result of Problem 7 in such a way that if an element
 * has no duplicates it is simply copied into the result list.
 * Only elements with duplicates are transferred as (N, E) lists.
 *
 * OCaml lists are homogeneous and one needs to define a type to hold
 * both single elements and sub-lists.
 *)

encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
(* expected: - : string rle list =
   [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] *)
