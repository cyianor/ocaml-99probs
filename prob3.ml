(* n'th element of a list
 * Find the n-th element of a list
 *)

List.nth ["a"; "b"; "c"; "d"; "e"] 2;;
List.nth ["a"] 2;;

(* implement on my own *)

let rec lengthImpl n = function
  | [] -> n
  | _ :: t -> lengthImpl (n + 1) t;;

let length l = lengthImpl 0 l;;

exception Failure of string;;

let nth l n =
  let rec aux m = function
    | [] -> None
    | x :: t -> if n = m then Some x else aux (m + 1) t
  in
  match aux 0 l with
  | None -> raise (Failure "nth")
  | Some x -> x;;
    
nth ["a"; "b"; "c"; "d"; "e"] 2;;
nth ["a"] 2;;

(* Smarter alternative *)

let rec nth n = function
  | [] -> None
  | x :: t -> if n = 0 then Some x else nth (n - 1) t;;
