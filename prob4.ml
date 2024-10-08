let rec lengthImpl n = function
  | [] -> n
  | _ :: t -> lengthImpl (n + 1) t;;

let length l = lengthImpl 0 l;;

(* Different implementation using local function *)
let length l =
  let rec aux n = function
    | [] -> n
    | _ :: t -> aux (n + 1) t
  in
  aux 0 list;;

(* length of a list
 * Find the number of elements of a list.
 *)

length ["a"; "b"; "c"];;
(* expected: - : int = 3 *)

length [];;
(* expected: - : int = 0 *)
