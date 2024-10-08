let rec duplicate = function
  | [] -> []
  | h :: t -> h :: h :: duplicate t;;

(* Duplicate the elements of a list *)

duplicate ["a"; "b"; "c"; "c"; "d"];;
(* expected: - : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"] *)

(* The function above is not tail recursive. Is it possible to modify it so it becomes such? *)

let duplicate2 l =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: h :: acc) t
  in
  List.rev (aux [] l);;

duplicate2 ["a"; "b"; "c"; "c"; "d"];;
