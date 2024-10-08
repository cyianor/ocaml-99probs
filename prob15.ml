(* Replicate the elements of a list a given number of times *)

let replicate l n =
  let rec rep acc count x = if count = 0 then acc else rep (x :: acc) (count - 1) x in
  let rec aux acc count = function
    | [] -> acc
    | h :: t -> aux (rep acc count h) count t
  in
    List.rev (aux [] n l);;

replicate ["a"; "b"; "c"] 3;;
(* - : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"] *)
