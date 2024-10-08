(* Drop every N'th element from a list *)

let drop l n =
  let rec aux acc k = function
    | [] -> acc
    | h :: t -> if k = n then aux acc 1 t
                else aux (h :: acc) (k + 1) t
  in
    List.rev (aux [] 1 l);;

drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
(* - : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"] *)
