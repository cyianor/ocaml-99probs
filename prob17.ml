let split l n =
  let rec aux acc1 acc2 n k = function
    | [] -> (List.rev acc1, acc2)
    | h :: t -> if k < n then aux (h :: acc1) t n (k + 1) t else aux acc1 acc2 n k []
  in
  aux [] [] n 0 l;;

(* can be formulated with a single accumulator *)
let split2 l n =
  let rec aux k acc = function
    | [] -> List.rev acc, []
    | h :: t as list -> if k = 0 then List.rev acc, list
                        else aux (k - 1) (h :: acc) t
  in
    aux n [] l;;
  

(* Split a list into two parts
 *
 * The length of the first part is given. If the length of the first part
 * is longer than the entire list, then the first part is the list and the
 * second part is empty. *)

split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
(* - : string list * string list =
   (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]) *)
split ["a"; "b"; "c"; "d"] 5;;
(* - : string list * string list = (["a"; "b"; "c"; "d"], []) *)
