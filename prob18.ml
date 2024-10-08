(* Extract a slice from a list
 *
 * Given two indices, i and k, the slice is the list containing the elements
 * between the i'th and k'th element of the original list (both limits included).
 * Start counting the elements with 0 (this is the way the List module
 * numbers elements).
 *)

let slice l low high =
  let rec aux acc i = function
    | [] -> acc
    | h :: t -> if i >= low then
                  if i <= high then
                    aux (h :: acc) (i + 1) t
                  else acc
                else aux acc (i + 1) t
  in
    List.rev (aux [] 0 l);;

slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;
(* - : string list = ["c"; "d"; "e"; "f"; "g"] *)
