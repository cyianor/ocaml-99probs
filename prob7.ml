(* Flatten a list
 *
 * Flatten a nested list structure
 *)

type 'a node =
  | One of 'a
  | Many of 'a node list;;

let flatten l =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> match h with
                | One s -> aux (s :: acc) t
                | Many l -> aux (aux acc l) t
  in
  List.rev (aux [] l);;

(* The auxiliary function can be simplified *)
let flatten2 l =
  let rec aux acc = function
    | [] -> acc
    | One s :: t -> aux (s :: acc) t
    | Many l :: t -> aux (aux acc l) t
  in
    List.rev (aux [] l);;


flatten [One "a"; Many [One "b"; Many [One "c"; One "d"]; One "e"]];;
(* - : string list = ["a"; "b"; "c"; "d"; "e"] *)
