let pack l =
  let rec aux acc inner = function
    | [] -> acc
    | [ a ] -> ((a :: inner) :: acc)
    | a :: (b :: _ as t) -> if a = b then aux acc (a :: inner) t
                            else aux ((a :: inner) :: acc) [] t
  in
  List.rev (aux [] [] l);;

(* non-tail-recursive version *)
let pack2 l =
  let rec aux inner = function
    | a :: (b :: _ as t) -> if a = b then aux (a :: inner) t
                            else (a :: inner) :: (aux [] t)
    | [ a ] -> [ a :: inner ]
    | [] -> []
  in
    aux [] l;;

(* Pack consecutive duplicates
 *
 * Pack consecutive duplicates of list elements into sublists.
 *)

pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"; "f"];;
pack2 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"; "f"];;
(* - : string list list =
[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]] *)
