(* Decode a RLE list
 *
 * Given a run-length code list generated as specified in Problem 11,
 * construct its uncompressed version.
 *)

type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let rec decode = function
  | [] -> []
  | One s :: t -> s :: decode t
  | Many (i, s) :: t -> if i = 1 then s :: decode t else s :: decode (Many (i - 1, s) :: t);;

(* tail-recursive version *)
let decode2 l =
  let rec rep acc i s =
    if i = 0 then acc
    else rep (s :: acc) (i - 1) s
  in
  let rec aux acc = function
    | [] -> acc
    | One s :: t -> aux (s :: acc) t
    | Many (i, s) :: t -> aux (rep acc i s) t
  in
  List.rev (aux [] l);;

decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
decode2 [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
(* - : string list =
      ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] *)
