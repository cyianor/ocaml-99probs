(* Run-length encoding of a list (direct solution) *)

type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let encode l =
  (* can refactor a bit
  let rle n x = if n = 1 then One x else Many (n, x) in *)
  let rec aux acc n = function
    | a :: (b :: _ as t) -> if a = b then aux acc (n + 1) t
                            else
                              if n = 1 then
                                aux (One a :: acc) 1 t
                              else
                                aux (Many (n, a) :: acc) 1 t  (* aux (rle n a :: acc) 1 t *)
    | [ a ] -> if n = 1 then One a :: acc else Many (n, a) :: acc (* rle n a :: acc *)
    | [] -> [] (* if original list is empty *)
  in
    List.rev (aux [] 1 l);;
                              

encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e";"f"];;
(* - : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")] *)
