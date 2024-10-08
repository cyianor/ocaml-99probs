let encode l = 
  let rec aux n x = function
    | [] -> [(n, x)]
    | h :: t -> if h = x then aux (n + 1) h t else (n, x) :: aux 1 h t
  in
  match l with
  | [] -> []
  | h :: t -> aux 1 h t;;

(* Run-Length Encoding
 *
 * Implement RLE
 *)

encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
(* expected: - : (int * string) list =
             [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"), (4, "e")] *)
