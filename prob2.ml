let rec last_two = function
  | [] -> None
  | [ _ ] -> None (* first and second case can be combined: | [] | [_] -> None *)
  | [ x; y ] -> Some (x, y)
  | _ :: t -> last_two t;;

(* Last two elements of a List
 * Find the last two (last and penultimate) elements of a list.
 *)

last_two ["a"; "b"; "c"; "d"];;
(* expected: - : (string * string) option = Some ("c", "d") *)
last_two ["a"];;
(* expected: - : (string * string) option = None *)
