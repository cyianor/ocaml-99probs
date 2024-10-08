let lotto_select n m =
  Random.self_init ();
  let rec aux acc k total high =
    if k <= total then
      aux ((Random.int_in_range ~min:1 ~max:high) :: acc) (k + 1) total high
    else acc
  in
    aux [] 1 n m;;
  

(* Lotto: Draw N different random numbers from the set 1..M
 *
 * Draw N different random numbers from the set 1..M
 * The selected numbers shall be returned in a list.
 *)

lotto_select 6 49;;
