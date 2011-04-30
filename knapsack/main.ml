open Util

module Goods = struct
  (** (weight, value) *)
  type t = int * int

  (** compares primarily which is heavier and secondary which is cheaper.
      Therefore lightest and most expensive one comes first when sorted by this function. *)
  let compare (w1, v1) (w2, v2) =
    Pervasives.compare w1 w2 * 2 - Pervasives.compare v1 v2
end

module GoodsSet = Set.Make(Goods)

let print_goods gs =
  GoodsSet.iter (fun g -> Printf.printf "(%d, %d) " (fst g) (snd g)) gs;
  Printf.printf "\n"

let opt capacity goods =
  let goodsSet = List.fold_left (fun set elt ->
    if (fst elt < 0 || snd elt < 0) then raise (Invalid_argument "Value and weight must not be negative.")
    else GoodsSet.add elt set) GoodsSet.empty goods
  in
  (* main part of DP. opt_log is a list of (total value, remained goods) and the nth element represents the optimum and the optimal solution in n+1 less capacity than current. *)
  let rec opt_aux capacity opt_log =
    (* debug *)
    (*
    List.iter (fun opt ->
      Printf.printf "%d" (fst opt);
      print_goods (GoodsSet.diff goodsSet (snd opt))
    ) opt_log;
    Printf.printf "\n";  *)
    if capacity <= 0 || GoodsSet.is_empty (snd (List.hd opt_log)) then (* termination *)
      opt_log
    else
      (* At first, choice n+1 lighter goods from remained goods at opt in n+1 less capacity.
      Then return None when not found and otherwise return the most expensive one. *)
      let to_add n remained =
	let addable = (GoodsSet.filter (fun g -> fst g = n + 1) remained) in
	if GoodsSet.is_empty addable then None
	else Some (GoodsSet.min_elt addable)
      in
      let add_good n (value, remained) = match to_add n remained with
	  None -> (value, remained)
	| Some g -> (value + snd g, GoodsSet.remove g remained)
      in
      (* Make candidates for opt in current capacity by mapping add_good.
	 and choice from them by total value *)
      let new_opt = List.find_max fst (List.mapi add_good opt_log) in
      opt_aux (capacity - 1) (new_opt :: opt_log)
  in
  opt_aux capacity [(0, goodsSet)] |> List.hd |> snd |> GoodsSet.diff goodsSet |> GoodsSet.elements
