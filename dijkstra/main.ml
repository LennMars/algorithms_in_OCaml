open Util
open Graph
module G = UndirectedGraph

exception Found

let dijkstra s t g =
  let dists = Array.make (G.size g) infinity
  and preds = Array.make (G.size g) None
  and are_fixed = Array.make (G.size g) false in
  let filter_unfixed = List.filter (fun n -> not are_fixed.(n)) in
  dists.(0) <- 0.; are_fixed.(0) <- true;
  let rec dijkstra_aux neighbor =
    if IntSet.is_empty neighbor then raise (Invalid_argument "There is no s-t pass.")
    else
      let dist_traceable u h = G.get (h, u) g +. dists.(h), h in
      let node_and_dist = List.map
	(fun u -> (u, List.find_min_val (dist_traceable u) (G.heads_of u g)))
	(IntSet.elements neighbor)
      in
      let (to_fix, (d, h)) = List.find_min snd node_and_dist in
      dists.(to_fix) <- d; preds.(to_fix) <- Some h; are_fixed.(to_fix) <- true;
      if to_fix = t then raise Found;
      let update u v =
	let new_dist = dists.(u) +. G.get (u, v) g in
	if dists.(v) > new_dist then begin dists.(v) <- new_dist; preds.(v) <- Some u; end
      in
      List.iter (update to_fix) (G.tails_of to_fix g |> filter_unfixed);
      let new_neighbor = IntSet.add_list (IntSet.remove to_fix neighbor) (G.tails_of to_fix g |> filter_unfixed)
      in
      dijkstra_aux new_neighbor
  in
  try dijkstra_aux (IntSet.add_list IntSet.empty (G.tails_of s g)) with
    Found ->
      let rec trace accum n = match preds.(n) with
	  None -> accum
	| Some m -> trace (m :: accum) m
      in
      dists.(t), trace [t] t

