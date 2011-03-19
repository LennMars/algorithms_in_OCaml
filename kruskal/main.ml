open Util
open Graph

module Int = struct type t = int end
module G = MakeUndirectedGraph (Int)

let kruskal g =
  let size = G.size g in
  let rec kruskal_aux edges gs = match edges with
    [] -> List.hd gs
    | min :: rest ->
      let (min_edge, min_weight) = min in
      let adjs = List.filter (G.is_adjacent min_edge) gs in
      Printf.printf "add (%d, %d) %d\n" (fst min_edge) (snd min_edge) min_weight;
      match let n = List.length adjs in Printf.printf "#adjs = %d\n" n; n with
	0 -> kruskal_aux rest (G.singleton size min_edge min_weight :: gs)
      | 1 -> begin let adj = List.single adjs in
	match G.num_share min_edge adj with
	  1 -> G.add min_edge min_weight adj;
	    kruskal_aux rest gs
	| 2 -> kruskal_aux rest gs
	| _ -> failwith "fatal error" end
      | 2 -> let g1 = List.nth adjs 0 and g2 = List.nth adjs 1 in
	let g = G.add min_edge min_weight g2 in
	let g = G.unite g1 g2 in
	kruskal_aux rest (List.remove ((==) g1) gs)(* stub *)
      | _ -> failwith "fatal error"
  and edges =
    let compare e1 e2 = compare (snd e1) (snd e2) in
    G.edges g |> List.sort compare in
kruskal_aux edges []

