open Util
open Graph
open UndirectedGraph



let kruskal g =
  let size = UndirectedGraph.size g in
  let rec kruskal_aux edges gs = match edges with
    [] -> List.hd gs
    | min :: rest ->
      let (min_edge, min_weight) = min in
      let adjs = List.filter (UndirectedGraph.is_adjacent min_edge) gs in
      Printf.printf "add (%d, %d) %d\n" (fst min_edge) (snd min_edge) min_weight;
      match let n = List.length adjs in Printf.printf "#adjs = %d\n" n; n with
	0 -> kruskal_aux rest (UndirectedGraph.singleton size min_edge min_weight :: gs)
      | 1 -> begin let adj = List.single adjs in
	match UndirectedGraph.num_share min_edge adj with
	  1 -> UndirectedGraph.add min_edge min_weight adj;
	    kruskal_aux rest gs
	| 2 -> kruskal_aux rest gs
	| _ -> failwith "fatal error" end
      | 2 -> let g1 = List.nth adjs 0 and g2 = List.nth adjs 1 in
	let g = UndirectedGraph.add min_edge min_weight g2 in
	let g = UndirectedGraph.unite g1 g2 in
	kruskal_aux rest (List.remove ((==) g1) gs)(* stub *)
      | _ -> failwith "fatal error"
  and edges =
    let compare e1 e2 = compare (snd e1) (snd e2) in
    UndirectedGraph.edges g |> List.sort compare in
kruskal_aux edges []

