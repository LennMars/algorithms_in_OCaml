open Util
module G = Graph.DirectedGraph

let make_flow_network g =
  let g2 = G.map (fun c -> (0, c)) g in
  let reverse (h, t) c = if not (G.exists (t, h) g) then G.add (t, h) (0, 0) g2 in
  G.itere reverse g2; g2

let max_flow s t g =
  let g = make_flow_network g in
  let rec max_flow_aux flow =
    try
      let aug_path = G.find_path s t (fun (f, c) -> c - f > 0) g in
      let aug_path_chain = to_chain aug_path in
      let bottleneck =
	let (f1, c1) = (G.get (List.hd aug_path_chain) g) in
	G.fold_through (fun r e (f, c) -> min r (c - f)) (c1 - f1) aug_path g
      in
      Printf.printf "augmenting path found : "; List.print_int_list aug_path;
      Printf.printf "(max flow : %d)\n" bottleneck;
      G.map_through (fun (f, c) -> (f + bottleneck, c)) aug_path g;
      G.map_through (fun (f, c) -> (f - bottleneck, c)) (List.rev aug_path) g;
      max_flow_aux (flow + bottleneck)
    with Not_found -> flow
  in
  max_flow_aux 0
