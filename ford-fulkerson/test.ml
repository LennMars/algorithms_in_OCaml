open Util
module G = Graph.DirectedGraph

let g = G.empty 7;;
   G.add (0, 3) 3 g;;
G.add (0, 1) 3 g;;
G.add (2, 0) 3 g;;
G.add (1, 2) 4 g;;
G.add (2, 3) 1 g;;
G.add (2, 4) 2 g;;
G.add (4, 1) 1 g;;
G.add (3, 4) 2 g;;
G.add (3, 5) 6 g;;
G.add (4, 6) 1 g;;
G.add (5, 6) 9 g;;
Main.max_flow 0 6 g |> Printf.printf "total max flow : %d\n"

