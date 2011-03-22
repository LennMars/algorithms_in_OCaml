open Util
open Graph.UndirectedGraph
open Main

let g = empty 7;;
add (0, 1) 7 g;;
add (1, 2) 8 g;;
add (2, 4) 5 g;;
add (4, 1) 7 g;;
add (1, 3) 9 g;;
add (3, 4) 15 g;;
add (3, 5) 6 g;;
add (5, 4) 8 g;;
add (4, 6) 9 g;;
add (5, 6) 11 g;;


let mst = kruskal g |> edges
