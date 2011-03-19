open Util;;
open Main;;


let g = G.empty 7
let g = G.add (0, 1) 7 g
let g = G.add (1, 2) 8 g
let g = G.add (2, 4) 5 g
let g = G.add (4, 1) 7 g
let g = G.add (1, 3) 9 g
let g = G.add (3, 4) 15 g
let g = G.add (3, 5) 6 g
let g = G.add (5, 4) 8 g
let g = G.add (4, 6) 9 g
let g = G.add (5, 6) 11 g;;


let mst = kruskal g |> G.edges
