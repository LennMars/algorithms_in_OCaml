open Util
open Graph
open Main

let g = G.empty 6;;
G.add (0, 1) 7. g;;
G.add (0, 5) 14. g;;
G.add (0, 2) 9. g;;
G.add (1, 2) 10. g;;
G.add (1, 3) 15. g;;
G.add (2, 3) 10. g;;
G.add (2, 5) 2. g;;
G.add (3, 4) 6. g;;
G.add (4, 5) 9. g;;
let d = dijkstra 0 4 g
