open Util
open Graph

let g1 = DirectedGraph.empty 5;;
DirectedGraph.add (3, 2) () g1;;
DirectedGraph.add (2, 4) () g1;;
DirectedGraph.add (3, 4) () g1;;

let g2 = DirectedGraph.empty 5;;
DirectedGraph.add (0, 1) () g2;;
DirectedGraph.add (0, 2) () g2;;
DirectedGraph.add (1, 2) () g2;;

DirectedGraph.unite g1 g2;;


module D = Draw.Make(DirectedGraph);;
D.draw g2
