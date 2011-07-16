open Util
open Graph

let g1 = DirectedGraph.empty 5;;
DirectedGraph.add (3, 2) "a" g1;;
DirectedGraph.add (2, 4) "a" g1;;
DirectedGraph.add (3, 4) "a" g1;;

let g2 = DirectedGraph.empty 5;;
DirectedGraph.add (0, 1) "b" g2;;
DirectedGraph.add (0, 2) "b" g2;;
DirectedGraph.add (1, 2) "b" g2;;

DirectedGraph.unite g1 g2;;

(* module D = Draw.Make(DirectedGraph);;
D.draw identity g2 *)
