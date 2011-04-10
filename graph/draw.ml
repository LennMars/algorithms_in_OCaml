open Util

module Make(G : Graph.Graph) = struct
 let client = new Ubigraph.client "http://localhost:20738/RPC2"
 let u = client#ubigraph

 let draw to_string g = (* stub : only nodes yet *)
   u#clear ();
   let make_assoc i =
     let v = u#new_vertex () in
     u#set_vertex_attribute v "label" (string_of_int i);
     u#set_vertex_attribute v "fontcolor" "#ff3333";
     u#set_vertex_attribute v "fontsize" "18";
     (i, v)
   in
   let vertexes = List.map make_assoc (G.nodes g) in
   let join ((i, j), w) =
     let e = u#new_edge (List.assoc i vertexes) (List.assoc j vertexes) in
     u#set_edge_attribute e "label" (to_string w);
     u#set_edge_attribute e "width" "3.0";
     u#set_edge_attribute e "color" "#ffffff";
     u#set_edge_attribute e "fontcolor" "#33cc66";
     u#set_edge_attribute e "fontsize" "18";
     ()
   in
   List.iter join (G.edges g)
end
