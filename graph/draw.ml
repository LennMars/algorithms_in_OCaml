open Util

module Make(G : Graph.Graph) = struct
 let client = new Ubigraph.client "http://localhost:20738/RPC2"
  let u = client#ubigraph;;
  let draw g = (* stub : only nodes yet *)
    u#clear ();
    let vertexes = G.nodes g |>
	List.map (fun i ->
	  let v = u#new_vertex () in
	  u#set_vertex_attribute v "label" (string_of_int i);
	  v
	)
    in
    ()
end
