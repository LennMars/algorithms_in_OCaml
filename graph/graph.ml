open Util

type edge = int * int
type path = int list

module type Graph = sig

  (** The type of graphs. *)
  type 'a t

  (** empty n returns a graph with at most n nodes and no edge. *)
  val empty : int -> 'a t

  (** Test whether a graph has no edge or not. *)
  val is_empty : 'a t -> bool

  val size : 'a t -> int
  val exists : edge -> 'a t -> bool
  val add : edge -> 'a -> 'a t -> unit
  val singleton : int -> edge -> 'a -> 'a t
  val copy : 'a t -> 'a t

  (** unite g1 g2 adds all edges of g1 to g2. *)
  val unite : 'a t -> 'a t -> unit
  val set : edge -> 'a -> 'a t -> unit
  val remove : edge -> 'a t -> unit
  val heads_of : int -> 'a t -> int list
  val tails_of : int -> 'a t -> int list
  val get : edge -> 'a t -> 'a
  val nodes : 'a t -> int list
  val edges : 'a t -> (edge * 'a) list
  val is_edge : edge -> 'a t -> bool

  (** num_share e g returns the number of endpoints that e shares with g. *)
  val num_share : edge -> 'a t -> int

  (** is_adjacent e g tests whether e shares an endpoint with g. *)
  val is_adjacent : edge -> 'a t -> bool

  (** find_path s 'a t p g finds a s-t path whose all edges satisfies p by BFS.
      Raises Not_found when there is no such a path. *)
  val find_path : int -> int -> ('a -> bool) -> 'a t -> path
  val fold_through : ('a -> edge -> 'b -> 'a) -> 'a -> path -> 'b t -> 'a
  val map_through : ('a -> 'a) -> path -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mape : (edge -> 'a -> 'b) -> 'a t -> 'b t
  val iter : ('a -> unit) -> 'a t -> unit
  val itere : (edge -> 'a -> unit) -> 'a t -> unit
end

module BaseGraph : sig
  type 'a interedge = int * 'a
  type 'a t = 'a interedge list array
  val head : edge -> int
  val tail : edge -> int
  val dest : 'a interedge -> int
  val weight : 'a interedge -> 'a
  val is_endpoint : int -> 'a interedge -> bool
  val empty : int -> 'a t
  val is_empty : 'a t -> bool
  val size : 'a t -> int
  val exists : edge -> 'a t -> bool
  val add' : edge -> 'a -> 'a t -> unit
  val remove' : edge -> 'a t -> unit
  val copy : 'a t -> 'a t
  val heads_of : int -> 'a t -> int list
  val tails_of : int -> 'a t -> int list
  val head_nodes : 'a t -> int list
  val tail_nodes : 'a t -> int list
  val get : edge -> 'a t -> 'a
  val adjacents : int -> 'a t -> 'a interedge list
  val is_edge : edge -> 'a t -> bool
  val find_path : int -> int -> ('a -> bool) -> 'a t -> path
  val path_exists : path -> 'a t -> bool
  val fold_through : ('b -> edge -> 'a -> 'b) -> 'b -> path -> 'a t -> 'b
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mape : (edge -> 'a -> 'b) -> 'a t -> 'b t
  val iter : ('a -> unit) -> 'a t -> unit
  val itere : (edge -> 'a -> unit) -> 'a t -> unit
end
 = struct
  type 'a interedge = int * 'a
  type 'a t = 'a interedge list array
  let head (e : edge) = fst e
  let tail (e : edge) = snd e
  let dest (x : 'a interedge) = fst x
  let weight (x : 'a interedge) = snd x
  let is_endpoint p x = dest x = p
  let empty n = Array.make n []
  let is_empty g = Array.for_all List.is_empty g
  let size = Array.length
  let exists (h, t) g =  List.exists (is_endpoint t) g.(h)
  let add' (h, t) w g = g.(h) <- (t, w) :: g.(h)
  let remove' (h, t) g = g.(h) <- List.remove (is_endpoint t) g.(h)
  let copy = Array.copy
  let heads_of t g = Array.to_list g
	      |> List.mapi (fun i l -> if List.exists (fun x -> dest x = t) l then Some i else None)
	      |> List.filter_some
  let tails_of h g = List.map dest g.(h)
  let head_nodes g = Array.to_list g
	   |> List.mapi (fun i dests -> match dests with [] -> None | _ -> Some i)
	   |> List.filter_some
  let tail_nodes g = Array.to_list g
	   |> List.flatten |> List.split |> fst |> IntSet.add_list IntSet.empty |> IntSet.elements
  let get (h, t) g = List.find (is_endpoint t) g.(h) |> weight
  let adjacents h g = g.(h)
  let is_edge e g = List.exists (fun x -> dest x = tail e) g.(head e)
  exception Found
  let find_path s t p g =
    let path_table = Array.make (size g) None in
    let rec find_path_aux to_search =
      if Stack2.is_empty to_search then raise Not_found
      else
	let (u, rest) = Stack2.pop to_search in
	let test q (v, w) =
	  if p w && path_table.(v) = None then begin
	    path_table.(v) <- Some u;
	    if v = t then raise Found else Stack2.push v q
	  end
	  else q
	in
	find_path_aux (List.fold_left test rest (adjacents u g))
    in
    try find_path_aux (Stack2.singleton s) with
      Found ->
	let rec rebuild_path v accum = match path_table.(v) with
	    Some u -> if u = s then s :: accum else rebuild_path u (u :: accum)
	  | None -> failwith "find_path : fatal error"
	in
	rebuild_path t [t]
  let path_exists path g =
    let rec path_exists_aux = function
	[] -> true
      | hd :: tl -> if exists hd g then path_exists_aux tl else false
    in
    path_exists_aux (List.to_chain path)
  let fold_through f x path g =
    if not (path_exists path g) then raise (Invalid_argument "That path does not exist in the graph.");
    let rec fold_through_aux x = function
	[] -> x
      | hd :: tl -> fold_through_aux (f x hd (get hd g)) tl
    in
    fold_through_aux x (List.to_chain path)
    let map f g =
      let rec map_aux accum = function
	  [] -> accum
	| (t, w) :: tl -> map_aux ((t, f w) :: accum) tl
      in
      Array.map (map_aux []) g
    let mape f g =
      let rec map_aux accum h = function
	  [] -> accum
	| (t, w) :: tl -> map_aux ((t, f (h, t) w) :: accum) h tl
      in
      Array.mapi (map_aux []) g
    let iter f g =
      let rec iter_aux = function
	  [] -> ()
	| (t, w) :: tl -> f w; iter_aux tl
      in
      Array.iter iter_aux g
    let itere f g =
      let rec iter_aux h = function
	  [] -> ()
	| (t, w) :: tl -> f (h, t) w; iter_aux h tl
      in
      Array.iteri iter_aux g
end


(* loop and multigraph are prohibited *)
module DirectedGraph : Graph
  = struct
    include BaseGraph
    let add e w g =
      if exists e g then raise (Invalid_argument "add : edge already exists.");
      add' e w g
    let singleton n e w = let g = empty n in add e w g; g
    let edges g = Array.to_list g
      |> List.mapi (fun i xs -> List.map (fun x -> ((i, dest x), weight x)) xs)
      |> List.flatten
    let unite g1 g2 = edges g1
      |> List.iter (fun(t, w) ->
	   try add t w g2
	   with Invalid_argument _ -> raise (Invalid_argument "unite : edge overlapping."))
    let set e w g =
      if exists e g = false then raise (Invalid_argument "set : edge doesn't exists.");
      remove' e g;
      add' e w g
    let remove e g =
      remove' e g
    let nodes g = IntSet.add_list (IntSet.add_list IntSet.empty (head_nodes g)) (tail_nodes g) |> IntSet.elements
    let num_share (h, t) g = List.count (fun n -> n = h || n = t) (nodes g)
    let is_adjacent e g =
      num_share e g > 0 &&
	not (is_edge e g) &&
	not (is_edge (swap e) g)
    let map_through f path g =
      if not (path_exists path g) then raise (Invalid_argument "That path does not exist in the graph.");
      let rec map_through_aux = function
	  [] -> ()
	| hd :: tl -> set hd (get hd g |> f) g; map_through_aux tl
      in
      map_through_aux (List.to_chain path)
  end

module UndirectedGraph : Graph = struct
  include BaseGraph
  let exists e g = exists e g || exists (swap e) g
  let add e w g =
    if exists e g then raise (Invalid_argument "add : edge already exists");
    add' e w g;
    add' (swap e) w g
  let singleton n e w = let g = empty n in add e w g; g
  let edges g = Array.to_list g
    |> List.mapi (fun i xs -> List.map
	 (fun x -> if i < dest x then Some ((i, dest x), weight x) else None)
	 xs)
    |> List.flatten
    |> List.filter_some
  let unite g1 g2 = edges g1
    |> List.iter (fun (t, w) ->
	 try add t w g2
	 with Invalid_argument _ -> raise (Invalid_argument "unite : edge overlapping."))
  let set e w g =
    if exists e g = false then raise (Invalid_argument "set : edge doesn't exists");
    remove' e g;
    remove' (swap e) g
  let remove e g =
    remove' e g;
    remove' (swap e) g
  let nodes = head_nodes
  let num_share (h, t) g = List.count (fun n -> n = h || n = t) (nodes g)
  let is_adjacent e g =
    num_share e g > 0 && not (is_edge e g)
  let map_through f path g =
    if not (path_exists path g) then raise (Invalid_argument "That path does not exist in the graph.");
    let rec map_through_aux = function
	[] -> ()
      | hd :: tl -> set hd (get hd g |> f) g; map_through_aux tl
    in
    map_through_aux (List.to_chain path)
end



