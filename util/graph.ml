open Util

type edge = int * int
type path = int list

module type Weight = sig
  type t
  val zero : t
  val plus : t -> t -> t
  val minus : t -> t -> t
end

module type Graph = sig
  (** The type of weights assigned to each edge. *)
  type weight

  (** The type of graphs. *)
  type t

  (** empty n returns a graph with at most n nodes and no edge. *)
  val empty : int -> t

  (** Test whether a graph has no edge or not. *)
  val is_empty : t -> bool

  val size : t -> int
  val exists : edge -> t -> bool
  val add : edge -> weight -> t -> t
  val singleton : int -> edge -> weight -> t

  (** unite g1 g2 adds all edges of g1 to g2. *)
  val unite : t -> t -> t
  val set : edge -> weight -> t -> t
  val remove : edge -> t -> t
  val get : edge -> t -> weight
  val nodes : t -> int list
  val edges : t -> (edge * weight) list
  val is_edge : edge -> t -> bool

  (** num_share e g returns the number of endpoints that e shares with g. *)
  val num_share : edge -> t -> int

  (** is_adjacent e g tests whether e shares an endpoint with g. *)
  val is_adjacent : edge -> t -> bool

  (** find_path s t p g finds a s-t path whose all edges satisfies p by BFS.
      Raises Not_found when there is no such a path. *)
  val find_path : int -> int -> (weight -> bool) -> t -> path
  val fold_through : ('a -> edge -> weight -> 'a) -> 'a -> path -> t -> 'a
  val path_length : path -> t -> weight
end

module BaseGraph : functor (W : Weight) -> sig
  type weight = W.t
  type interedge = int * weight
  type t = interedge list array
  val head : edge -> int
  val tail : edge -> int
  val dest : interedge -> int
  val weight : interedge -> weight
  val is_endpoint : int -> interedge -> bool
  val empty : int -> t
  val is_empty : t -> bool
  val size : t -> int
  val exists : edge -> t -> bool
  val add' : edge -> weight -> t -> unit
  val remove' : edge -> t -> unit
  val head_nodes : t -> int list
  val tail_nodes : t -> int list
  val get : edge -> t -> weight
  val adjacents : int -> t -> interedge list
  val is_edge : edge -> t -> bool
  val find_path : int -> int -> (weight -> bool) -> t -> path
  val path_exists : path -> t -> bool
  val fold_through : ('a -> edge -> weight -> 'a) -> 'a -> path -> t -> 'a
  val path_length : path -> t -> weight
end
 = functor (W : Weight) -> struct
  type weight = W.t
  type interedge = int * weight
  type t = interedge list array
  let head (e : edge) = fst e
  let tail (e : edge) = snd e
  let dest (x : interedge) = fst x
  let weight (x : interedge) = snd x
  let is_endpoint p x = dest x = p
  let empty n = Array.make n []
  let is_empty = Array.for_all List.is_empty
  let size = Array.length
  let exists (h, t) g =  List.exists (is_endpoint t) g.(h)
  let add' (h, t) weight g = g.(h) <- (t, weight) :: g.(h)
  let remove' (h, t) g = g.(h) <- List.remove (is_endpoint t) g.(h)
  let head_nodes g = Array.to_list g
	   |> List.mapi (fun i dests -> match dests with [] -> None | _ -> Some i)
	   |> List.filter_some
  let tail_nodes g = Array.to_list g
	   |> List.flatten |> List.split |> fst |> IntSet.add_list IntSet.empty |> IntSet.elements
  let get (h, t) g = List.find (is_endpoint t) g.(h) |> snd
  let adjacents h g = g.(h)
  let is_edge e g = List.exists (fun x -> dest x = tail e) g.(head e)
  exception Found
  let find_path s t p g =
    let path_table = Array.make (size g) None in
    let rec find_path_aux to_search =
      if Queue2.is_empty to_search then raise Not_found
      else
	let (u, rest) = Queue2.pop to_search in
	let test q (v, w) =
	  if p w && path_table.(v) = None then path_table.(v) <- Some u;
	  if v = t then raise Found else Queue2.push v q
	in
	let rest = List.fold_left test rest (adjacents u g) in
	find_path_aux rest
    in
    try find_path_aux (Queue2.singleton s) with
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
    path_exists_aux (to_chain path)
  let fold_through f x path g =
    if not (path_exists path g) then raise (Invalid_argument "That path does not exist in the graph.");
    let rec fold_through_aux x = function
	[] -> x
      | hd :: tl -> fold_through_aux (f x hd (get hd g)) tl
    in
    fold_through_aux x (to_chain path)
  let path_length = fold_through (fun i _ w -> W.plus i w) W.zero
end

module type DirectedGraph = sig (* stub *)
  include Graph
end

module type UnirectedGraph = sig (* stub *)
  include Graph
end

(* loop and multigraph are prohibited *)
module MakeDirectedGraph (W : Weight) : DirectedGraph with type weight = W.t
  = struct
    include BaseGraph (W)
    let add e w g =
      if exists e g then raise (Invalid_argument "add : edge already exists.");
      add' e w g;
      g
    let singleton n e w = let g = empty n in add e w g; g
    let edges g = Array.to_list g
      |> List.mapi (fun i xs -> List.map (fun x -> ((i, dest x), weight x)) xs)
      |> List.flatten
    let unite g1 g2 = edges g1
      |> List.fold_left (fun g (t, w) ->
	   try add t w g
	   with Invalid_argument _ -> raise (Invalid_argument "unite : edge overlapping."))
	  g2
    let set e w g =
      if exists e g = false then raise (Invalid_argument "set : edge doesn't exists.");
      remove' e g;
      add' e w g;
      g
    let remove e g =
      remove' e g;
      g
    let nodes g = IntSet.add_list (IntSet.add_list IntSet.empty (head_nodes g)) (tail_nodes g) |> IntSet.elements
    let num_share (h, t) g = List.count (fun n -> n = h || n = t) (nodes g)
    let is_adjacent e g =
      num_share e g > 0 &&
	not (is_edge e g) &&
	not (is_edge (swap e) g)
  end

module MakeUndirectedGraph (W : Weight) : UnirectedGraph with type weight = W.t
  = struct
    include BaseGraph (W)
    let exists e g = exists e g || exists (swap e) g
    let add e w g =
      if exists e g then raise (Invalid_argument "add : edge already exists");
      add' e w g;
      add' (swap e) w g;
      g
    let singleton n e w = let g = empty n in add e w g; g
    let edges g = Array.to_list g
      |> List.mapi (fun i xs -> List.map
	   (fun x -> if i < dest x then Some ((i, dest x), weight x) else None)
	   xs)
      |> List.flatten
      |> List.filter_some
    let unite g1 g2 = edges g1
      |> List.fold_left (fun g (t, w) ->
	   try add t w g
	   with Invalid_argument _ -> raise (Invalid_argument "unite : edge overlapping."))
	  g2
    let set e w g =
      if exists e g = false then raise (Invalid_argument "set : edge doesn't exists");
      remove' e g;
      remove' (swap e) g;
      g
    let remove e g =
      remove' e g;
      remove' (swap e) g;
      g
    let nodes = head_nodes
    let num_share (h, t) g = List.count (fun n -> n = h || n = t) (nodes g)
    let is_adjacent e g =
      num_share e g > 0 && not (is_edge e g)
  end

module Unit = struct
  type t = unit
  let zero = ()
  let plus () () = ()
  let minus () () = ()
end

module UDG = MakeDirectedGraph(Unit)

let g1 = UDG.empty 5;;
UDG.add (3, 2) () g1;;
UDG.add (2, 4) () g1;;
UDG.add (3, 4) () g1;;

let g2 = UDG.empty 5;;
UDG.add (0, 1) () g2;;
UDG.add (0, 2) () g2;;
UDG.add (1, 2) () g2;;

UDG.unite g1 g2;;


