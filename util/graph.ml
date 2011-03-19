open Util

type edge = int * int

module type Weight = sig
  type t
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
  val is_edge : edge -> t -> bool
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
  let is_edge e g = List.exists (fun x -> dest x = tail e) g.(head e)
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
      |> List.fold_left (fun g we ->
	   try add (fst we) (snd we) g
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
      |> List.fold_left (fun g we ->
	   try add (fst we) (snd we) g
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



