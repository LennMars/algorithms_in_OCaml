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

  val add : edge -> weight -> t -> t
  val set : edge -> weight -> t -> t
  val remove : edge -> t -> t
  val get : edge -> t -> weight
  val nodes : t -> int list
  val is_edge : edge -> t -> bool
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
  val exists : edge -> t -> bool
  val add' : edge -> weight -> t -> unit
  val remove' : edge -> t -> unit
  val nodes : t -> int list
  val get : edge -> t -> weight
  val is_edge : edge -> t -> bool
end
 = functor (W : Weight) -> struct
  type weight = W.t
  type interedge = int * weight
  type t = interedge list array
  let head = fst
  let tail = snd
  let dest = fst
  let weight = snd
  let is_endpoint p x = dest x = p
  let empty n = Array.make n []
  let is_empty = Array.for_all List.is_empty
  let exists (h, t) g =  List.exists (is_endpoint t) g.(h)
  let add' (h, t) weight g = g.(h) <- (t, weight) :: g.(h)
  let remove' (h, t) g = g.(h) <- List.remove (is_endpoint t) g.(h)
  let nodes g = Array.mapi (fun i ends -> match ends with [] -> None | _ -> Some i) g
	   |> Array.to_list
	   |> List.filter (fun x -> match x with Some _ -> true | None -> false)
	   |> List.map (fun x -> match x with Some y -> y | None -> failwith "error")
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
      if exists e g then raise (Invalid_argument "edge already exists");
      add' e w g;
      g
    let set e w g =
      if exists e g = false then raise (Invalid_argument "edge doesn't exists");
      remove' e g;
      add' e w g;
      g
    let remove e g =
      remove' e g;
      g
  end

module MakeUndirectedGraph (W : Weight) : UnirectedGraph with type weight = W.t
  = struct
    include BaseGraph (W)
    let exists e g = exists e g || exists (swap e) g
    let add e w g =
      if exists e g then raise (Invalid_argument "edge already exists");
      add' e w g;
      add' (swap e) w g;
      g
    let set e w g =
      if exists e g = false then raise (Invalid_argument "edge doesn't exists");
      remove' e g;
      remove' (swap e) g;
      g
    let remove e g =
      if exists e g = false then raise (Invalid_argument "edge doesn't exists");
      remove' e g;
      remove' (swap e) g;
      g
  end

module Int = struct
  type t = int
end

module Test = MakeDirectedGraph(Int)

let g = Test.empty;;



