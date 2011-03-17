open Util

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

  val add : int * int -> weight -> t -> t
  val set : int * int -> weight -> t -> t
  val remove : int * int -> t -> t
  val get : int * int -> t -> weight
  val nodes : t -> int list
end

module MakeDirectedGraph (W : Weight) : Graph with type weight = W.t
  = struct
    type weight = W.t
    type t = (int * weight) list array
    let is_endpoint e x = fst x = e
    let empty n = Array.make n []
    let is_empty = Array.for_all List.is_empty
    let exists (s, e) g = List.exists (is_endpoint e) g.(s)
    let add (s, e) weight g =
      if exists (s, e) g then raise (Invalid_argument "edge already exists");
      g.(s) <- (e, weight) :: g.(s);
      g
    let set (s, e) weight g =
      if exists (s, e) g = false then raise (Invalid_argument "edge doesn't exists");
      g.(s) <- List.remove (is_endpoint e) g.(s);
      g.(s) <- (e, weight) :: g.(s);
      g
    let remove (s, e) g =
      g.(s) <- List.remove (is_endpoint e) g.(s);
      g
    let get (s, e) g = List.find (is_endpoint e) g.(s) |> snd
    let nodes g = Array.mapi (fun i ends -> match ends with [] -> None | _ -> Some i) g
	     |> Array.to_list
	     |> List.filter (fun x -> match x with Some _ -> true | None -> false)
	     |> List.map (fun x -> match x with Some y -> y | None -> failwith "error")
  end

module MakeUndirectedGraph (W : Weight) : Graph with type weight = W.t
  = struct
    type weight = W.t
    type t = (int * weight) list array
    let is_endpoint e x = fst x = e
    let empty n = Array.make n []
    let is_empty = Array.for_all List.is_empty
    let exists (s, e) g = List.exists (is_endpoint e) g.(s)
    let add (s, e) weight g =
      if exists (s, e) g then raise (Invalid_argument "edge already exists");
      g.(s) <- (e, weight) :: g.(s);
      g.(e) <- (s, weight) :: g.(e);
      g
    let set (s, e) weight g =
      if exists (s, e) g = false then raise (Invalid_argument "edge doesn't exists");
      g.(s) <- List.remove (is_endpoint e) g.(s);
      g.(s) <- (e, weight) :: g.(s);
      g.(e) <- List.remove (is_endpoint s) g.(e);
      g.(e) <- (s, weight) :: g.(e);
      g
    let remove (s, e) g =
      if exists (s, e) g = false then raise (Invalid_argument "edge doesn't exists");
      g.(s) <- List.remove (is_endpoint e) g.(s);
      g.(e) <- List.remove (is_endpoint s) g.(e);
      g
    let get (s, e) g = List.find (is_endpoint e) g.(s) |> snd
    let nodes g = Array.mapi (fun i ends -> match ends with [] -> None | _ -> Some i) g
	     |> Array.to_list
	     |> List.filter (fun x -> match x with Some _ -> true | None -> false)
	     |> List.map (fun x -> match x with Some y -> y | None -> failwith "error")
  end





