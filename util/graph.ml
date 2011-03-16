open Util

module type Anything = sig
  type t
end

module type DirectedGraph = sig
  type weight
  type t
  val empty : int -> t
  val is_empty : t -> bool
  val add : int * int -> weight -> t -> unit
  val set : int * int -> weight -> t -> unit
  val remove : int * int -> t -> unit
  val get : int * int -> t -> weight
  val nodes : t -> int list
end

module MakeImperativeDirectedGraph (A : Anything) : DirectedGraph with type weight = A.t
  = struct
    type weight = A.t
    type t = (int * weight) list array
    let is_endpoint e x = fst x = e
    let empty n = Array.make n []
    let is_empty = Array.for_all List.is_empty
    let exists (s, e) g = List.exists (is_endpoint e) g.(s)
    let add (s, e) weight g =
      if exists (s, e) g then raise (Invalid_argument "edge already exists");
      g.(s) <- (e, weight) :: g.(s)
    let set (s, e) weight g =
      if exists (s, e) g = false then raise (Invalid_argument "edge doesn't exists");
      g.(s) <- List.remove (is_endpoint e) g.(s);
      g.(s) <- (e, weight) :: g.(s)
    let remove (s, e) g = g.(s) <- List.remove (is_endpoint e) g.(s)
    let get (s, e) g = List.find (is_endpoint e) g.(s) |> snd
    let nodes g = Array.mapi (fun i ends -> match ends with [] -> None | _ -> Some i) g
	     |> Array.to_list
	     |> List.filter (fun x -> match x with Some _ -> true | None -> false)
	     |> List.map (fun x -> match x with Some y -> y | None -> failwith "error")
  end

module Int = struct
  type t = int
end

module IDG = MakeImperativeDirectedGraph(Int)




