open Util

module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module Make(Ord : OrderedType) : sig
  type elt = Ord.t
  type t= Nil | Node of elt * t * t
  val empty : t
  val find : elt -> t -> elt * t
  val insert : elt -> t -> t
  val print : (elt -> unit) -> t -> unit
end =
struct
  type elt = Ord.t
  type t = Nil | Node of elt * t * t

  let (=~) x y = Ord.compare x y = 0
  let (<~) x y = Ord.compare x y < 0
  let (>~) x y = Ord.compare x y > 0

  let empty = Nil

  let rot_right = function
      Node (y, Node(z, l', r'), r) -> Node (z, l', Node (y, r', r))
    | _ ->  failwith "rot_right : Nil must be leaf."

  let rot_left = function
      Node (y, l, Node (z, l', r')) -> Node (z, Node (y, l, l'), r')
    | _ -> failwith "rot_left : Nil must be leaf."

  let zig x = function
      Node (_, Node(y, _, _), _) as n when x =~ y -> rot_right n
    | Node (_, _, Node(y, _, _)) as n when x =~ y -> rot_left n
    | _ as n -> n

  let splay x = function
      Nil -> raise Not_found
    | Node (y, l, r) as a ->
	if x =~ y then a
	else if x <~ y then match l with
	  Node (_, Node(z, _, _), _) when x =~ z -> rot_right (rot_right a) (* zig-zig *)
	| Node (_, _, Node (z, _, _)) when x =~ z -> rot_right (Node(y, (rot_left l), r)) (* zig-zag *)
	| _ -> a
	else match r with
	  Node (_, _, Node (z, _, _)) when x =~ z -> rot_left (rot_left a)
	| Node (_, Node(z, _, _), _) when x =~ z -> rot_left (Node(y, l, rot_right r))
	| _ -> a

  let find x t =
    let found = ref x in
    let rec aux = function
	Nil -> raise Not_found
      | Node (y, _, _) as n when x =~ y -> found := y; splay x n
      | Node (y, l, r) when x <~ y -> Node (y, splay x (aux l), r)
      | Node (y, l, r) -> Node (y, l, splay x (aux r))
    in
    !found, aux t |> splay x |> zig x

  let insert x t =
    let rec aux = function
   	Nil -> Node (x, Nil, Nil)
      | Node (y, _, _) when x =~ y -> raise (Invalid_argument "insert : already exists")
      | Node (y, l, r) when x <~ y -> Node (y, splay x (aux l), r)
      | Node (y, l, r) -> Node (y, l, splay x (aux r))
    in
    aux t |> splay x |> zig x

  let rec height = function
      Nil -> 0
    | Node (_, l, r) -> max (height l + 1) (height r + 1)

  let print p t =
    let elms = Array.make (height t) [] in
    let rec aux t h = match t with
	Nil -> ()
      | Node (x, l, r) ->
	  elms.(h) <- x :: elms.(h);
	  aux l (h + 1);
	  aux r (h + 1)
    in
    aux t 0;
    Array.iter (fun l -> List.iter (fun x -> p x; Printf.printf " ") (List.rev l); Printf.printf "\n") elms
end
