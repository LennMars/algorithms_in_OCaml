open Util

module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module Make(Ord : OrderedType) : sig
  type elt = Ord.t
  type t
  val empty : t
  val find : elt -> t -> elt
  val insert : elt -> t -> t
end =
struct
  type color = Red | Black
  type elt = Ord.t
  type t = Nil | Node of color * elt * t * t

  let (=~) x y = Ord.compare x y = 0
  let (<~) x y = Ord.compare x y < 0
  let (>~) x y = Ord.compare x y > 0

  let empty = Nil

  let rec find x = function
      Nil -> raise Not_found
    | Node (_, y, _, _) when x =~ y -> y
    | Node (_, y, l, _) when x <~ y -> find x l
    | Node (_, y, _, r) -> find x r
  let rec apply_at f x = function
      Nil -> raise Not_found
    | Node (_, y, _, _) as n when x =~ y -> f n
    | Node (c, y, l, r) when x <~ y -> Node (c, y, apply_at f x l, r)
    | Node (c, y, l, r) -> Node (c, y, l, apply_at f x r)
  let rot_right = function
      Node (c, y, Node(c', z, l', r'), r) -> Node (c', z, l', Node (c, y, r', r))
    | _ ->  failwith "rot_right : Nil must be leaf."
  let rot_left = function
      Node (c, y, l, Node (c', z, l', r')) -> Node (c', z, Node (c, y, l, l'), r')
    | _ -> failwith "rot_left : Nil must be leaf."
  type position = Left | Right | None
  let insert x t =
    let double_red = function
	Node (Red, _, (Node (Red, _, _, _)), _) -> Left
      | Node (Red, _, _, (Node (Red, _, _, _))) -> Right
      | _ -> None
    and paint c p t = match (p, t) with
	(Left, Node (cc, x, Node (_, y, a, b), r)) -> Node (cc, x, Node (c, y, a, b), r)
      | (Right, Node (cc, x, l, Node (_, y, a, b))) -> Node (cc, x, l, Node (c, y, a, b))
      | (None, Node (_, x, l, r)) -> Node (c, x, l, r)
      | _  -> failwith "paint : attempt to paint Nil."
    in
    let balance c y l r =
      match (double_red l, double_red r) with
	(Left, None) -> Node (c, y, l, r) |> rot_right |> paint Black Left
      | (Right, None) -> Node (c, y, rot_left l, r) |> rot_right |> paint Black Left
      | (None, Left) -> Node (c, y, l, rot_right r) |> rot_left |> paint Black Right
      | (None, Right) -> Node (c, y, l, r) |> rot_left |> paint Black Right
      | (None, None) -> Node (c, y, l, r)
      | _ -> failwith "balance : fatal error."
    in
    let rec insert_aux x = function
	Nil -> Node (Red, x, Nil, Nil)
      | Node (_, y, l, r) as n when x =~ y -> n
      | Node (c, y, l, r) when x <~ y -> balance c y (insert_aux x l) r
      | Node (c, y, l, r) -> balance c y l (insert_aux x r)
    in
    insert_aux x t |> paint Black None
end
