open Util

type direction = Left | Right | Center (* CounterClokWise | ClockWise | Line *)
type point2D = float * float
type line = point2D * point2D

module Point2D = struct
  type t = point2D
  let ( +^ ) (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
  let ( -^ ) (x1, y1) (x2, y2) = (x1 -. x2, y1 -. y2)
  let dot (x1, y1) (x2, y2) = x1 *. x2 +. y1 *. y2
  let cross (x1, y1) (x2, y2) = x1 *. y2 -. y1 *. x2
  let compare (x1, y1) (x2, y2) = compare x1 x2 * 2 + compare y1 y2
  let ccw a b c =
    let d = cross (b -^ a) (c -^ a) in
    if d > 0. then Left
    else if d < 0. then Right
    else Center
end

module Point2DSet' = Set.Make(Point2D)
module Point2DSet = struct
  include Point2DSet'
  let add_list = List.fold_left (swap_arg Point2DSet'.add)
  let remove_min s =
    let min = Point2DSet'.min_elt s in
    (min, Point2DSet'.remove min s)
  let remove_max s =
    let max = Point2DSet'.max_elt s in
    (max, Point2DSet'.remove max s)
end

let convex_hull points =
  let rec getout hull =
    if List.length hull < 3 then
      hull
    else
      let (c, r) = List.hd hull, List.tl hull in
      let (b, r) = List.hd r, List.tl r in
      let a = List.hd r in
      if Point2D.ccw a b c = Left then getout (List.delete_nth_naive 1 hull)
      else hull
  and aux is_upper pointset hull =
    if Point2DSet.is_empty pointset then
      hull
    else
      let (m, r) =
	(if is_upper then Point2DSet.remove_min else Point2DSet.remove_max) pointset
      in
      aux is_upper r (getout (m :: hull))
  in
  let pointset = Point2DSet.add_list Point2DSet.empty points  in
  let upper_hull =
    let (p0, removed) = Point2DSet.remove_min pointset in
    let (p1, removed) = Point2DSet.remove_min removed in
    aux true removed [p1;p0]
  and lower_hull =
    let (p0, removed) = Point2DSet.remove_max pointset in
    let (p1, removed) = Point2DSet.remove_max removed in
    aux false removed [p1;p0]
  in
  lower_hull @ (List.tl upper_hull)

