open Util
open Main

let dist (x1, y1) (x2, y2) = sqrt((x2 -. x1) ** 2. +. (y2 -. y1) ** 2.)
let mc points =
  let (x, y) = List.fold_left (fun (x1, y1) (x2, y2) -> (x1 +. x2, y1 +. y2)) (0., 0.) points in
  let denom = List.length points |> float in
  (x /. denom, y /. denom)

let random_point (c_x, c_y) sigma =
  let (rand_x, rand_y) = box_muller () in
  (c_x +. rand_x *. sigma, c_y +. rand_y *. sigma)

let random_points n =
  let a = Array.append
    (Array.init (n / 4) (fun _ -> random_point (1., 1.) 0.1))
    (Array.init (n / 4) (fun _ -> random_point (-1., -1.) 0.1))
  and b = Array.append
    (Array.init (n / 4) (fun _ -> random_point (-1., 1.) 0.1))
    (Array.init (n / 4) (fun _ -> random_point (1., -1.) 0.1))
  in
  Array.append a b

let points = random_points 100

let clustering_result = Main.kmeans dist mc points 4
