open Util

module Int = struct
  type t = int
  let compare = compare
end

module IntSet = Set.Make(Int)

let eratosthenes n =
  let remove_multiple base max set =
    let rec remove_multiple_aux n set =
    if n > max then set
    else remove_multiple_aux (n + base) (IntSet.remove n set)
    in
    remove_multiple_aux base set
  in
  let rec eratosthenes_aux set =
    let minimum = IntSet.min_elt set
    and maximum = IntSet.max_elt set
    in
    if minimum * minimum > maximum then
      set
    else
      eratosthenes_aux (remove_multiple minimum maximum set)
  in
  List.range 2 n |> List.fold_left (swap_arg IntSet.add) IntSet.empty |> eratosthenes_aux |> IntSet.elements
