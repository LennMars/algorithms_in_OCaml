open Util

module Int = struct
  type t = int
  let compare = compare
end

module IntSet = Set.Make(Int)

let eratosthenes n =
  let rec eratosthenes_aux set =
    let minimum = IntSet.min_elt set
    in
    if minimum * minimum > n then
      set
    else
      eratosthenes_aux (IntSet.filter (fun m -> m mod minimum <> 0) set)
  in
  List.range 2 n |> List.fold_left (swap_arg IntSet.add) IntSet.empty |> eratosthenes_aux |> IntSet.elements
