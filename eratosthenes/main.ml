open Util
open IntSet

let eratosthenes n =
  let rec eratosthenes_aux primes remains =
    let minimum = min_elt remains in
    if minimum * minimum > n then
      union primes remains
    else
      eratosthenes_aux (add minimum primes) (filter (fun m -> m mod minimum <> 0) remains)
  in
  List.range 2 n 1 |> add_list empty |> eratosthenes_aux empty |> elements
