open Util
open IntSet

let eratosthenes n =
  let rec eratosthenes_aux primes remains =
    let minimum = min_elt remains in
    if minimum * minimum > (max_elt remains) then
      union primes remains
    else
      eratosthenes_aux (add minimum primes) (filter (fun m -> m mod minimum <> 0) remains)
  in
  List.range 3 n 2 |> add_list (singleton 2) |> eratosthenes_aux empty |> elements
