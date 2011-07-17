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

open Bigarray;;

let eratosthenes_better n = (* more fast *)
  let nums = Array1.create int c_layout (n + 1) in
  for i = 0 to n do
    Array1.set nums i i
  done;
  let term = sqrt (float n) |> int_of_float |> ( + ) 1 in
  let rec aux m j =
    if j > n then ()
    else let _ = Array1.set nums j 0 in aux m (j + m)
  in
  let rec aux2 k =
    if k > term then ()
    else let _ = aux k (2 * k) in aux2 (k + 1)
  in
  let _ = aux2 2 in
  let rec make_ans i accum =
    if i = 0 then accum
    else make_ans (i - 1) (let j = Array1.get nums i in if j = 0 then accum else j :: accum)
  in
  make_ans n [] |> List.filter ((<>) 0) |> List.tl
