(* ref : http://ja.wikipedia.org/wiki/クイックソート *)
open Util;;

Random.self_init ()

let select_pivot xs = xs.(Random.int (Array.length xs))

let rec move_by_swap comp pivot xs =
  try
    let i = Array.find_num_left (fun x -> comp pivot x <= 0) xs (* todo : search start point optimization *)
    and j = Array.find_num_right (fun x -> comp pivot x > 0) xs
    in
    if i = j then
      failwith "there are bug(s) in Util.Array.find_num_* "
    else if i < j then begin
      Array.swap i j xs;
      move_by_swap comp pivot xs
    end
    else
      i
  with
    Not_found -> (* The case that the pivot is minimum. There is room for improvement. *)
      Array.swap 0 (Array.find_num_left ((=) pivot) xs) xs;
      1

let quicksort comp xs =
  let rec quicksort_aux xs =
    if Array.length xs = 1 then
      xs
    else
      let pivot = select_pivot xs in
      let i = move_by_swap comp pivot xs in
      let (left, right) = Array.split i xs in
      Array.append (quicksort_aux left) (quicksort_aux right)
  in
  quicksort_aux xs
