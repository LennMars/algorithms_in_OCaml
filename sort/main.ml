open Util;;

Random.self_init ()

let insertsort comp xs =
  let rec target pos value xs =
    if pos = 0 then 0
    else if comp xs.(pos - 1) value <= 0 then pos
    else target (pos - 1) value xs
  in
  for pos = 1 to Array.length xs - 1 do
    let value = xs.(pos) in
    let t = target pos value xs in
    Array.blit xs t xs (t + 1) (pos - t);
    xs.(t) <- value
  done

let quicksort comp xs =
  let n = Array.length xs in
  let rec quicksort_aux xs s e =
    if s >= e - 1  then
      ()
    else
      let rec move_by_swap comp pivot s e xs =
	try
	  let i = Array.find_num_left ~start:s (fun x -> comp pivot x <= 0) xs
	  and j = Array.find_num_right ~start:(n - e) (fun x -> comp pivot x > 0) xs
	  in
	  if i = j then
	    failwith "There are bug(s) in Util.Array.find_num_* "
	  else if i < j then begin
	    Array.swap i j xs;
	    move_by_swap comp pivot i j xs
	  end
	  else
	    i
	with
	  Not_found -> s
      in
      let select_pivot xs s e = xs.(Random.int (e - s) + s) in
      let pivot = select_pivot xs s e in
      let i = move_by_swap comp pivot s e xs in
      quicksort_aux xs s i;
      quicksort_aux xs i e
  in
  quicksort_aux xs 0 n

