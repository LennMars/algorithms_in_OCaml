let debug = false

let find_freq x =
  let open String in
  let rec aux x len remain =
    if debug then Printf.printf "%s, %d, %c\n" (sub x 0 len) len remain;
    match len with
    | 1 -> get x 0
    | 0 -> remain
    | _ ->
	let rec pair i len_next =
	  if i = len / 2 then (* problem size halved *)
	    (len_next, if len land 1 = 1 then get x (len - 1) else remain)
	  else match get x (2 * i) with
	    c when c = get x (2 * i + 1) ->
	      set x len_next c; (* constant time operation *)
	      pair (i + 1) (len_next + 1)
	  | _ ->
	      pair (i + 1) len_next
	in
	let (len_next, remain_next) = pair 0 0 in
	aux x len_next remain_next
  in
  aux (copy x) (length x) ' ';;
