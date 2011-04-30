open UtilPervasives

let sub n len xs =
  let xs = Array.of_list xs in
  try Array.sub xs n len |> Array.to_list with
    Invalid_argument _ -> raise (Invalid_argument "List.sub")

let take n xs =
  let rec aux n xs accum =
    if n <= 0 then List.rev accum
    else if xs = [] then raise (Invalid_argument "take : too much length")
    else aux (n - 1) (List.tl xs) (List.hd xs :: accum)
  in
  aux n xs []

let rec drop n xs =
  if n <= 0 then xs
  else if xs = [] then raise (Invalid_argument "drop : too much length")
  else drop (n - 1) (List.tl xs)

let rec drop_while p xs =
  if xs = [] then []
  else if p (List.hd xs) then drop_while p (List.tl xs)
  else xs

let delete_nth n xs =
  if n < 0 then
    raise (Invalid_argument "delete_nth")
  else if n = 0 then
    List.tl xs
  else if n = 1 then
    List.hd xs :: (List.tl (List.tl xs))
  else
    let tl_field xsr = Obj.field xsr 1 in
    let rec aux n xsr_orig xsr =
      let tl = xsr |> tl_field |> tl_field in
      if n > 0 && Obj.obj tl = 0 then
	raise (Invalid_argument "delete_nth")
      else if n = 0 then let _ = begin
	if Obj.obj tl = 0 then
	  Obj.set_field xsr 1 (Obj.repr 0)
	else
	  Obj.set_field xsr 1 tl
      end in Obj.obj xsr_orig
      else aux (n - 1) xsr_orig (Obj.field xsr 1)
    in
    aux (n - 1) (Obj.repr xs) (Obj.repr xs |> Obj.dup)

let delete_nth_naive n xs =
  let rec aux n xs accum =
    if n <= 0 then List.rev accum @ List.tl xs
    else aux (n - 1) (List.tl xs) (List.hd xs :: accum)
  in
  aux n xs []


let is_empty xs = List.length xs = 0

let single xs = if List.length xs = 1 then List.hd xs else
  raise (Invalid_argument "The length of the list must be 1.")

let last xs = List.nth xs (List.length xs - 1)

let to_chain = function (* ex. [1;2;3] -> [(1, 2); (2, 3)]*)
    [] | [_] -> []
  | hd :: tl ->
      let rec to_chain_aux accum = function
	  [] -> failwith "to_chain : fatal error."
	| [last] -> List.rev accum
	| hd :: tl -> to_chain_aux ((hd, List.hd tl) :: accum) tl
      in
      to_chain_aux [hd, List.hd tl] tl

let filter_some xs = List.filter (fun x -> match x with Some _ -> true | None -> false) xs
  |> List.map (fun x -> match x with Some y -> y | None -> failwith "fatal error")

let rec print_int_list = function
    [] -> Printf.printf "\n"
  | hd :: tl -> Printf.printf "%d " hd; print_int_list tl

let rec print_float_list = function
    [] -> Printf.printf "\n"
  | hd :: tl -> Printf.printf "%d " hd; print_float_list tl

let remove cond =
  let rec remove_aux accum = function
      [] -> List.rev accum (* unfound *)
    | hd :: tl -> if cond hd then List.rev accum @ tl else remove_aux (hd :: accum) tl
  in
  remove_aux []

let find_max_with comp f = function
    [] -> raise (Invalid_argument "find_max_with")
  | hd :: tl -> List.fold_left (fun x y -> if comp (f x) (f y) > 0 then x else y) hd tl

let find_min_with comp = find_max_with (swap_arg comp)

let find_max_val_with comp f = function
    [] -> raise (Invalid_argument "find_max_val_with")
  | hd :: tl -> List.fold_left (fun x y -> let fy = f y in if comp x fy > 0 then x else fy) (f hd) tl

let find_min_val_with comp = find_max_val_with (swap_arg comp)

let find_max f = find_max_with compare f

let find_min f = find_min_with compare f

let find_max_val f = find_max_val_with compare f

let find_min_val f = find_min_val_with compare f

let mapi f =
  let rec mapi_aux f accum n = function
      [] -> List.rev accum
    | x :: xs -> mapi_aux f (f n x :: accum) (n + 1) xs
  in
  mapi_aux f [] 0

let count cond xs = List.filter cond xs |> List.length

let rev_flatten xs =
  let rec rev_flatten_aux accum = function
      [] -> accum
    | hd :: tl ->
	let rec rev_flatten_deep accum = function
	    [] -> accum
	  | hd :: tl -> rev_flatten_deep (hd :: accum) tl
	in
	rev_flatten_aux (rev_flatten_deep accum hd) tl
  in
  rev_flatten_aux [] xs

let flatten xs = rev_flatten xs |> List.rev

let concat = flatten

let init f n =
  let rec init_aux n accum =
    if n <= 0 then accum else
      init_aux (n - 1) (f (n - 1) :: accum)
  in
  init_aux n []

let make n x =
  let rec make_aux n accum =
    if n <= 0 then accum else
      make_aux (n - 1) (x :: accum)
  in
  make_aux n []

let map_tail f =
  let rec map_tail_aux accum = function
      [] -> List.rev accum
    | hd :: tl -> map_tail_aux (f hd :: accum) tl
  in
  map_tail_aux []

let average_first n lst =
  let n = min n (List.length lst) in
  let rec sum_first n accum lst=
    if n = 0 then accum else
      match lst with
	[] -> failwith "ave"
      | hd :: tl -> sum_first (n - 1) (hd +. accum) tl
  in
  (sum_first n 0. lst) /. float n

let average lst =
  average_first (List.length lst) lst

let split_while f lst =
  let rec split_while_aux f accum_fst accum_snd = function
      [] -> (List.rev accum_fst, accum_snd)
    |hd :: tl ->
       if f hd then
	 split_while_aux f (hd :: accum_fst) tl tl
       else
	 (List.rev accum_fst, accum_snd)
  in
  split_while_aux f [] lst lst

let range a b inc =
  let rec aux a b inc accum =
    if a > b then accum
    else aux a (b - inc) inc (b :: accum)
  in
  if inc = 0 then raise (Invalid_argument "range : increment must be positive.")
  else if inc > 0 then aux a (b - (b - a) mod inc) inc []
  else aux b a (-inc) [] |> List.rev

let take_ns ns lst =
  let rec take_ns_aux ns lst l accum =
    if lst == [] || ns == [] then List.rev accum else
      if l == List.hd ns then
	take_ns_aux (List.tl ns) (List.tl lst) (l + 1) (List.hd lst :: accum)
      else
	take_ns_aux ns (List.tl lst) (l + 1) accum
  in
  take_ns_aux ns lst 0 [] (* preserves order *)

let random_permutation n =
  let rec aux accum emerged n =
    if n <= 0 then accum
    else
      let r = Random.int n in
      let r = IntSet.fold (fun e r -> if e <= r then r + 1 else r) emerged r in
      aux (r :: accum) (IntSet.add r emerged) (n - 1)
  in
  aux [] IntSet.empty n

let remove_adjacent = function
    [] -> []
  | hd :: tl ->
      let rec aux pred accum xs =
	match xs with
	  [] -> List.rev accum
	| hd :: tl -> if hd = pred then aux pred accum tl else aux hd (hd :: accum) tl
      in
      aux hd [hd] tl

let remove_adjacent_tuple m xs =
  let rec remove pattern xs = (* Some : succeed to remove, None : failed to remove *)
    if pattern = [] then
      Some xs
    else if xs = [] then
      None
    else if List.hd pattern = List.hd xs then
      remove (List.tl pattern) (List.tl xs)
    else
      None
  in
  let rec remove_eager pattern xs =
    match remove pattern xs with
      None -> xs
    | Some xs' -> remove_eager pattern xs'
  in
  let rec aux pred xs accum =
    let removed = remove_eager pred xs in
    if removed = [] then
      List.rev accum
    else
      aux (List.tl pred @ [List.hd removed]) (List.tl removed) (List.hd removed :: accum)
  in
  aux (take m xs) (drop m xs) (take m xs |> List.rev)

let n_divide n k =
  if n <= 0 || k <= 0 then raise (Invalid_argument "n_divide");
  let rec aux n k =
    if k = 1 then
      [[n]]
    else
      let rec aux2 n k i accum =
	if i <= 0 then accum
	else
	  let shorten = List.map (fun xs -> i :: xs) (aux (n - i) (k - 1)) in
	  aux2 n k (i - 1) (shorten @ accum)
      in
      aux2 n k (n - 1) []
  in
  aux n k

