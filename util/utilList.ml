open UtilPervasives

let iteri f xs =
  let rec aux n = function
    | [] -> ()
    | hd :: tl -> f n hd; aux (n + 1) tl
  in
  aux 0 xs

let sub start len xs =
  try
    let rec skip i xs =
      if i = start then xs
      else skip (i + 1) (List.tl xs)
    and drop j acc xs =
      if j = len then List.rev acc
      else drop (j + 1) (List.hd xs :: acc) (List.tl xs)
    in
    skip 0 xs |> drop 0 []
  with Failure "tl" -> invalid_arg "Util.List.sub"

let take n xs =
  let rec aux n xs accum =
    if n <= 0 || xs = [] then List.rev accum
    else aux (n - 1) (List.tl xs) (List.hd xs :: accum)
  in
  aux n xs []

let take_option n_option xs =
  let rec aux n xs acc =
    if n <= 0 then List.rev acc
    else match xs with
      | [] -> List.rev acc
      | l :: r -> aux (n - 1) r (l :: acc) in
  match n_option with
    | None -> xs
    | Some n -> aux n xs []

let rec take_even_aux acc is_even = function
  | [] -> List.rev acc
  | l :: r -> take_even_aux (if is_even then (l :: acc) else acc) (not is_even) r

let take_even xs = take_even_aux [] true xs

let take_odd xs = take_even_aux [] false xs

let rec drop n xs =
  if n <= 0 || xs = [] then xs
  else drop (n - 1) (List.tl xs)

let rec split_at n xs =
  let rec aux n xs acc =
    if n <= 0 || xs = [] then List.rev acc, xs
    else aux (n - 1) (List.tl xs) (List.hd xs :: acc)
  in
  aux n xs []

let rec drop_while p xs =
  if xs = [] then []
  else if p (List.hd xs) then drop_while p (List.tl xs)
  else xs

let delete_nth n xs =
  if n < 0 then
    raise (Invalid_argument "Util.List.delete_nth")
  else if n = 0 then
    List.tl xs
  else if n = 1 then
    List.hd xs :: (List.tl (List.tl xs))
  else
    let tl_field xsr = Obj.field xsr 1 in
    let rec aux n xsr_orig xsr =
      let tl = xsr |> tl_field |> tl_field in
      if n > 0 && Obj.obj tl = 0 then
        raise (Invalid_argument "Util.List.delete_nth")
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
  raise (Invalid_argument "Util.List.single: The length of the list must be 1.")

let last xs = List.nth xs (List.length xs - 1)

let to_chain = function (* ex. [1;2;3] -> [(1, 2); (2, 3)]*)
    [] | [_] -> []
  | hd :: tl ->
      let rec to_chain_aux accum = function
        | [] -> failwith "Util.List.to_chain : fatal error."
        | [last] -> List.rev accum
        | hd :: tl -> to_chain_aux ((hd, List.hd tl) :: accum) tl
      in
      to_chain_aux [hd, List.hd tl] tl

let filter_some xs = List.filter (fun x -> match x with Some _ -> true | None -> false) xs
  |> List.map (function Some y -> y | None -> failwith "Util.List.filter_some: fatal error")

let rec print_int_list = function
  | [] -> Printf.printf "\n"
  | hd :: tl -> Printf.printf "%d " hd; print_int_list tl

let rec print_float_list = function
  | [] -> Printf.printf "\n"
  | hd :: tl -> Printf.printf "%d " hd; print_float_list tl

let remove cond =
  let rec remove_aux accum = function
    | [] -> List.rev accum (* unfound *)
    | hd :: tl -> if cond hd then List.rev accum @ tl else remove_aux (hd :: accum) tl
  in
  remove_aux []

let find_max ?(comp = Pervasives.compare) f xs =
  let rec aux (max, max_val) = function
    | [] -> max
    | l :: r ->
      let y = f l in
      aux (if comp y max_val > 0 then l, y else max, max_val) r in
  match xs with
    | [] -> raise (Invalid_argument "Util.List.find_max")
    | l :: r -> aux (l, f l) r

let find_min ?(comp = Pervasives.compare) = find_max ~comp:(swap_arg comp)

let find_max_val ?(comp = Pervasives.compare) f = function
  | [] -> invalid_arg "Util.List.find_max_val_with"
  | hd :: tl -> List.fold_left (fun x y -> let fy = f y in if comp x fy > 0 then x else fy) (f hd) tl

let find_min_val ?(comp = Pervasives.compare) = find_max_val ~comp:(swap_arg comp)

let mapi f =
  let rec mapi_aux f accum n = function
    | [] -> List.rev accum
    | x :: xs -> mapi_aux f (f n x :: accum) (n + 1) xs
  in
  mapi_aux f [] 0

let count cond xs = List.filter cond xs |> List.length

let rev_flatten xs =
  let rec rev_flatten_aux accum = function
    | [] -> accum
    | hd :: tl ->
      let rec rev_flatten_deep accum = function
        | [] -> accum
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

let map_orig = List.map

let map f =
  let rec aux accum = function
    | [] -> List.rev accum
    | hd :: tl -> aux (f hd :: accum) tl
  in
  aux []

let average_first n lst =
  let n = min n (List.length lst) in
  let rec sum_first n accum lst=
    if n = 0 then accum else
      match lst with
        | [] -> invalid_arg "Util.List.average_first"
        | hd :: tl -> sum_first (n - 1) (hd +. accum) tl
  in
  (sum_first n 0. lst) /. float n

let average lst =
  average_first (List.length lst) lst

let split_while f lst =
  let rec split_while_aux f accum_fst accum_snd = function
    | [] -> (List.rev accum_fst, accum_snd)
    | hd :: tl ->
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
  if inc = 0 then raise (Invalid_argument "Util.List.range : increment must be positive.")
  else if inc > 0 && a <= b then aux a (b - (b - a) mod inc) inc []
  else if inc < 0 && a >= b then aux b a (-inc) [] |> List.rev
  else []

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
  | [] -> []
  | hd :: tl ->
    let rec aux pred accum xs =
      match xs with
        | [] -> List.rev accum
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
      | None -> xs
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
  if n <= 0 || k <= 0 then raise (Invalid_argument "Util.List.n_divide");
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

let combination n m =
  let g (k, r) = init (fun i -> k + int_exp 2 (n - i - 1), i) r in
  let rec aux m xs =
    if m = 1 then
      map fst xs
    else
      aux (m - 1) (map g xs |> List.concat)
  in
  aux m (init (fun i -> int_exp 2 i, n - i - 1) n);;

let remove_duplicated f =
  let rec aux acc = function
    | [] -> acc
    | x :: xs ->
      if List.exists ((=) (f x)) (map f acc) then aux acc xs
      else aux (x :: acc) xs in
  aux []

let remove_duplicated xs =
  let open Hashtbl in
  let table = create 100 in
  List.iter (fun x -> replace table x ()) xs;
  fold (fun x () acc -> x :: acc) table []

let tuplize xs ys =
  List.map (fun x -> List.map (fun y -> (x, y)) ys) xs |> List.flatten

let pack ?(comp = Pervasives.compare) xs =
  let rec aux last count acc = function
    | [] -> (last, count) :: acc
    | l :: r when comp last l = 0 -> aux last (count + 1) acc r
    | l :: r -> aux l 1 ((last, count) :: acc) r in
  match List.sort comp xs with
    | [] -> []
    | l :: r -> aux l 1 [] r

let filter_map f xs =
  let rec aux acc = function
    | [] -> List.rev acc
    | l :: r -> match f l with
        | Some x -> aux (x :: acc) r
        | None -> aux acc r in
  aux [] xs
