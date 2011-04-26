let identity x = x

let (|>) f x = x f

let (<|) f x = f x

let swap (a, b) = (b, a)

let swap_arg f x y = f y x

let maxf f x y = max (f x) (f y)

let xor x y = x && (not y) || y && (not x)

let incr x = x := !x + 1

let triple1 (x, _, _) = x
let triple2 (_, x, _) = x
let triple3 (_, _, x) = x

let pi = atan 1. *. 4.

let rec iter n f x =
  if n <= 0 then ()
  else begin
    f x;
    iter (n - 1) f x
  end;;

let time timer n f x =
  let start = timer () in
  iter n f x;
  (timer () -. start) /. float n


let general_exp one mul x y =
  if y < 0 then failwith "y must be positive";
  let is_even_list =
    let rec is_even_list_ y accum =
      if y = 0 then accum
      else if (y mod 2 = 0) then is_even_list_ (y lsr 1) (true::accum)
      else is_even_list_ (y lsr 1) (false::accum)
    in
    is_even_list_ y []
  in
  let  rec int_exp_aux x is_even_list accum = match is_even_list with
      [] -> accum
    | hd :: tl -> int_exp_aux x tl (mul (mul accum accum) (if hd then one else x))
  in
  int_exp_aux x is_even_list one

let int_exp = general_exp 1 ( * )

let minimum_bigger_power_of_two n =
  let rec aux accum = function
      0 -> accum
    | m -> aux (accum + 1) (m lsr 1)
  in
  aux 0 (n-1)

let string_of_option to_string = function
    None -> "None"
  | Some x -> to_string x

module Int = struct type t = int let compare = compare end
module IntSet' = Set.Make (Int)
module IntSet = struct
  include IntSet'
  let add_list = List.fold_left (swap_arg IntSet'.add)
  let print s = IntSet'.iter (Printf.printf "%d ") s; Printf.printf "\n"
end

module List = struct
  include List

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

  (** remove cond xs removes the first element satisfies cond from xs.*)
  let remove cond =
    let rec remove_aux accum = function
	[] -> List.rev accum (* unfound *)
      | hd :: tl -> if cond hd then List.rev accum @ tl else remove_aux (hd :: accum) tl
    in
    remove_aux []

  let find_max f xs =
    if List.length xs = 0 then raise (Invalid_argument "") else
      List.fold_left (fun x y -> if f x > f y then x else y) (List.hd xs) (List.tl xs)

  let find_max_val f xs =
    if List.length xs = 0 then raise (Invalid_argument "") else
      List.fold_left (fun x y -> if x > f y then x else f y) (f (List.hd xs)) (List.tl xs)

  let find_min f xs =
    if List.length xs = 0 then raise (Invalid_argument "") else
      List.fold_left (fun x y -> if f x < f y then x else y) (List.hd xs) (List.tl xs)

  let find_min_val f xs =
    if List.length xs = 0 then raise (Invalid_argument "") else
      List.fold_left (fun x y -> if x < f y then x else f y) (f (List.hd xs)) (List.tl xs)

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

end

module Array = struct
  include Array

  (** Print elements in an array. Function which converts an element to some string representation is needed. *)
  let print to_string xs =
    Array.iter (fun x -> Printf.printf "%s " (to_string x)) xs;
    Printf.printf "\n"

  let mapi f = function
      [||] -> [||]
    | _ as xs ->
	let len = Array.length xs in
	let xs2 = Array.make len (f 0 xs.(0)) in
	for i = 0 to len - 1 do
	  xs2.(i) <- f i xs.(i)
	done;
	xs2

  (** Swap two elements at the designated locations. destructive. *)
  let swap i j xs =
    let elm_j = xs.(j) in
    xs.(j) <- xs.(i);
    xs.(i) <- elm_j

  (** For loop escapement. *)
  exception Found of int option

  (** Scan an array from left and return the index of the first element which satisfies cond. *)
  let find_num_left ?(start = 0) cond xs =
    match
      try
	for i = start to Array.length xs - 1 do
	  if cond xs.(i) then raise (Found (Some i))
	done;
	None
      with
	Found x -> x
    with
      Some x -> x
    | None -> raise Not_found

  (** Same to find_num_left, but scanning starts from right. *)
  let find_num_right ?(start = 0) cond xs =
    let len = Array.length xs in
    match
      try
	for i = start + 1 to len do
	  if cond xs.(len - i) then raise (Found (Some i))
	done;
	None
      with
	Found x -> x
    with
      Some x -> len - x
    | None -> raise Not_found

  (** Split array at just left the nth element. *)
  let split n xs = (Array.sub xs 0 n, Array.sub xs n (Array.length xs - n))

  (** Same to that of List *)
  let for_all cond xs = Array.fold_left (fun p x -> p && cond x) true xs

  (** Same to that of List *)
  let exists cond xs = Array.fold_left (fun p x -> p || cond x) false xs

  let permutate order xs =
    let n = Array.length xs in
    if List.length order <> n then raise (Invalid_argument "Array length must be the same.");
    let a = Array.make n xs.(0) in
    let rec permutate_aux m = function
	[] -> a
      | hd :: tl -> a.(m) <- xs.(hd); permutate_aux (m + 1) tl
    in
    permutate_aux 0 order

end


(**
    reference : Okasaki, C. (1995) Simple and Efficient Purely Functional Queues and Deques. J. Functional Programming, 5(4), 583–592.
 *)
module Queue2 : sig
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val length : 'a t -> int
  val singleton : 'a -> 'a t
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> 'a * 'a t
  val peek : 'a t -> 'a
end = struct
  type 'a t = 'a list * 'a list
  let empty = ([], [])
  let is_empty (l, r) = List.is_empty l && List.is_empty r
  let length (l, r) = List.length l + List.length r
  let singleton e = ([], [e])
  let push e (l, r) = (l, e :: r)
  let rec pop (l, r) = match l with
      [] -> pop (List.rev r, [])
    | hd :: tl -> (hd, (tl, r))
  let rec peek (l, r) = match l with
      [] -> peek (List.rev r, [])
    | hd :: tl -> hd
end

module Stack2 : sig
  type 'a t
  val empty : 'a t
  val singleton : 'a -> 'a t
  val is_empty : 'a t -> bool
  val length : 'a t -> int
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> 'a * 'a t
  val peek : 'a t -> 'a
end = struct
  type 'a t = 'a list
  exception Empty
  let empty = []
  let singleton x = [x]
  let is_empty = function [] -> true | _ -> false
  let length = List.length
  let push x q = x :: q
  let pop = function
      [] -> raise Empty
    | hd :: tl -> (hd, tl)
  let peek = function
      [] -> raise Empty
    | hd :: tl -> hd
end

(* reference : http://d.hatena.ne.jp/blanketsky/20070221/1172002969 *)
module LazyList(*  : sig
  type 'a t
  val from : int -> int t
  val head : 'a t -> 'a
  val tail : 'a t -> 'a t
  val take : int -> 'a t -> 'a list
  val map  : ('a -> 'b) -> 'a t -> 'b t
  val nth  : int -> 'a t -> 'a
  val primes : int t
end *)
  =
struct
  type 'a t = Cons of 'a * ('a t lazy_t)

  let rec from n = Cons (n, lazy (from (n+1)))

  let head (Cons (x, _)) = x
  let tail (Cons (_, xs)) = Lazy.force xs

  let take n s =
   let rec take' m (Cons (x, xs)) l =
     if m = 0 then List.rev l
     else take' (m-1) (Lazy.force xs) (x :: l)
   in
     take' n s []

  let rec map f (Cons (x, xs)) =
    Cons (f x, lazy (map f (Lazy.force xs)))

  let rec nth n (Cons (x, xs)) =
    if n = 1 then x
    else nth (n-1) (Lazy.force xs)

  (* remove multiples of n *)
  let rec sift n (Cons (x, xs)) =
    if x mod n <> 0 then Cons (x, lazy (sift n (Lazy.force xs)))
    else sift n (Lazy.force xs)

  let rec sieve (Cons (x, xs)) =
    Cons (x, lazy (sieve (sift x (Lazy.force xs))))

  let primes = sieve (from 2)
end
