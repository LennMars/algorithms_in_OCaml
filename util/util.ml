let identity x = x

let (|>) f x = x f

let (<|) f x = f x

let swap (a, b) = (b, a)

let swap_arg f x y = f y x

let maxf f x y = max (f x) (f y)

let xor x y = x && (not y) || y && (not x)

let rec iter f n =
  if n <= 0 then ()
  else begin
    f ();
    iter f (n - 1)
  end;;

let int_exp x y = (*does not have overflow catcher yet*)
  if y < 0 then failwith "y must be positive";
    let is_even_list =
      let rec is_even_list_ y accum =
	if y = 0 then accum
	else if (y mod 2 = 0) then is_even_list_ (y / 2) (true::accum)
	else is_even_list_ ((y - 1) / 2) (false::accum)
      in
      is_even_list_ y []
    in
    let  rec int_exp_aux x is_even_list accum = match is_even_list with
	[] -> accum
      | hd :: tl -> int_exp_aux x tl (accum * accum * if hd then 1 else x)
    in
    int_exp_aux x is_even_list 1

module Int = struct type t = int let compare = compare end
module IntSet' = Set.Make (Int)
module IntSet = struct
  include IntSet'
  let add_list = List.fold_left (swap_arg IntSet'.add)
end


module List = struct
  include List

  let is_empty xs = if List.length xs = 0 then true else false

  let single xs = if List.length xs = 1 then List.hd xs else
    raise (Invalid_argument "The length of the list must be 1.")

  let filter_some xs = List.filter (fun x -> match x with Some _ -> true | None -> false) xs
    |> List.map (fun x -> match x with Some y -> y | None -> failwith "fatal error")

  let rec print_int_list = function
      [] -> Printf.printf "\n"
    | hd :: tl -> Printf.printf "%d " hd; print_int_list tl

  let rec print_float_list = function
      [] -> Printf.printf "\n"
    | hd :: tl -> Printf.printf "%d " hd; print_float_list tl

  (** only first *)
  let remove cond =
    let rec remove_aux accum = function
	[] -> List.rev accum (* unfound *)
      | hd :: tl -> if cond hd then List.rev accum @ tl else remove_aux (hd :: accum) tl
    in
    remove_aux []

  let find_max f xs =
    if List.length xs = 0 then raise (Invalid_argument "") else
      List.fold_left (fun x y -> if f x > f y then x else y) (List.hd xs) (List.tl xs)

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

  let init_list f n =
    let rec init_list_aux n accum =
      if n <= 0 then accum else
	init_list_aux (n - 1) (f (n - 1) :: accum)
    in
    init_list_aux n []

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

  let range a b =
    let rec range_aux a b accum =
      if a > b then accum
      else range_aux a (b - 1) (b :: accum)
    in
    range_aux a b [] (* ex. range 1 3 = [1; 2; 3] *)

  let take_ns ns lst =
    let rec take_ns_aux ns lst l accum =
      if lst == [] || ns == [] then List.rev accum else
	if l == List.hd ns then
	  take_ns_aux (List.tl ns) (List.tl lst) (l + 1) (List.hd lst :: accum)
	else
	  take_ns_aux ns (List.tl lst) (l + 1) accum
    in
    take_ns_aux ns lst 0 [] (* preserves order *)
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
end
