open UtilPervasives

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

let map2 f xs ys =
  let n = Array.length xs in
  if n <> Array.length ys then raise (Invalid_argument "map2")
  else Array.init n (fun i -> f xs.(i) ys.(i))

let swap i j xs =
  let elm_j = xs.(j) in
  xs.(j) <- xs.(i);
  xs.(i) <- elm_j

let rec swap_region s1 len1 s2 len2 xs =
  if s1 > s2 then
    swap_region s2 len2 s1 len1 xs
  else if s1 < 0 || s2 + len2 > Array.length xs || s1 + len1 > s2 then
    invalid_arg "swap_region"
  else
    let left = Array.sub xs s1 len1
    and center = Array.sub xs (s1 + len1) (s2 - s1 - len1)
    in
    Array.blit xs s2 xs s1 len2;
    Array.blit center 0 xs (s1 + len2) (s2 - s1 - len1);
    Array.blit left 0 xs (s2 - len1 + len2) len1

let find_max ?(comp = Pervasives.compare) f xs =
  if Array.length xs = 0 then
    raise (Invalid_argument "find_max")
  else
    Array.fold_left (fun x y -> if comp (f x) (f y) > 0 then x else y) xs.(0) xs

let find_min ?(comp = Pervasives.compare) =
  find_max ~comp:(swap_arg comp)

let find_max_num ?(comp = Pervasives.compare) f xs =
  if Array.length xs = 0 then
    raise (Invalid_argument "find_max_num")
  else
    let max_i = ref 0
    and max = ref xs.(0)
    and fmax = ref (f xs.(0)) in
    let test i x =
      let fx = f x in
      if comp fx !fmax > 0 then begin
        max_i := i;
        max := x;
        fmax := fx end
      else
        () in
    Array.iteri test xs;
    !max_i

let find_min_num ?(comp = Pervasives.compare) = find_max_num ~comp:(swap_arg comp)


(* For loop escapement. *)
exception Found of int option

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

let split n xs = (Array.sub xs 0 n, Array.sub xs n (Array.length xs - n))

let for_all cond xs = Array.fold_left (fun p x -> p && cond x) true xs

let for_all2 cond xs ys =
  let n = Array.length xs in
  if n <> Array.length ys then raise (Invalid_argument "for_all2")
  else
    let p = ref true in
    let _ =
      for i = 0 to n - 1 do
	if not (cond xs.(i) ys.(i)) then p := false
      done
    in
    !p

(* let exists cond xs = Array.fold_left (fun p x -> p || cond x) false xs *)

exception Found

let exists cond xs =
  try
    for i = 0 to Array.length xs - 1 do
      if cond xs.(i) then raise Found
    done;
    false
  with Found -> true

let permutate order xs =
  let n = Array.length xs in
  if List.length order <> n then raise (Invalid_argument "Array length must be the same.");
  let a = Array.make n xs.(0) in
  let rec permutate_aux m = function
      [] -> a
    | hd :: tl -> a.(m) <- xs.(hd); permutate_aux (m + 1) tl
  in
  permutate_aux 0 order

let count p xs = Array.fold_left (fun n x -> if p x then n + 1 else n) 0 xs

let filter_some xs =
  Array.to_list xs |> UtilList.filter_some |> Array.of_list

let select_rand n xs =
  let m = Array.length xs - n in
  if n < 0 || m < 0 then raise (Invalid_argument "select_rand");
  let xs = Array.map (fun x -> Some x) xs in
  let rec aux k =
    if k >= m then
      filter_some xs
    else
      let to_delete = Random.int (Array.length xs - k) in
      let rec aux2 i j =
	match xs.(j) with
	  Some _ -> if i = to_delete then xs.(j) <- None else aux2 (i + 1) (j + 1)
	| None -> aux2 i (j + 1)
      in
      aux2 0 0;
      aux (k + 1)
  in
  aux 0
