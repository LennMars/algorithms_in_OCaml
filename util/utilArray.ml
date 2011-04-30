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
