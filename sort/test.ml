open Util
open Unix

let size = ref 1

type order = Sorted | Almost_sorted | Random | Reverse
let string_to_order = function
    "sorted" -> Sorted
  | "almost" -> Almost_sorted
  | "random" -> Random
  | "reverse" -> Reverse
  | _ -> failwith "Invalid type of order"
let order = ref Random
let reorder order xs =
  let n = Array.length xs in match order with
      Sorted -> xs
    | Almost_sorted ->
	let rec perturb accum xs =
	  let hd = List.hd xs and tl = List.tl xs in
	  if List.length xs <= 4 then List.rev accum @ xs
	  else if Random.int 4 = 0 then
	    let modify = List.map (fun x -> if hd < x && x <= hd + 4 then x - 1 else x) in
	    perturb (hd + 4 :: modify accum) (modify tl)
	  else
	    perturb (hd :: accum) tl
	in
	Array.permutate (List.range 0 (n - 1) 1 |> perturb []) xs
    | Random -> Array.permutate (List.random_permutation n) xs
    | Reverse -> Array.permutate (List.range (n - 1) 0 (-1)) xs


type element = Integer | List
let string_to_element = function
    "int" -> Integer
  | "list" -> List
  | _ -> failwith "Invalid type of elements"
let element = ref Integer

type sorter = Quicksort
let string_to_sorter = function
    "quicksort" -> Quicksort
  | _ -> failwith "Invalid name of sort function"
let sorter = ref Quicksort

let speclist = [
  ("-s", Arg.Set_int size, "");
  ("-t", Arg.String (fun s -> order := string_to_order s), "");
  ("-e", Arg.String (fun s -> element := string_to_element s), "");
  ("-a", Arg.String (fun s -> sorter := string_to_sorter s), "");
];;

let _ =
  Random.self_init ();
  Arg.parse speclist (fun s -> ()) "";
  let f () = match !element with
      Integer ->
	let elm = identity
	and cmp =  Pervasives.compare
	and sort = match !sorter with
	    Quicksort -> Quicksort.quicksort
	in
	sort cmp (reorder !order (Array.init !size elm)); ()
    | List ->
	let elm n = List.make 9 0 @ [n]
	and cmp xs ys = Pervasives.compare (List.last xs) (List.last ys)
	and sort = match !sorter with
	    Quicksort -> Quicksort.quicksort
	in
	sort cmp (reorder !order (Array.init !size elm)); ()
  in
  Util.time Unix.gettimeofday 1 f () |> Printf.printf "%e\n"
