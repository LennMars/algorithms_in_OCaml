open Util
open Unix

type order = Sorted | Almost_sorted | Random | Reverse
type element = Integer | List
type sorter = Quicksort | Insertsort

let size = ref 1
let order = ref Random
let element = ref Integer
let sorter = ref Quicksort

let string_to_order = function
    "sorted" -> Sorted
  | "almost" -> Almost_sorted
  | "random" -> Random
  | "reverse" -> Reverse
  | _ -> failwith "Invalid type of order"
let string_to_element = function
    "int" -> Integer
  | "list" -> List
  | _ -> failwith "Invalid type of elements"
let string_to_sorter = function
    "quick" -> Quicksort
  | "insert" -> Insertsort
  | _ -> failwith "Invalid name of sort function"

let speclist = [
  ("-s", Arg.Set_int size, "");
  ("-t", Arg.String (fun s -> order := string_to_order s), "");
  ("-e", Arg.String (fun s -> element := string_to_element s), "");
  ("-a", Arg.String (fun s -> sorter := string_to_sorter s), "");
];;

let select_order order xs =
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

let select_sort = function
    Quicksort -> Main.quicksort
  | Insertsort -> Main.insertsort

let check sorter order comp m =
  let a = select_order order (Array.init m identity) in
  let b = Array.copy a in
  Array.sort comp a = sorter comp b

let _ =
  Random.self_init ();
  Arg.parse speclist (fun s -> ()) "";
  let f () = match !element with
      Integer ->
	let elm n = n
	and cmp =  Pervasives.compare in
	(select_sort !sorter) cmp (select_order !order (Array.init !size elm)); ()
    | List ->
	let elm n = List.make 9 0 @ [n]
	and cmp xs ys = Pervasives.compare (List.last xs) (List.last ys) in
	(select_sort !sorter) cmp (select_order !order (Array.init !size elm)); ()
  in
  Util.time Unix.gettimeofday 1 f () |> Printf.printf "%e\n"
