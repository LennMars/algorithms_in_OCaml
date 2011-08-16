open Util

let tripartition compare pivot xs =
  let len = Array.length xs in
  let rec aux l m r n is_l_turn =
    if l > r then (* finished *)
      (
	Array.sub xs m (r - m + 1),
	Array.append (Array.sub xs 0 m) (Array.sub xs (n + 1) (len - n - 1)),
	Array.sub xs l (n - r)
      )
     else if is_l_turn then (* step from left side *)
      let c = compare pivot xs.(l) in
      if c = 0 then
	(Array.swap l m xs; aux (l + 1) (m + 1) r n false)
      else if c < 0 then
	if compare pivot xs.(r) > 0 then (Array.swap l r xs; aux (l + 1) m (r - 1) n false)
	else aux l m r n false
      else
	aux (l + 1) m r n false
     else (* step from right side *)
       let c = compare pivot xs.(r) in
      if c = 0 then
	(Array.swap r n xs; aux l m (r - 1) (n - 1) true)
      else if c > 0 then
	if compare pivot xs.(l) < 0 then (Array.swap r l xs; aux (l + 1) m (r - 1) n true)
	else aux l m r n true
      else
	aux l m (r - 1) n true
  in
  aux 0 0 (len - 1) (len - 1) true;;

type sign = Pos | Zero | Neg
let sign i = if i > 0 then Pos else if i = 0 then Zero else Neg

let rec ternary_sort cmp xs =
  let len = Array.length xs in
  match len with
    0 -> []
  | 1 -> [xs]
  | 2 -> (
      let a, b = xs.(0), xs.(1) in match cmp a b |> sign with
	  Pos -> [[|b|];[|a|]]
	| Zero -> [[|a;b|]]
	| Neg -> [[|a|];[|b|]]
    )
  | 3 -> (
      let a, b, c = xs.(0), xs.(1), xs.(2) in
      match sign (cmp a b) with
	Pos -> begin match sign (cmp b c) with
	  Pos -> [[|c|];[|b|];[|a|]]
	| Zero -> [[|b;c|];[|a|]]
	| Neg -> begin match sign (cmp a c) with
	  | Pos -> [[|b|];[|c|];[|a|]]
	  | Zero -> [[|b|];[|a;c|]]
	  | Neg -> [[|b|];[|a|];[|c|]] end end
      | Zero -> begin match sign (cmp b c) with
	| Pos -> [[|c|];[|a;b|]]
	| Zero -> [[|a;b;c|]]
	| Neg -> [[|a;b|];[|c|]] end
      | Neg -> begin match sign (cmp b c) with
	  Pos -> begin match sign (cmp a c) with
	  | Pos -> [[|c|];[|a|];[|b|]]
	  | Zero -> [[|a;c|];[|b|]]
	  | Neg -> [[|a|];[|c|];[|b|]] end
	| Zero -> [[|a|];[|b;c|]]
	| Neg -> [[|a|];[|b|];[|c|]] end
    )
  | _ ->
      let select_pivot xs = (* len supposed to be larger than 3 *)
	let a = xs.(len / 4)
	and b = xs.(len / 2)
	and c = xs.(len - len / 4)
	in
	if cmp a b >= 0 then
	  if cmp c a >= 0 then a
	  else if cmp b c >= 0 then b
	  else c
	else
	  if cmp c b >= 0 then b
	  else if cmp a c >= 0 then a
	  else c
      in
      let l, c, r = tripartition cmp (select_pivot xs) xs in
      ternary_sort cmp l @ [c] @ ternary_sort cmp r

let sort_0 str = (* initialization. make 1-ordered suffix array *)
  let str = str ^ "\n" in
  let length = String.length str in
  let compare i j = compare str.[i] str.[j]
  and suffix = Array.init length identity
  and group = Array.make length (length - 1)
  and skip = Array.make length 0
  in
  (* make init suffix *)
  Array.stable_sort compare suffix;
  (* make init group *)
  for i = length - 2 downto 0 do
    if compare suffix.(i) suffix.(i + 1) = 0 then
      group.(suffix.(i)) <- group.(suffix.(i + 1))
    else
      group.(suffix.(i)) <- i
  done;
  (* make init skip *)
  let cont = ref 0 in
  for i = length - 1 downto 1 do
    if group.(suffix.(i)) != group.(suffix.(i - 1)) then begin
      if !cont = 0 then skip.(suffix.(i)) <- -1
      else skip.(suffix.(i)) <- !cont + 1;
      cont := 0 end
    else
      incr cont
  done;
  skip.(suffix.(0)) <- -1;
  suffix, group, skip;;

let sort_h h start len (suffix, group, skip) = (* make 2h-ordered suffix array using that is h-ordered *)
  let cmp i j = compare group.(i + h) group.(j + h) in
  let sorted = ternary_sort cmp (Array.sub suffix start len) in
  let rec apply n sorted = match sorted with
      [] ->
	()
    | hd :: tl ->
	let l = Array.length hd in
	Array.blit hd 0 suffix (start + n) l; (* update suffix *)
	Array.iter (fun i -> group.(i) <- group.(i) - len + n + l) hd; (* update group *)
	let _ = match hd with (* update skip *)
	    [||] -> failwith "sort_h"
	  | [|i|] -> skip.(i) <- -1
	  | _ -> skip.(hd.(0)) <- Array.length hd
	in
	apply (n + l) tl
  in
  apply 0 sorted;;

let sort (suffix, group, skip) = (* sort recursively starting from 1-ordered suffix array *)
  let len = Array.length suffix in
  let rec aux_out h = (* loop on h *)
    if h > len then (* finished *)
      suffix
    else
      let rec aux_in n = (* loop in some h *)
	if n >= len then ()
	else
	  let s = skip.(suffix.(n)) in match sign s with
	      Pos -> sort_h h n s (suffix, group, skip); aux_in (n + s)
	    | Zero -> failwith "sort"
	    | Neg -> aux_in (n - s)
      in
      aux_in 0;
      aux_out (h * 2)
  in
  aux_out 1;;

let make_suffix_array str = sort (sort_0 str);;






