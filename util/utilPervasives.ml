external identity : 'a -> 'a = "%identity"

let (|>) f x = x f

let (<|) f x = f x

let ($) f g x = f (g x)

let swap (a, b) = (b, a)

let swap_arg f x y = f y x

let maxf f x y = max (f x) (f y)

let xor x y = x && (not y) || y && (not x)

let incr x = x := !x + 1

let triple1 (x, _, _) = x
let triple2 (_, x, _) = x
let triple3 (_, _, x) = x

let quad1 (x, _, _, _) = x
let quad2 (_, x, _, _) = x
let quad3 (_, _, x, _) = x
let quad4 (_, _, _, x) = x

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

let rec bisection f a b =
  let (fa, fb, c) = f a, f b, (a +. b) /. 2.
  and is_same_sign x y =
      let sign x = if x > 0. then true else false in
      (sign x && sign y) || (not (sign x) && not (sign y))
  in
  if a > b || classify_float a = FP_infinite || classify_float b = FP_infinite then
    raise (Invalid_argument "bisection")
  else if is_same_sign fa fb then bisection f (2. *. a -. c) (2. *. b -. c)
  else if abs_float (f c) < 10e-9 then c
  else if is_same_sign fa (f c) then bisection f c b
  else bisection f a c

let is_even_list x =
  if x < 0 then invalid_arg "is_even_list";
  let rec aux x accum =
    if x = 0 then accum
    else if (x mod 2 = 0) then aux (x lsr 1) (true::accum)
    else aux (x lsr 1) (false::accum)
  in
  aux x []

let general_exp one mul x y =
  let rec aux accum = function
      [] -> accum
    | hd :: tl -> aux (mul (mul accum accum) (if hd then one else x)) tl
  in
  aux one (is_even_list y)

let int_exp = general_exp 1 ( * )

let minimum_bigger_power_of_two n =
  if n = 0 then -1 else
    let rec aux accum = function
        0 -> accum
      | m -> aux (accum + 1) (m lsr 1)
    in
    aux 0 (n-1)

let string_of_option to_string = function
    None -> "None"
  | Some x -> to_string x

let box_muller () =
  let rand () = 1. -. Random.float 1. in
  let (a, b) = rand (),  rand () in
  sqrt (-2. *. log a) *. sin (2. *. pi *. b),
  sqrt (-2. *. log b) *. sin (2. *. pi *. a)

module Int = struct type t = int let compare = compare end
module IntSet' = Set.Make (Int)
module IntSet = struct
  include IntSet'
  let add_list = List.fold_left (swap_arg IntSet'.add)
  let print s = IntSet'.iter (Printf.printf "%d ") s; Printf.printf "\n"
end

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
  let is_empty (l, r) = (l = []) && (r = [])
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
  val remove : 'a t -> 'a t
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
  let remove = function
      [] -> raise Empty
    | hd :: tl -> tl
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

