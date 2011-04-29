open Util
module P = PolynomialRing.Make (RationalBig)
open RationalBig
open P

let f = [one, [|2; 1|]; one, [|1; 2|]; one, [|0; 2|]] (* x^2y + xy^2 + y^2 *)
let negone = r (-1, 1)
let g1 = [one, [|0; 2|]; negone, [|0; 0|]] (* y^2 - 1 *)
let g2 = [one, [|1; 1|]; negone, [|0; 0|]] (* xy - 1 *)

let remove_abstr f = List.map (fun (a, x) -> float_of_rational a, x) f

let divide lex gs f =
  let (qs, r) = divide (lex [0;1]) gs f in
  (qs, r)

let sort f = List.sort (fun (_,x) (_,y)-> lex [0;1] x y) f

let mo = lex [0;1];;

Random.self_init ();;

let rand_rational n = RationalBig.rational_of_int (Random.int n + 1)
let num_var = 3
let max_divisor_dim = 2
let max_dividend_dim = 5
let max_num_term = 4
let max_coef = 10
let num_divisors = 3
let rand_poly max_dim : P.poly =
  let term () =
    (rand_rational max_coef, Array.init num_var (fun i -> Random.int max_dim))
  in
  let rec aux n accum =
    if n <= 0 then accum
    else aux (n - 1) (term () +: accum)
  in
  aux (Random.int max_num_term + 1) [] |> clean
let divisors () = List.init (fun i -> rand_poly max_divisor_dim) num_divisors
let dividend () = rand_poly max_dividend_dim
let test () =
  let s = to_string in let m = List.map to_string in
  let f = List.fold_left (fun s t -> s^"\n"^t) "" in
  let divisors = divisors ()
  and dividend = dividend () in
  let (qs, r) = divide lex divisors dividend in
  Printf.printf "divisors : %s\ndividend : %s\n" (m divisors |> f) (s dividend);
  Printf.printf "qs : %s\nr : %s\n" (m qs |> f) (s r) (* List.fold_left ( +~ ) [] (List.map2 ( *~ ) qs divisors) +~ r *);
  (List.map2 ( *~ ) qs divisors |> List.fold_left (+~) []) +~ r =~ dividend

let rec iter n =
  if n <= 0 then ()
  else begin
    if test () then iter (n - 1)
    else failwith "false"
  end

let _ = iter (int_of_string Sys.argv.(1))

