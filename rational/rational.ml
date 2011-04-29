open Util

type t = int * int (* p /. q. q must be positive. *)

let gcd m n =
  let abs k = if k >= 0 then k else -k in
  let rec aux m n =
    if n = 0 then m
    else aux n (m mod n)
  in
  aux (abs m) (abs n)

let normalize (p, q) =
  let (p, q) = if q < 0 then (-p, -q) else if q > 0 then (p, q) else raise Division_by_zero in
  let d = gcd p q in
  (p / d, q / d)

let r = normalize

let ( +^ ) (ap, aq) (bp, bq) = normalize (ap * bq + bp * aq, aq * bq)
let ( -^ ) (ap, aq) (bp, bq) = normalize (ap * bq - bp * aq, aq * bq)
let ( *^ ) (ap, aq) (bp, bq) = normalize (ap * bp, aq * bq)
let ( /^ ) (ap, aq) (bp, bq) = normalize (ap * bq, bp * aq)
let ( >^ ) (ap, aq) (bp, bq) = ap * bq > bp * aq
let ( >=^ ) a b = not (b >^ a)
let ( <^ ) a b = b >^ a
let ( <=^ ) a b = not (a >^ b)
let ( =^ ) a b = a >=^ b && a <=^ b
let ( <>^ ) a b = a <^ b || a >^ b
let one = (1, 1)
let zero = (0, 1)
let negative (p, q) = (-p, q)
let inverse (p, q) = if p < 0 then (-q, -p) else (q, p)
let float_of_rational (p, q) =
  if q = 0 then failwith "Division_by_zero at rational"
  else float p /. float q
let rational_of_int i = (i, 1)
let to_string r = float_of_rational r |> string_of_float
