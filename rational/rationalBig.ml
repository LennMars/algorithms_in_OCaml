open Util
open Big_int

type t = big_int * big_int (* p /. q. q must be positive. *)

let is_zero i = sign_big_int i = 0

let gcd m n =
  let rec aux m n =
    if is_zero n then m
    else aux n (mod_big_int m n)
  in
  aux (abs_big_int m) (abs_big_int n)

let normalize (p, q) =
  if is_zero q && not (is_zero p) then failwith "aaa"
  else if is_zero q then failwith "bbb";
  let (p, q) =
    if sign_big_int q < 0 then
      (minus_big_int p, minus_big_int q)
    else if sign_big_int q > 0 then
      (p, q)
    else raise Division_by_zero
  in
  let d = gcd p q in
  (div_big_int p d, div_big_int q d)

let r (p, q) = normalize (big_int_of_int p, big_int_of_int q)

let ( + ) = add_big_int
let ( * ) = mult_big_int
let ( - ) = sub_big_int
let ( +^ ) (ap, aq) (bp, bq) =
  if is_zero aq || is_zero bq then failwith "ccc"
  else if is_zero (aq * bq)  then failwith "ddd";
  normalize (ap * bq + bp * aq, aq * bq)
let ( -^ ) (ap, aq) (bp, bq) = normalize (ap * bq - bp * aq, aq * bq)
let ( *^ ) (ap, aq) (bp, bq) = normalize (ap * bp, aq * bq)
let ( /^ ) (ap, aq) (bp, bq) = normalize (ap * bq, bp * aq)
let ( >^ ) (ap, aq) (bp, bq) = gt_big_int (ap * bq) (bp * aq)
let ( >=^ ) a b = not (b >^ a)
let ( <^ ) a b = b >^ a
let ( <=^ ) a b = not (a >^ b)
let ( =^ ) a b = a >=^ b && a <=^ b
let ( <>^ ) a b = a <^ b || a >^ b
let one = (big_int_of_int 1, big_int_of_int 1)
let zero = (big_int_of_int 0, big_int_of_int 1)
let negative (p, q) = (minus_big_int p, q)
let inverse (p, q) = if sign_big_int p < 0 then (minus_big_int q, minus_big_int p) else (q, p)
let float_of_rational (p, q) =
  if is_zero q then failwith "Division_by_zero at rational"
  else float_of_big_int p /. float_of_big_int q
let rational_of_int i = (big_int_of_int i, big_int_of_int 1)
let to_string (p, q) =
  let (sp, sq) = string_of_big_int p, string_of_big_int q in
  if eq_big_int q unit_big_int then sp
  else "(" ^ sp ^ " / " ^ sq  ^ ")"
