open Util
open Main

(* symbol *)
module String = struct
  type t = V of string | P of t * t | M of t * t | E of t * int
  let zero = V "0"
  let one = V "1"
  let ( +^ ) x y = match (x, y) with
      (V "0", x) | (x, V "0") -> x
    | _ -> P (x, y)
  let ( *^ ) x y = match (x, y) with
      (V "0", x) | (x, V "0") -> zero
    | (V "1", y) -> y
    | (x, V "1") -> x
    | (V a, V b) when a = b -> E (V a, 2)
    | (E (V a, n), E (V b, m)) when a = b -> E (V a, m + n)
    | (E (V a, n), V b) | (V b, E (V a, n)) when a = b -> E (V a, n + 1)
    | (_, _) -> M (x, y)
  let ( =^ ) = (=)
  let rec to_string = function
      V s -> s
    | P (a, b) -> "(" ^ to_string a ^ " + " ^ to_string b ^ ")"
    | M (a, b) -> to_string a ^ " * " ^ to_string b
    | E (a, n) -> to_string a ^ "^" ^ string_of_int n
end

module DFT = Make (String)

open String

let x = DFT.dft (V "x") [|V "a";V "b";V "c";V "d"|] |> Array.map to_string
let y = DFT.dft (V "y") [|V "a";V "b";V "c";V "d";V "e";V "f";V "g"|] |> Array.map to_string
let z = DFT.dft (V "z") [|V "a";V "b";V "c";V "d";V "e";V "f";V "g";V "h"|] |> Array.map to_string


(* complex number *)
module Complex = struct
  type t = float * float
  let zero = (0., 0.)
  let one = (1., 0.)
  let ( +^ ) (xr, xi) (yr, yi) = (xr +. yr, xi +. yi)
  let ( *^ ) (xr, xi) (yr, yi) = (xr *. yr -. xi *. yi, xr *. yi +. yr *. xi)
  let ( =^ ) x y = x = y
end

module DFT2 = Make (Complex)

open Complex

let pi = atan 1. *. 4.
let rect n m = Array.init (2 * n) (fun i -> if n - m <= i && i < n + m then one else zero)
let kernel n =
  let theta = -1. *. pi /. float n in
  (cos theta, sin theta)
let sinc n m = DFT2.dft (kernel n) (rect n m)
