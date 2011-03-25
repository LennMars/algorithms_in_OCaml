open Util

module type Ring = sig
  type t
  val zero : t
  val one : t
  val ( +^ ) : t -> t -> t
  val ( *^ ) : t -> t -> t
  val ( =^ ) : t -> t -> bool
end

module Make (R : Ring) : sig
  type elt = R.t
  type t = elt array
  val dft : elt -> t -> t
end =
struct
  type elt = R.t
  type t = elt array

  let ( +^ ) = R.( +^ )
  let ( *^ ) = R.( *^ )
  let ( =^ ) = R.( =^ )

  let bitrev m i =
    let rec bitrev_aux accum i = function
	0 -> accum
      | j -> bitrev_aux (1 land i :: accum) (i lsr 1) (j - 1)
    and to_int accum d = function
	[] -> accum
      | hd :: tl -> to_int (hd * d + accum) (d * 2) tl
    in
    bitrev_aux [] i m |> to_int 0 1

  let dft w xs =
    let n = Array.length xs in
    let m = minimum_bigger_power_of_two n in
    let n' = int_exp 2 m in
    let ws = Array.init n (general_exp R.one ( *^ ) w)
    and pair step i =
      let span = int_exp 2 step in
      let is_even = i / span mod 2 = 0 in
      (is_even, i + if is_even then span else (-span))
    in
    let rec dft_aux xs step =
	if step = m then xs
	else
	  let butterfly i x =
	    let (is_even, j) = pair step i
	    and pow = int_exp 2 (m - step - 1) * i mod n in
	    if is_even then x +^ xs.(j) *^ ws.(pow)
	    else xs.(j) +^ x *^ ws.(pow)
	  in
	  dft_aux (Array.mapi butterfly xs) (step + 1)
    in
    dft_aux (Array.init n' (fun i -> try xs.(bitrev m i) with Invalid_argument _ -> R.zero)) 0
end
