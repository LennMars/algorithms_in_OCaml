open Util

module type Field = sig
  type t
  val ( +^ ) : t -> t -> t
  val ( -^ ) : t -> t -> t
  val ( *^ ) : t -> t -> t
  val ( /^ ) : t -> t -> t
  val ( =^ ) : t -> t -> bool
  val zero : t
  val one : t
  val negative : t -> t
  val inverse : t -> t
end

module Make (F : Field) = struct
  type monomial = int list
  type term = F.t * monomial
  type order = monomial -> monomial -> int
  type t = term list
  let ( +^ ) = F.( +^ )
  let ( -^ ) = F.( -^ )
  let ( /^ ) = F.( /^ )
  let ( *^ ) = F.( *^ )
  let ( =^ ) = F.( =^ )
  let degree x = List.fold_left (+) 0 x
  let total_degree f =
    let deg (a, x) = if a =^ F.zero then 0 else degree x in
    List.find_max_val deg f
  let lex x y = compare (List.hd x) (List.hd y)
  let glex x y = compare (degree x) (degree y) * 2 + lex x y
  let grevlex x y = compare (degree x) (degree y) * 2 - compare (List.last x) (List.last y)

end
