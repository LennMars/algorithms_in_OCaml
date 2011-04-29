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
  val to_string : t -> string
end

module Make (F : Field) : sig
  type mono = int array
  type term = F.t * mono
  type var_order = int list
  type mono_order = mono -> mono -> int
  type poly = term list
  val clean : poly -> poly
  val degree : mono -> int
  val total_degree : poly -> int
  val lterm : mono_order -> poly -> term
  val is_divider : mono -> mono -> bool
  val is_lterm_divider : mono_order -> poly -> poly -> bool
  val lex : var_order -> mono_order
  val grlex : var_order -> mono_order
  val grevlex : var_order -> mono_order
  val order_auto : (var_order -> mono_order) -> mono -> mono -> int
  val ( +: ) : term -> poly -> poly
  val ( +~ ) : poly -> poly -> poly
  val ( -~ ) : poly -> poly -> poly
  val ( *~ ) : poly -> poly -> poly
  val divide : mono_order -> poly list -> poly -> poly list * poly
  val ( =~ ) : poly -> poly -> bool
  val mult_const_poly : poly -> F.t -> poly
  val divide_term : term -> term -> term
  val to_string : poly -> string
end = struct
  type mono = int array
  type term = F.t * mono
  type var_order = int list
  type mono_order = mono -> mono -> int
  type poly = term list
  let ( +^ ) = F.( +^ )
  let ( -^ ) = F.( -^ )
  let ( /^ ) = F.( /^ )
  let ( *^ ) = F.( *^ )
  let ( =^ ) = F.( =^ )
  let degree x = Array.fold_left (+) 0 x
  let total_degree f =
    let deg (a, x) = if a =^ F.zero then 0 else degree x in
    List.find_max_val deg f
  let lterm mo f = List.find_min_with mo snd f
  let is_divider x y = Array.for_all2 (<=) x y
  let is_lterm_divider mo g f = is_divider (lterm mo g |> snd) (lterm mo f |> snd)
  let rec lex vo x y =
    match vo with
      [] -> 0
    | hd :: tl ->
	let diff = x.(hd) - y.(hd) in
	if diff > 0 then -1
	else if diff < 0 then 1
	else lex tl x y
  let grlex vo x y =
    let ddiff = degree x - degree y in
    if ddiff > 0 then -1
    else if ddiff < 0 then 1
    else lex vo x y
  let grevlex vo x y =
    let ddiff = degree x - degree y in
    if ddiff > 0 then -1
    else if ddiff < 0 then 1
    else lex (List.rev vo) y x
  let order_auto lex x y =
    lex (List.range 0 (Array.length x - 1) 1) x y
  let clean = List.filter (fun (a, _) -> not (a =^ F.zero))
 let ( +~ ) f g =
   let order = order_auto lex in
   let order_term (_, x) (_, y) = order x y in
    let rec aux f g accum =
      match f, g with
	[], g -> g @ accum
      | f, [] -> f @ accum
      | ((fa, fx) as fhd) :: ftl, ((ga, gx) as ghd) :: gtl ->
	  if order fx gx < 0 then
	    aux ftl g (fhd :: accum)
	  else if order fx gx > 0 then
	    aux f gtl (ghd :: accum)
	  else
	    aux ftl gtl ((fa +^ ga, fx) :: accum)
    in
    aux (List.sort order_term f) (List.sort order_term g) [] |> clean
 let ( +: ) t f = [t] +~ f
 let mult_const_poly f a =
   List.map (fun (b, x) -> a *^ b, x) f |> clean
 let mult_term_poly (a, x) f =
   List.map (fun (b, y) -> (a *^ b, Array.map2 (+) x y)) f |> clean
 let ( -~ ) f g = f +~ mult_const_poly g (F.negative F.one)
 let ( *~ ) f g =
   List.map (fun t -> mult_term_poly t g) f |> List.fold_left ( +~ ) []
 let ( =~ ) f g = (f -~ g) = []
 let divide_term (a, x) (b, y) =
   (a /^ b, Array.map2 (-) x y)
 let var_to_string num_total_vars num_var =
   if num_total_vars <= 3 then [|"x";"y";"z"|].(num_var)
   else [|"z";"y";"x";"w";"v";"u";"t";"s";"r";"q";"p"|].(num_total_vars - num_var - 1)
 let term_to_string (a, x) =
   let num_total_vars = Array.length x in
   let powered_var_to_string i v =
     if v = 0 then "" (* const *)
     else if v = 1 then var_to_string num_total_vars i
     else var_to_string num_total_vars i ^ "^" ^ string_of_int v (* v-th power *)
   in
   let vars = Array.fold_left (^) "" (Array.mapi powered_var_to_string x) in
   F.to_string a ^ (if vars = "" then "" else " * ") ^ vars
 let to_string = function
     [] -> "0" (* zero polynomial *)
   | [hd] -> term_to_string hd (* single term polynomial *)
   | hd :: tl ->
       List.fold_left (fun t1 t2 -> t1^" + "^t2) (term_to_string hd) (List.map term_to_string tl)
 let divide mo gs f = (* monomial order, divisors, dividend respectively *)
   let sort = List.sort (fun (_, x) (_, y) -> mo x y) in
   let gs, f = List.map sort gs, sort f in
   let rec aux qs r r_inter =
     (* Printf.printf "r_inter : %s\n" (to_string r_inter);
     Printf.printf "r : %s\n" (to_string r); *)
     match r_inter with
       [] -> List.map clean qs, clean r
     | r_inter_hd :: r_inter_tl ->
	 let r_inter = sort r_inter in
	 let rec find_dividable qs gs qaccum =
	   match qs, gs with
	     [], [] -> raise Not_found
	   | [], _ | _, [] -> failwith "PolynomialRing.divide : quotient or divisor is missing."
	   | q :: qtl, g :: gtl ->
	       if is_lterm_divider mo g r_inter then
		 let divided = divide_term (lterm mo r_inter) (lterm mo g) in
		 let new_qs = List.rev qaccum @ ((divided +: q) :: qtl) in
		 let r_inter = r_inter -~ mult_term_poly divided g in
		 (* Printf.printf "divider : %s\ndivided : %s\nr_inter' : %s\n\n" (to_string g) (term_to_string divided) (to_string r_inter); *)
		 (new_qs, r_inter)
	       else find_dividable qtl gtl (q :: qaccum)
	 in
	 try
	   let (qs, r_inter) = find_dividable qs gs [] in
	   aux qs r r_inter
	 with
	   Not_found -> aux qs (List.hd r_inter +: r) (List.tl r_inter)
   in aux (List.map (fun _ -> []) gs) [] f
end
