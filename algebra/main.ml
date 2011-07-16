(* type si = {s : int; m : int}
let x : si = {s = 0; m = 1}
let t : si = {s = 1; m = 0}
 *)

open Util

type symbol =
    ConstSymbol of float
  | Func of string list (* constructor is a list of arguments *)
  | Var

type env = (string * symbol) list

let env : env = [("x", Var); ("t", Var); ("u", Func ["x";"t"])]

type dvar = string * ((string * int) list)
(* snd represents how many times differentiated by what variable *)

type expr =
    Const of int (* todo : change to rational *)
  | DVar of dvar
  | Sum of expr * expr
  | Prod of expr * expr
  | Partial of string * expr
  | Power of expr * int

let t1 = DVar ("u", ["t", 1])
let t2 = Prod ((DVar ("u", [])), (DVar ("u", ["x", 1])))
let t3 = DVar ("u", ["x", 3])
let kdv = Sum (t1, Sum(t2, t3))

let expr1 a b = Prod (Power (DVar ("u", []), a), Power((DVar ("u", ["x", 1])), b))

let string_of_diff_args xs =
  let concat = String.concat "" in
  List.map (fun (s, i) -> concat (List.init (fun _ -> s) i)) xs |> concat

let rec partial env s expr =
  let rec aux expr =
    match expr with
      Const i -> Const 0
    | DVar (v, xs) -> begin
	match List.assoc v env with
	  ConstSymbol _ -> Const 0
	| Var -> if s = v then Const 1 else Const 0
	| Func args ->
	    if List.exists ((=) s) args then
	      try
		let order = List.assoc s xs in
		let xs = (s, order + 1) :: List.remove (fun x -> fst x = s) xs in
		DVar (v, xs)
	      with
		Not_found -> DVar (v, (s, 1) :: xs)
	    else
	      Const 0 end
    | Sum (l, r) -> Sum (aux l, aux r)
    | Prod (l, r) -> Sum (Prod (aux l, r), Prod (l, aux r))
    | Partial (s', e) -> aux (partial env s' e)
    | Power (e, i) ->
	if i < 0 then failwith "negative power not allowed."
	else if i = 0 then Const 0
	else if i = 1 then aux e
	else aux (Prod (e, Power (e, i - 1)))
  in
  (try
    if List.assoc s env <> Var then failwith "not variable"
  with
    Not_found -> failwith "not in env");
  aux expr

let print_expr expr =
  let open_paren prec op_prec =
    if prec > op_prec then print_string "("
  and close_paren prec op_prec =
    if prec > op_prec then print_string ")"
  in
  let rec print prec = function
      Const i -> print_int i
    | DVar (s, xs) -> begin
	match xs with
	  [] -> print_string s
	| xs -> print_string (s ^ "_" ^ string_of_diff_args xs) end
    | Sum (l, r) ->
	open_paren prec 0;
	print 0 l; print_string " + "; print 0 r;
	close_paren prec 0
    | Prod (l, r) ->
	open_paren prec 2;
	print 2 l; print_string " * "; print 2 r;
	close_paren prec 2
    | Partial (s, e) ->
	print_string ("d/d" ^ s);
	print 0 e;
    | Power (e, i) ->
	if i = 0 then ()
	else if i = 1 then print 3 e
	else begin
	  open_paren prec 3;
	  print 3 e;
	  close_paren prec 3;
	  print_string ("^" ^ string_of_int i)
	end
  in
  print 0 expr;
  print_newline ()
