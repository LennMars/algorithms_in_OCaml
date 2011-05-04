open Util

let anon_fun s = "unknown argument: " ^ s |> print_endline
(* options init *)
let to_display = ref false
let seed = ref 0
let turn = ref 1
let prob = ref 0.5
let m = ref 10
let spec_display = ("-d", Arg.Bool (fun x -> to_display := x), "")
let spec_seed = ("-s", Arg.Int (fun x -> seed := x), "set seed")
let spec_turn = ("-t", Arg.Int (fun x -> turn := x), "set turn")
let spec_prob = ("-p", Arg.Float (fun x -> prob := x), "set prob")
let spec_m = ("-m", Arg.Int (fun x -> m := x), "set m");;
Arg.parse [spec_display; spec_seed; spec_turn; spec_prob; spec_m] anon_fun "";;
Random.init !seed;;

(* Sampling from binomial distribution whose #experiments is n and success probability is p. *)
let rec binomial_generator m p () =
  let rec binomial_generator_aux m accum =
    if m <= 0 then
      accum
    else
      binomial_generator_aux (m - 1) (accum + if Random.float 1. < p then 1 else 0)
  in
  binomial_generator_aux m 0;;

(* binomial coefficient C(n, k). *)
let bin_coef n k =
  if n < k then raise (Invalid_argument "first arg too small");
  let rec fact l accum =
    if l <= 1 then accum else fact (l - 1) (l * accum)
  in
  fact n 1 / fact k 1 / fact (n - k) 1

let binomial_expectator m sequence current_value =
  let turn = List.length sequence + 1
  and past_sum = List.fold_left ( + ) 0 sequence in
  let estimated_prob x = float (past_sum + x) /. float (m * turn) in
  let max_likelihood_dist_point x =
    float (bin_coef m x)
    *. estimated_prob x ** float x
    *. (1. -. estimated_prob x) ** float (m - x)
  in
  let max_likelihood_dist x =
    List.fold_left ( *. ) 1. (List.map max_likelihood_dist_point (x :: sequence))
  in
  let normalizer = List.fold_left (+.) 0. (List.init max_likelihood_dist (m + 1))in
  max_likelihood_dist current_value /. normalizer

let negative_log_loss x =
  if x < 0. || x > 1. then raise (Invalid_argument "not probability");
  (-1.) *. log x

let main loss_func generator expectator turn =
  let rec main_aux total_loss sequence t =
    if t > turn then
      let min_total_loss = List.fold_left ( +. ) 0.
	(List.map (fun x -> loss_func (expectator sequence x)) sequence)
      and upper_bound_of_regret = float !m /. 2. *. (log (float turn +. 1.) +. 1.) in
      Printf.printf "regret : %f, upper_bound : %f\n"
	(total_loss -. min_total_loss) upper_bound_of_regret
    else
      let current_value = generator () in
      let loss = loss_func (expectator sequence current_value) in
      if !to_display then Printf.printf "turn : %d, value : %d, loss : %f\n" t current_value loss;
      main_aux
	(total_loss +. loss)
	(current_value :: sequence)
	(t + 1)
  in
  main_aux 0. [] 1

let _ = main negative_log_loss (binomial_generator !m !prob) (binomial_expectator !m) !turn; print_newline ()
