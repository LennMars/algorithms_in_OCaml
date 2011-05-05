open Unix
open Util

(** Get resolution of unix timer. *)
let get_tick n =
  let rec get_tick_aux i accum =
    if i <= 0 then
      let tick = List.to_chain accum
	|> List.map (fun (a, b) -> a -. b)
	|> List.filter ((<) 0.) |> List.find_min identity
      and overhead = (List.find_max identity accum -. List.find_min identity accum) /. float n
      in
      (tick, overhead)
    else get_tick_aux (i - 1) (gettimeofday () :: accum)
  in
  get_tick_aux n []

let _ =
  let (tick, overhead) = get_tick 10000 in
  Printf.printf "Unix timer : resolution is %.2e sec, overhead is %.2e sec.\n" tick overhead;;
