open Unix
open Util

(** Get resolution of unix timer. *)
let get_tick () =
  let rec get_tick_aux i accum =
    if i <= 0 then List.to_chain accum
      |> List.map (fun (a, b) -> a -. b)
      |> List.filter ((<) 0.) |> List.find_min identity
    else get_tick_aux (i - 1) (gettimeofday () :: accum)
  in
  get_tick_aux 10000 []

let _ = Printf.printf "Unix timer resolution is %.2e sec.\n" (get_tick ());;
