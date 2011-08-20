open Util

(* fpiSummerIntern2011Quiz *)
(*
let x1 = "aabbababa"
let x2 = "aabbaabbaabbaabbccddaaabbabaababaaaaabaa"
let x3 = "baabbaa"
let x4 = "abcaabaca"
let x5 = "abcaabaac"
let xs = [x1; x2; x3; x4; x5]

let _ = List.iter (fun x -> Printf.printf "problem : %s, ans : %c\n" x (FpiSummerIntern2011Quiz.find_freq x)) xs
*)

(* suffixArray *)

let make_string f m n =
  let x = String.create n in
  List.iter (fun i -> x.[i] <- Char.chr(f i)) (List.init identity n);
  x

let random_string m n =
  let f i = Random.int m + 33 in
  make_string f m n

let u1 m n =
  let f i = i mod m + 33 in
  make_string f m n

let u2 m n =
  let f i = i / (n / m) + 33 in
  make_string f m n

let read i = int_of_string Sys.argv.(i)

let _ =
  let s = match Sys.argv.(1) with
      "-s" -> String.sub (read_line ()) 0 (read 2) (* stdin *)
    | "-r" -> random_string (read 2) (read 3)
    | "-u1" -> u1 (read 2) (read 3)
    | "-u2" -> u2 (read 2) (read 3)
    | _ -> failwith "invalid input"
  in
  time Unix.gettimeofday 1
    (fun s -> SuffixArray.make_suffix_array s |> Array.iter (Printf.printf "%d "))
    s
  |> Printf.printf "\ntime : %f[s]\n"
