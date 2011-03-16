let capacity = 20

let goods = [(1, 2); (1, 1); (2, 2); (2, 3); (3, 2); (3, 6); (5, 5); (5, 7); (6, 10); (6, 4); (9, 11); (10, 12); (10, 9); (11, 15)]

let solution = Main.opt capacity goods

let _ =
  List.iter (fun x -> Printf.printf "(%d, %d) " (fst x) (snd x)) solution;
  Printf.printf "\n"
