(* fpiSummerIntern2011Quiz *)
let x1 = "aabbababa"
let x2 = "aabbaabbaabbaabbccddaaabbabaababaaaaabaa"
let x3 = "baabbaa"
let x4 = "abcaabaca"
let x5 = "abcaabaac"
let xs = [x1; x2; x3; x4; x5]

let _ = List.iter (fun x -> Printf.printf "problem : %s, ans : %c\n" x (FpiSummerIntern2011Quiz.find_freq x)) xs

(* suffixArray *)
let str = "mississippi"
let _ =
  print_newline ();
  print_endline str;
  Array.iter (Printf.printf "%d ") (SuffixArray.make_suffix_array str);
  print_newline ()
