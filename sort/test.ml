open Util



let xs = [|4;5;3;2;0;1;6;8;7;5;6;3;1|];;

let _ =
  Printf.printf "before : ";
  Array.print string_of_int xs;
  Printf.printf "after  : ";
  Array.print string_of_int (Quicksort.quicksort compare xs)
