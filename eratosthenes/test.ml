open Util;;
let n = Sys.argv.(1) |> int_of_string;;
Printf.printf "Prime numbers up to %d are : \n" n;
Main.eratosthenes n |> List.print_int_list
