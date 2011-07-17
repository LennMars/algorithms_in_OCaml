open Util;;
if Array.length Sys.argv < 2 || int_of_string Sys.argv.(1) < 2 then
  Printf.printf "input some integer > 1\n"
else
  let n = Sys.argv.(1) |> int_of_string in
  Main.eratosthenes n |> List.print_int_list
