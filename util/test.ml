open Util
let f m n = (List.delete_nth m) (List.init identity n);;
let check_delete_nth n trial = time Unix.gettimeofday trial (List.delete_nth (n / 2)) (List.init identity n);;
let check_delete_nth_naive n trial = time Unix.gettimeofday trial (List.delete_nth_naive (n / 2)) (List.init identity n);;
