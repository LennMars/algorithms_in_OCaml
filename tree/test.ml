open Util

module M = struct
  type t = int * string
  let compare (m, _) (n, _)= compare m n
end

module T = SplayTree.Make(M)



let t = T.empty
let t = T.insert (13, "thirteen") t
let t = T.insert (8, "eight") t
let t = T.insert (17, "seventeen") t
let t = T.insert (1, "one") t
let t = T.insert (11, "eleven") t
let t = T.insert (15, "fifteen") t
let t = T.insert (25, "twenty-five") t
