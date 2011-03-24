open Util

module Int = struct
  type t = int
  let compare = compare
end

module T = RedBlackTree.Make(Int)

let t = T.empty
let t = T.insert 13 t
let t = T.insert 8 t
let t = T.insert 17 t
let t = T.insert 1 t
let t = T.insert 11 t
let t = T.insert 15 t
let t = T.insert 25 t
