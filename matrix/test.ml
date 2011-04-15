open Main

let a = SparseMatrix.make [|
  [|6.;4.;1.|];
  [|4.;5.;0.|];
  [|1.;0.;1.|]|]

let x = [|1.;1.;1.|]

let b = SparseMatrix.mut_mul_vec a x

let x' = SparseMatrix.cg a b [|0.;0.;0.|]




