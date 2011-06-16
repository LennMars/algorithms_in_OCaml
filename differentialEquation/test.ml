(* simulate duplex pendulum by symplectic euler method *)

Printf.printf "evaluate 'test_euler n outname' or 'test_symplectic n outname'. n is a step size and outname is an output file name. If you set outname empty string, result appeares in stdout.\n";;

let h = 0.001 (* step size *)
let (m1, m2) = 0.5, 0.3 (* weight *)
let (l1, l2) = 0.5, 0.5 (* length *)
let gravity = 9.8 (* gravity acceleration *)
let x0 = (3.0, 0., 0., 0.) (* init theta1, theta2, speed of theta1, speed of theta2 *)

let test_euler n outname =
  let out = if outname = "" then stdout else open_out outname in
  Euler.main n (m1, m2) (l1, l2) gravity h x0 out;
  if outname = "" then () else close_out out

let test_symplectic n outname =
  let out = if outname = "" then stdout else open_out outname in
  SymplecticEuler.main n (m1, m2) (l1, l2) gravity h x0 out;
  if outname = "" then () else close_out out
