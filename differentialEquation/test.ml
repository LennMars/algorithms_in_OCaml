(* simulate duplex pendulum by symplectic euler method *)
let h = 0.001 (* step size *)
let (m1, m2) = 1., 0.1 (* weight *)
let (l1, l2) = 1., 0.1 (* length *)
let gravity = 9.8 (* gravity acceleration *)
let x0 = (0., 0.1, 0., 0.) (* init theta1, theta2, speed of theta1, speed of theta2 *)
let test n = SymplecticEuler.main n (m1, m2) (l1, l2) gravity h x0
