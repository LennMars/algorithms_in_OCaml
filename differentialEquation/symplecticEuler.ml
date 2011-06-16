open Util

let rec newton f f' x tol =
  let fx = f x in
  if abs_float fx < tol then x
  else newton f f' (x -. fx /. f' x) tol

let tol = 1e-13

let cos2 x = cos x ** 2.

let to_general_coord (m1, m2) (l1, l2) (th1, th2, thd1, thd2) =
  let q1, q2 = th1, th2
  and a11 = (m1 +. m2) *. l1 ** 2.
  and a12 = m2 *. l1 *. l2 *. cos (th1 -. th2)
  and a22 = m2 *. l2 ** 2.
  in
  let p1 = a11 *. thd1 +. a12 *. thd2
  and p2 = a12 *. thd1 +. a22 *. thd2
  in
  (q1, q2, p1, p2)

let of_general_coord (m1, m2) (l1, l2) (q1, q2, p1, p2) =
  let th1, th2 = q1, q2 in
  let thd = th1 -. th2 in
  let divider = (m2 *. l1 *. l2) ** 2. *. ((m1 +. m2) /. m2 -. cos2 thd)
  in
  let b11 = m2 *. l2 ** 2. /. divider
  and b12 = -1. *. m2 *. l1 *. l2 *. cos thd /. divider
  and b22 = (m1 +. m2)  *. l1 ** 2. /. divider
  in
  let thd1 = b11 *. p1 +. b12 *. p2
  and thd2 = b12 *. p1 +. b22 *. p2
  in
  (th1, th2, thd1, thd2)

let energy (m1, m2) (l1, l2) gravity (th1, th2, thd1, thd2) =
  let t = 0.5 *. m1 *. (l1 *. thd1) ** 2. +. 0.5 *. m2 *. ((l1 *. thd1) ** 2. +. (l2 *. thd2) ** 2. +. 2. *. l1 *. l2 *. thd1 *. thd2 *. cos (th1 -. th2))
  and u = -1. *. m1 *. l1 *. gravity *. cos th1 -. m2 *. gravity *. (l1 *. cos l1 +. l2 *. cos l2)
  in
  t +. u

let rec normalize theta =
  if abs_float theta < pi then theta
  else if theta > 0. then normalize (theta -. 2. *. pi)
  else normalize (theta +. 2. *. pi)

let print out_ch m l gravity ((q1, q2, p1, p2) as x) =
  let (th1, th2, thd1, thd2) as c = of_general_coord m l x in
  let energy = energy m l gravity c in
  if abs_float thd1 < 0.1 then Printf.fprintf out_ch "%f %f %f %f %f\n" (normalize th1) (normalize th2) thd1 thd2 energy

let main turn (m1, m2) (l1, l2) gravity h (theta1, theta2, theta_dot1, theta_dot2) out_ch =
  Printf.fprintf out_ch "# tol_of_newton : %e, weight : (%f, %f) length : (%f, %f) gravity : %f stepsize : %f num_step : %d init : (%f, %f, %f, %f)\n" tol m1 m2 l1 l2 gravity h turn theta1 theta2 theta_dot1 theta_dot2;
  (* calculate sub constants *)
  let m = m1 +. m2 in
  let coef1 = h /. (l1 *. l1)
  and coef2 = (h *. m) /. (m2 *. l2 *. l2)
  and coef3 = h /. (l1 *. l2)
  and f1 (q1, q2, p1, p2) qd = l2 *. p1 -. l1 *. p2 *. cos qd
  and f2 (q1, q2, p1, p2) qd = l1 *. m *. p2 -. m2 *. l2 *. p1 *. cos qd
  and g qd = m -. m2 *. cos2 qd
  in
  (* initialize generalized coordinates *)
  let (q1, q2, p1, p2) = to_general_coord (m1, m2) (l1, l2) (theta1, theta2, theta_dot1, theta_dot2) in
  (* scheme body *)
  let step ((q1, q2, p1, p2) as x) =
    let kappa = coef1 *. p1 -. coef2 *. p2
    and lambda = coef3 *. (p1 -. p2)
    in
    let f qd = q1 -. q2 +. (kappa +. lambda *. cos qd) /. (m -. m2 *. cos2 qd) -. qd
    and f' qd = (-1. *. kappa *. m2 *. sin (2. *. qd) -. lambda *. m *. sin qd -. lambda *. m2 *. sin qd *. cos2 qd) /. (m -. m2 *. cos2 qd) ** 2. -. 1.
    in
    let qd = newton f f' (q1 -. q2) tol in
    let (f1, f2, g) = f1 x qd, f2 x qd, g qd in
    let q1_next = q1 +. h *. f1 /. (l1 *. l1 *. l2 *. g)
    and q2_next = q2 +. h *. f2 /. (m2 *. l1 *. l2 *. l2 *. g)
    in
    let p1_next, p2_next =
      let t = (f1 *. f2 *. sin qd) /. (l1 *. l2 *. g) ** 2. in
      p1 -. h *. (t +. m *. l1 *. gravity *. sin q1_next), p2 +. h *. (t -. m2 *. l2 *. gravity *. sin q2_next)
    in
    (q1_next, q2_next, p1_next, p2_next)
  in
  let rec aux turn x =
    if turn <= 0 then ()
    else
      let next = step x in
      print out_ch (m1, m2) (l1, l2) gravity next;
      aux (turn - 1) next
  in
  aux turn (q1, q2, p1, p2)
