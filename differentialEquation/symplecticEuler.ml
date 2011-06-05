let rec newton f f' x tol =
  if abs_float (f x) < tol then x
  else newton f f' (x -. f x /. f' x) tol

let tol = 0.000000001

let cos2 x = cos x ** 2.

let print (q1, q2, p1, p2) = Printf.printf "%f %f\n" q1 q2

let main turn (m1, m2) (l1, l2) gravity h (theta1, theta2, theta_dot1, theta_dot2) =
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
  let (q1, q2) = theta1, theta2 in
  let a11 = m *. l1 *. l1
  and a12 = m2 *. l1 *. l2 *. cos (q1 -. q2)
  and a22 = m2 *. l2 *. l2
  in
  let (p1, p2) = a11 *. theta_dot1 +. a12 *. theta_dot2, a12 *. theta_dot1 +. a22 *. theta_dot2 in
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
    let  p1_next, p2_next =
      let t = (f1 *. f2 *. sin qd) /. (l1 *. l2 *. g) ** 2. in
      p1 -. h *. (t +. m *. l1 *. gravity *. sin q1_next), p2 +. h *. (t -. m2 *. l2 *. gravity *. sin q2_next)
    in
    (q1_next, q2_next, p1_next, p2_next)
  in
  let rec aux turn x =
    if turn <= 0 then ()
    else let next = step x in print next; aux (turn - 1) next
  in
  aux turn (q1, q2, p1, p2)
