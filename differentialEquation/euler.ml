open Util

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

let print out_ch m l gravity ((th1, th2, thd1, thd2) as x) =
  let energy = energy m l gravity x in
  Printf.fprintf out_ch "%f %f %f %f %f\n" th1 th2 thd1 thd2 energy

let main turn (m1, m2) (l1, l2) gravity h (th1, th2, thd1, thd2) out_ch =
  Printf.fprintf out_ch "# euler method. weight : (%f, %f) length : (%f, %f) gravity : %f stepsize : %f num_step : %d init : (%f, %f, %f, %f)\n" m1 m2 l1 l2 gravity h turn th1 th2 thd1 thd2;
  (* calculate sub constants *)
  let m = m2 /. (m1 +. m2)
  and l = l2 /. l1
  and omega_squared = gravity /. l1
  in
  (* scheme body *)
  let step (th1, th2, thd1, thd2) =
    let th1_next = th1 +. h *. thd1
    and th2_next = th2 +. h *. thd2
    and thd1_next, thd2_next =
      let dth = th1 -. th2 in
      let divider = l *. (1. -. m *. cos2 dth) in
      let denom1 = omega_squared *. l *. (-1. *. sin th1 +. m *. cos dth *. sin th2) -. m *. l *. (thd1 ** 2. *. cos dth +. l *. thd2 ** 2.) *. sin dth
      and denom2 = omega_squared *. (cos dth *. sin th1 -. sin th2) +. (thd1 ** 2. +. m *. l *. thd2 ** 2.) *. sin dth
      in
      thd1 +. h *. denom1 /. divider, thd2 +. h *. denom2 /. divider
    in
    (th1_next, th2_next, thd1_next,  thd2_next)
  in
  let rec aux turn x =
    if turn <= 0 then ()
    else
      let next = step x in
      print out_ch (m1, m2) (l1, l2) gravity next;
      aux (turn - 1) next
  in
  aux turn (th1, th2, thd1, thd2)
