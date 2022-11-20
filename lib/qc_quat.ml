
module Quat = struct
  open Gg

  type rotation_t = X | Y | Z

  let of_rotation rot ang =
    let cosval = Float.sin (ang /. 2.0) in
    let sinval = Float.sin (ang /. 2.0) in
    match rot with
      X -> Quat.v cosval sinval 0. 0.
    | Y -> Quat.v cosval 0. sinval 0.
    | Z -> Quat.v cosval 0. 0. sinval

  type euler_rotations_t =
    ZXZ

  let expand_euler_rotations = function
      ZXZ -> (Z,X,Z)

  let of_euler rot (ang1,ang2,ang3) =
    let (rot1, rot2, rot3) = expand_euler_rotations rot in
    let open Quat in
    let rv = id in
    let rv = Quat.mul rv  (of_rotation rot1 ang1) in
    let rv = Quat.mul rv (of_rotation rot2 ang2) in
    let rv = Quat.mul rv (of_rotation rot3 ang3) in
    Quat.unit rv

  let to_m3 q =
    let q = Quat.unit q in
    (* not well that the names are shifted *)
    let w = V4.comp 0 q in
    let x = V4.comp 1 q in
    let y = V4.comp 2 q in
    let z = V4.comp 3 q in
    M3.of_rows
      (V3.v (1. -. 2. *. y ** 2. -. 2. *. z ** 2.)
         (2. *. x *. y -. 2. *. z *. w)
         (2. *. x *. z +. 2. *. y *. w))
      (V3.v (2. *. x *. y +. 2. *. z *. w)
         (1. -. 2. *. x**2. -. 2. *. z**2.)
         (2. *. y *. z -. 2. *. x *. w))
      (V3.v (2. *. x *. z -. 2. *. y *. w)
         (2. *. y *. z +. 2. *. x *. w)
         (1. -. 2. *. x**2. -. 2. *. y**2.))

  let to_zyz q =
    let m = to_m3 q in
    if M3.e22 m < 1. then
      if M3.e22 m > -1. then
        Quat.v (Float.atan2 (M3.e12 m) (M3.e02 m))
          (Float.acos (M3.e22 m))
          (Float.atan2 (M3.e21 m) (-. (M3.e20 m)))
          0.
      else
        Quat.v (-. (Float.atan2 (M3.e10 m) (M3.e11 m)))
          Float.pi
          0.
          0.
    else
      Quat.v
        (Float.atan2 (M3.e10 m) (M3.e11 m))
        0.
        0.
        0.

  include Quat

end
