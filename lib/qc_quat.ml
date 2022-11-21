open Pa_ppx_utils

module type EXTENDED_QUAT_SIG = sig
  type t = Gg.Quat.t [@@deriving to_yojson, show, eq]
  type explicit_t = { x : float ; y : float ; z : float ; w : float }[@@deriving to_yojson, show, eq]
  val to_explicit : t -> explicit_t
  val of_explicit : explicit_t -> t
  include (module type of Gg.Quat with type t := t)
end

module ExtendedQuat : EXTENDED_QUAT_SIG = struct
  open Gg
  include Quat
  type explicit_t = { x : float ; y : float ; z : float ; w : float }[@@deriving to_yojson, show { with_path = false }, eq]

  let to_explicit q = { x = V4.x q ; y = V4.y q ; z = V4.z q ; w = V4.w q }
  let of_explicit {x;y;z;w} = v x y z w

  let equal q1 q2 = Gg.V4.equal q1 q2 || Gg.V4.equal q1 (Gg.V4.neg q2)
  let pp pps q = Fmt.(pf pps "%a" pp_explicit_t (to_explicit q))
  let show q = Fmt.(str "%a" pp q)

  let to_yojson q =
    q |> to_explicit |> explicit_t_to_yojson

end

module Quat = struct
  module Q = ExtendedQuat

  type rotation_t = X | Y | Z

  let of_rotation rot ang =
    let open Gg in
    let open V3 in
    let axis = match rot with
        X -> ox
      | Y -> oy
      | Z -> oz in
  
    let m3 = M3.rot3_axis axis ang in
    Quat.unit (Quat.of_m3 m3)

  let of_euler rots angles =
      assert (List.length rots = List.length angles) ;
      let pairs = Std.combine rots angles in
      let q = List.fold_left (fun q (rot, ang) -> Q.mul q (of_rotation rot ang)) Q.id pairs in
      Q.unit q

  let to_m3 q =
    let q = Q.unit q in
    (* not well that the names are shifted *)
    let open Gg in
    let w = V4.w q in
    let x = V4.x q in
    let y = V4.y q in
    let z = V4.z q in
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
    let open Gg in
    let m = to_m3 q in
    if M3.e22 m < 1. then
      if M3.e22 m > -1. then
        Q.v (Float.atan2 (M3.e12 m) (M3.e02 m))
          (Float.acos (M3.e22 m))
          (Float.atan2 (M3.e21 m) (-. (M3.e20 m)))
          0.
      else
        Q.v (-. (Float.atan2 (M3.e10 m) (M3.e11 m)))
          Float.pi
          0.
          0.
    else
      Q.v
        (Float.atan2 (M3.e10 m) (M3.e11 m))
        0.
        0.
        0.

  include ExtendedQuat

end
