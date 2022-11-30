open Pa_ppx_utils

module Angles = struct
let twopi = 2. *. Gg.Float.pi
let d2r d = d *. twopi /. 360.
let r2d r = r *. 360. /. twopi
let rec norm1_tol ~eps a =
  if Gg.Float.is_nan a then a
  else if Gg.Float.compare_tol ~eps a 0. < 0 then norm1_tol ~eps (a +. twopi)
  else if Gg.Float.compare_tol ~eps twopi a <= 0 then norm1_tol ~eps (a -. twopi)
  else a

  module ZYZ = struct
    type t = { z_0 : float ; y_1 : float ; z_2 : float }[@@deriving (to_yojson, show, eq, ord);]
    let norm_tol ~eps x = { z_2 = norm1_tol ~eps x.z_2 ; y_1 = norm1_tol ~eps x.y_1 ; z_0 = norm1_tol ~eps x.z_0 }
    let cmp_tol ~eps a b =
      let a = norm_tol ~eps a in
      let b = norm_tol ~eps b in
      Gg.Float.equal_tol ~eps a.z_0 b.z_0
      && Gg.Float.equal_tol ~eps a.y_1 b.y_1
      && Gg.Float.equal_tol ~eps a.z_2 b.z_2
  end

  type generic_euler_t = { theta : float ; phi : float ; lambda : float }[@@deriving (to_yojson, show, eq, ord);]

  type t =
    ZYZ of ZYZ.t[@@deriving (to_yojson, show, eq, ord);]

  let norm_tol ~eps = function
      ZYZ x -> ZYZ (ZYZ.norm_tol ~eps x)

  let of_zyz x = ZYZ ZYZ.{z_0 = x.theta ; y_1 = x.phi ; z_2 = x.lambda }

  let cmp_tol ~eps a b = match (a,b) with
      (ZYZ a, ZYZ b) ->
      ZYZ.cmp_tol ~eps a b
    | _ -> false

end

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

  let of_rotations pairs =
      let q = List.fold_left (fun q (rot, ang) -> Q.mul q (of_rotation rot ang)) Q.id pairs in
      Q.unit q

  let of_euler rots angles =
      assert (List.length rots = List.length angles) ;
      let pairs = Std.combine rots angles in
      of_rotations pairs

  let of_zyz a =
    let open Angles in
    of_rotations ZYZ.[(Z,a.z_0); (Y,a.y_1); (Z,a.z_2)]

  let of_angles = function
      Angles.ZYZ a -> of_zyz a

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

(**

if (EulerOrder=='zyz') { 
EA <- cbind(atan2((2*(Q[,3]*Q[,4] - Q[,1]*Q[,2])),(2*(Q[,2]*Q[,4] + Q[,1]*Q[,3]))), atan2(sqrt(1-(Q[,1]^2 - Q[,2]^2 - Q[,3]^2 + Q[,4]^2)^2),(Q[,1]^2 - Q[,2]^2 - Q[,3]^2 + Q[,4]^2)),atan2((2*(Q[,3]*Q[,4] + Q[,1]*Q[,2])),-(2*(Q[,2]*Q[,4] - Q[,1]*Q[,3]))))
}


 *)

let sqr x = x *. x

let to_zyz q =
  let open Angles.ZYZ in
  let q = Q.to_explicit q in
  
  let z_0 = atan2 (2. *. (q.y *. q.z -. q.w *. q.x)) (2. *. (q.x *. q.z +. q.w *. q.y)) in
  let y_1 = atan2 (sqrt(1. -. (sqr ((sqr q.w) -. (sqr q.x) -. (sqr q.y) +. (sqr q.z))))) ((sqr q.w) -. (sqr q.x) -. (sqr q.y) +. (sqr q.z)) in
  let z_2 = atan2(2. *. (q.y *. q.z +. q.w *. q.x)) (-. (2. *. (q.x *. q.z -. q.w *. q.y))) in
  {z_0 ; y_1 ; z_2}

  let to_zyz0 q =
    let open Angles.ZYZ in
    let open Gg in
    let m = to_m3 q in
    if M3.e22 m < 1. then
      if M3.e22 m > -1. then
        {
          z_0 = Float.atan2 (M3.e12 m) (M3.e02 m)
        ; y_1 = Float.acos (M3.e22 m)
        ; z_2 = Float.atan2 (M3.e21 m) (-. (M3.e20 m))
        }
      else
        {
          z_0 = -. (Float.atan2 (M3.e10 m) (M3.e11 m))
        ; y_1 = Float.pi
        ; z_2 = 0.
        }
    else
      {
        z_0 = Float.atan2 (M3.e10 m) (M3.e11 m)
      ; y_1 = 0.
      ; z_2 = 0.
      }

  include ExtendedQuat

  let oper q x =
    let open Gg in
    let x = of_explicit {x=(V3.x x); y=(V3.y x); z=(V3.z x); w=0.} in
    let w = Quat.(mul (mul q x) (conj q)) in
    let w = to_explicit w in
    V3.v w.x w.y w.z

end
