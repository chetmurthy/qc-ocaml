(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open OUnit2
open Gg
open Qc_quat

Pa_ppx_base.Pp_MLast.Ploc.pp_loc_verbose := true ;;
Pa_ppx_runtime.Exceptions.Ploc.pp_loc_verbose := true ;;
Pa_ppx_runtime_fat.Exceptions.Ploc.pp_loc_verbose := true ;;

let rotation_matrix angle axis =
  let open Quat in
  let direction = match axis with
      X -> V3.ox
    | Y -> V3.oy
    | Z -> V3.oz in
  M3.rot3_axis direction angle


let r2d r = 360. *. r /. (2. *. Float.pi)
let d2r d = (2. *. Float.pi) *. (d /. 360.)

let rnd_array = [| 0.5 ; 0.8 ; 0.9 ; -0.3 |]
let quat_unnormalized = Quat.v rnd_array.(1) rnd_array.(2) rnd_array.(3) rnd_array.(0) 
let axes = Quat.[| X ; Y ; Z |]
let rnd = [| -0.92545003 ; -2.19985357 ; 6.01761209 |]
let idx = [| 0; 2; 1 |]
let mat1 =
  M3.mul
    (rotation_matrix rnd.(0)  axes.(idx.(0)))
    (M3.mul
       (rotation_matrix rnd.(1) axes.(idx.(1)))
       (rotation_matrix rnd.(2) axes.(idx.(2))))
let quat = Quat.of_euler [X;Z;Y] (Array.to_list rnd)
let mat2 = Quat.to_m3 quat

let equal_f = Float.equal_tol ~eps:1e-4
let compare_f = Float.compare_tol ~eps:1e-4
let cmp_quat q1 q2 = (V4.equal_f equal_f q1 q2) || (V4.equal_f equal_f q1 (V4.neg q2))
let printer_quat q = Fmt.(str "%a" Quat.pp q)
let cmp_m3 = M3.equal_f equal_f
let printer_m3 m = Fmt.(str "%a" M3.pp m)

let simple_tests = "simple tests" >:::
  [
    "simple" >:: (fun _ ->
      let expect_quat_unnormalized_matrix =
        M3.of_rows
          (V3.v (-0.00558659)  0.97206704  0.23463687)
          (V3.v  0.63687151  0.18435754 (-0.74860335))
          (V3.v (-0.77094972)  0.1452514  (-0.62011173)) in
      assert_equal ~msg:"quat_unnormalized.matrix" ~cmp:cmp_m3 ~printer:printer_m3
        expect_quat_unnormalized_matrix
        (Quat.to_m3 quat_unnormalized) ;
      let expect_mat1 =
        M3.of_rows
          (V3.v (-0.56775533)  0.80858257  0.15442836)
          (V3.v (-0.25961321) (-0.35389757)  0.89853074)
          (V3.v 0.78118812  0.47005397  0.41084594) in
      assert_equal ~msg:"mat1" ~cmp:cmp_m3 ~printer:printer_m3
        expect_mat1 mat1 ;
      let expect_quat = Quat.v 0.30630716  0.44805463  0.76362603 (-0.34971168) in
      assert_equal ~msg:"quat" ~cmp:cmp_quat ~printer:printer_quat
        expect_quat quat ;
      let expect_mat2 =
        M3.of_rows
          (V3.v (-0.56775533)  0.80858257  0.15442836)
          (V3.v (-0.25961321) (-0.35389757)  0.89853074)
          (V3.v  0.78118812  0.47005397  0.41084594) in
      assert_equal ~msg:"mat2" ~cmp:cmp_m3 ~printer:printer_m3
        expect_mat2 mat2
    )
  ; "axis_rotation" >:: (fun _ ->
    let quat0 = Quat.(of_rotation axes.(0) rnd.(0)) in
    let expected_quat0 = Quat.v (-0.44638821) 0.         0. 0.89483941 in

    assert_equal ~msg:"quat0" ~cmp:cmp_quat ~printer:printer_quat
      expected_quat0 quat0 ;

    let quat1 = Quat.(of_rotation axes.(1) rnd.(1)) in
    let expected_quat1 = Quat.v 0. (-0.89117415) 0. 0.45366137 in


    assert_equal ~msg:"quat1" ~cmp:cmp_quat ~printer:printer_quat
      expected_quat1 quat1 ;

    let quat2 = Quat.(of_rotation axes.(2) rnd.(2)) in
    let expected_quat2 = Quat.v  0.          0. 0.13239673 (-0.9911968) in


    assert_equal ~msg:"quat2" ~cmp:cmp_quat ~printer:printer_quat
      expected_quat2 quat2 ;

    let quat012 = Quat.(mul (mul quat0 quat1) quat2) in
    let expected_quat012 = Quat.v 0.09514556  0.81724911 (-0.34056065) (-0.45504907) in

    assert_equal ~msg:"quat012" ~cmp:cmp_quat ~printer:printer_quat
      expected_quat012 quat012 ;

    let quat12 = Quat.(mul quat1 quat2) in
    let expected_quat12 = Quat.v   (-0.11798854)  0.88332897  0.06006328 (-0.4496677) in

    assert_equal ~msg:"quat12" ~cmp:cmp_quat ~printer:printer_quat
      expected_quat12 quat12
  )
  ; "axis_rotation-0" >:: (fun _ ->
    let quat0 = Quat.(of_rotation axes.(0) rnd.(0)) in
    let expected_quat0 = Quat.v (-0.44638821) 0.         0. 0.89483941 in
    assert_equal ~msg:"quat" ~cmp:cmp_quat ~printer:printer_quat
      expected_quat0 quat0
  )
  ; "axis_rotation-1" >:: (fun _ ->
    let quat1 = Quat.(of_rotation axes.(1) rnd.(1)) in
    let expected_quat1 = Quat.v 0. (-0.89117415) 0. 0.45366137 in
    assert_equal ~msg:"quat" ~cmp:cmp_quat ~printer:printer_quat
      expected_quat1 quat1
  )
  ; "axis_rotation-2" >:: (fun _ ->
    let quat2 = Quat.(of_rotation axes.(2) rnd.(2)) in
    let expected_quat2 = Quat.v  0.          0. 0.13239673 (-0.9911968) in
    assert_equal ~msg:"quat" ~cmp:cmp_quat ~printer:printer_quat
      expected_quat2 quat2
  )
]
;;

let zyz_to_quat (z1, y1, z2) =
  Quat.(of_euler [Z;Y;Z] [z1;y1;z2])

let zyz_to_quat_test (angles, explicit_q) =
  let open Quat in
  let expected_q = of_explicit explicit_q in
  let name = "zyz->quat:"^(show_explicit_t explicit_q) in
  [name >:: (fun _ ->
    assert_equal ~msg:name ~cmp:cmp_quat ~printer:printer_quat
       expected_q (zyz_to_quat angles))]

let twopi = 2. *. Float.pi
let rec norm_angle a =
  if compare_f a 0. < 0 then norm_angle (a +. twopi)
  else if compare_f twopi a <= 0 then norm_angle (a -. twopi)
  else a
let cmp_angles (a1,a2,a3) (b1, b2, b3) =
  let a1 = norm_angle a1 in
  let a2 = norm_angle a2 in
  let a3 = norm_angle a3 in
  let b1 = norm_angle b1 in
  let b2 = norm_angle b2 in
  let b3 = norm_angle b3 in
  equal_f a1 b1
  && equal_f a2 b2
  && equal_f a3 b3

let printer_angles = [%show: (float * float * float)]

let quat_to_zyz_test (expected_angles, explicit_q) =
  let open Quat in
  let q = of_explicit explicit_q in
  let name = "quat->zyz:"^(show_explicit_t explicit_q) in
  [name >:: (fun _ ->
    assert_equal ~msg:name ~cmp:cmp_angles ~printer:printer_angles
       expected_angles (Quat.to_zyz q))]

let zyz_quat_test (angles,  q) =
  (zyz_to_quat_test (angles,  q))
  @(quat_to_zyz_test (angles,  q))

let zyz_quat_tests = "zyz/quat tests" >:::
(
  List.concat_map zyz_to_quat_test
    Quat.[
    ((0.000000,0.000000,0.100000), {w=0.998750;x=0.000000;y=0.000000;z=0.049979})
  ; ((0.000000,0.000000,0.300000), {w=0.988771;x=0.000000;y=0.000000;z=0.149438})
  ; ((0.000000,0.000000,0.400000), {w=0.980067;x=0.000000;y=0.000000;z=0.198669})
  ]
)
@(
  List.concat_map quat_to_zyz_test
    Quat.[
    ((0.100000,0.000000,0.000000), {w=0.998750;x=0.000000;y=0.000000;z=0.049979})
  ; ((0.300000,0.000000,0.000000), {w=0.988771;x=0.000000;y=0.000000;z=0.149438})
  ; ((0.400000,0.000000,0.000000), {w=0.980067;x=0.000000;y=0.000000;z=0.198669})
  ]
)
@(
  List.concat_map zyz_quat_test
    Quat.[
    ((0.000000,0.000000,0.000000), {w=1.000000;x=0.000000;y=0.000000;z=0.000000})
  ; ((0.000000,0.100000,0.000000), {w=0.998750;x=0.000000;y=0.049979;z=0.000000})
  ; ((0.000000,0.300000,0.000000), {w=0.988771;x=0.000000;y=0.149438;z=0.000000})
  ; ((0.000000,0.400000,0.300000), {w=0.969061;x=0.029689;y=0.196438;z=0.146459})
  ; ((0.100000,0.200000,0.300000), {w=0.975170;x=0.009967;y=0.099335;z=0.197677})
  ; ((0.100000,0.200000,0.400000), {w=0.964072;x=0.014919;y=0.098712;z=0.246168})
  ]
)
;;


let compose_zyz_test (zyz1, zyz2, expected_zyz) =
  let open Quat in
  let name = "compose-zyz:"^(printer_angles zyz1)^"*"^(printer_angles zyz2) in
  [name >:: (fun _ ->
    assert_equal ~msg:name ~cmp:cmp_angles ~printer:printer_angles
       expected_zyz (Quat.to_zyz (Quat.mul (zyz_to_quat zyz1) (zyz_to_quat zyz2))))]


let compose_tests = "compose zyz tests" >:::
(
  List.concat_map compose_zyz_test
    [
      ((0.000000,0.000000,0.100000), (0.000000,0.000000,0.000000), (0.100000,0.000000,0.000000))
    ; ((0.000000,0.000000,0.300000), (0.000000,0.000000,0.000000), (0.300000,0.000000,0.000000))
    ; ((0.000000,0.000000,0.400000), (0.000000,0.000000,0.300000), (0.700000,0.000000,0.000000))
    ; ((0.000000,0.000000,0.785398), (0.000000,0.000000,0.000000), (0.785398,0.000000,0.000000))
    ; ((0.000000,0.000000,1.570796), (0.000000,0.000000,0.000000), (1.570796,0.000000,0.000000))
    ; ((0.000000,0.000000,1.570796), (0.000000,0.000000,4.712389), (0.000000,0.000000,0.000000))
    ; ((0.000000,0.000000,3.141593), (0.000000,0.000000,0.000000), (3.141593,0.000000,0.000000))
    ; ((0.000000,0.000000,3.141593), (0.000000,0.000000,1.570796), (-1.570796,0.000000,0.000000))
    ; ((0.000000,0.000000,3.141593), (0.000000,0.000000,3.141593), (0.000000,0.000000,0.000000))
    ; ((0.000000,0.000000,6.283185), (0.000000,0.000000,0.000000), (0.000000,0.000000,0.000000))
    ; ((0.100000,0.000000,0.000000), (0.000000,0.000000,0.000000), (0.100000,0.000000,0.000000))
    ; ((0.100000,0.200000,0.300000), (0.000000,0.000000,0.100000), (0.100000,0.200000,0.400000))
    ; ((0.200000,0.000000,0.000000), (0.100000,0.000000,0.000000), (0.300000,0.000000,0.000000))
    ; ((1.570796,0.000000,0.785398), (0.000000,0.000000,0.000000), (2.356194,0.000000,0.000000))
    ; ((1.570796,0.000000,3.141593), (0.000000,0.000000,0.000000), (-1.570796,0.000000,0.000000))
    ; ((1.570796,0.000000,3.141593), (0.000000,0.000000,6.283185), (-1.570796,0.000000,0.000000))
    ; ((1.570796,0.000000,3.141593), (1.570796,0.000000,3.141593), (3.141593,0.000000,0.000000))
    ; ((1.570796,1.047198,0.785398), (0.000000,0.000000,0.000000), (1.570796,1.047198,0.785398))

    ]
)
;;


(* Run the tests in test suite *)
let _ =
if not !Sys.interactive then
  run_test_tt_main ("all_tests" >::: [
        simple_tests
      ; zyz_quat_tests
      ; compose_tests
    ])
;;
