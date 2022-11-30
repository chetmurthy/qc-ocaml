(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open OUnit2
open Gg
open Qc_quat

Pa_ppx_base.Pp_MLast.Ploc.pp_loc_verbose := true ;;
Pa_ppx_runtime.Exceptions.Ploc.pp_loc_verbose := true ;;
Pa_ppx_runtime_fat.Exceptions.Ploc.pp_loc_verbose := true ;;

let equal_f = Float.equal_tol ~eps:1e-4
let compare_f = Float.compare_tol ~eps:1e-4
let cmp_quat q1 q2 = (V4.equal_f equal_f q1 q2) || (V4.equal_f equal_f q1 (V4.neg q2))
let printer_quat q = Fmt.(str "%a" Quat.pp q)
let cmp_m3 = M3.equal_f equal_f
let printer_m3 m = Fmt.(str "%a" M3.pp m)
let cmp_v3 = V3.equal_f equal_f
let printer_v3 m = Fmt.(str "%a" V3.pp m)

let m3_rotation_matrix angle axis =
  let open Quat in
  let direction = match axis with
      X -> V3.ox
    | Y -> V3.oy
    | Z -> V3.oz in
  M3.rot3_axis direction angle

let expected_m3_rotation_matrix angle axis =
  let open Quat in
  let cosa = cos angle in
  let sina = sin angle in
  match axis with
    X ->
     M3.of_rows
       (V3.v 1. 0. 0.)
       (V3.v 0. cosa (-. sina))
       (V3.v 0. sina cosa)
  | Y ->
     M3.of_rows
       (V3.v cosa 0. sina)
       (V3.v 0. 1. 0.)
       (V3.v (-. sina) 0. cosa)
  | Z -> 
     M3.of_rows
       (V3.v cosa (-. sina) 0.)
       (V3.v sina cosa 0.)
       (V3.v 0. 0. 1.)

let m3_tests = "m3 tests" >:::
  [
    (* check that rotation works like we think *)
    "simple" >:: (fun _ ->
      let cmp = cmp_m3 in
      let printer = printer_m3 in
      let open Angles in
      for i = 0 to 359 do
        let msg = Fmt.(str "X %d degrees" i) in
        let i = d2r (float_of_int i) in
        assert_equal ~msg ~cmp ~printer (expected_m3_rotation_matrix i X) (m3_rotation_matrix i X)
      done ;
      for i = 0 to 359 do
        let msg = Fmt.(str "Y %d degrees" i) in
        let i = d2r (float_of_int i) in
        assert_equal ~msg ~cmp ~printer (expected_m3_rotation_matrix i Y) (m3_rotation_matrix i Y)
      done ;
      for i = 0 to 359 do
        let msg = Fmt.(str "Z %d degrees" i) in
        let i = d2r (float_of_int i) in
        assert_equal ~msg ~cmp ~printer (expected_m3_rotation_matrix i Z) (m3_rotation_matrix i Z)
      done
    )
  (* check that the action of a simple axis rotation is what we think *)
  ; "action" >:: (fun _ ->
      let cmp = cmp_v3 in
      let printer = printer_v3 in
      let open Angles in
      for i = 0 to 359 do
        let msg = Fmt.(str "X %d degrees" i) in
        let i = d2r (float_of_int i) in
        let cosi = cos i in
        let sini = sin i in
        let m = m3_rotation_matrix i X in
        assert_equal ~msg ~cmp ~printer V3.ox (V3.ltr m V3.ox)
        ; assert_equal ~msg ~cmp ~printer (V3.v 0. cosi sini) (V3.ltr m V3.oy)
        ; assert_equal ~msg ~cmp ~printer (V3.v 0. (-. sini) cosi) (V3.ltr m V3.oz)
      done ;
      for i = 0 to 359 do
        let msg = Fmt.(str "Y %d degrees" i) in
        let i = d2r (float_of_int i) in
        let cosi = cos i in
        let sini = sin i in
        let m = m3_rotation_matrix i Y in
        assert_equal ~msg ~cmp ~printer V3.oy (V3.ltr m V3.oy)
        ; assert_equal ~msg ~cmp ~printer (V3.v sini 0. cosi) (V3.ltr m V3.oz)
        ; assert_equal ~msg ~cmp ~printer (V3.v cosi 0. (-. sini)) (V3.ltr m V3.ox)
      done ;
      for i = 0 to 359 do
        let msg = Fmt.(str "Z %d degrees" i) in
        let i = d2r (float_of_int i) in
        let cosi = cos i in
        let sini = sin i in
        let m = m3_rotation_matrix i Z in
        assert_equal ~msg ~cmp ~printer V3.oz (V3.ltr m V3.oz)
        ; assert_equal ~msg ~cmp ~printer (V3.v (-. sini) cosi 0.) (V3.ltr m V3.oy)
        ; assert_equal ~msg ~cmp ~printer (V3.v cosi sini 0.) (V3.ltr m V3.ox)
      done ;
    )
  (* check that an M3 rotation and a Quaternion rotation achieve the same effect *)
  ; "M3=Quat rotation" >:: (fun _ ->
      let cmp = cmp_v3 in
      let printer = printer_v3 in
      let open Angles in
      for i = 0 to 359 do
        let msg = Fmt.(str "X %d degrees" i) in
        let i = d2r (float_of_int i) in
        let cosi = cos i in
        let sini = sin i in
        let m = m3_rotation_matrix i X in
        let q = Quat.of_rotation X i in
        assert_equal ~msg ~cmp ~printer (V3.ltr m V3.ox) (Quat.apply3 q V3.ox)
        ; assert_equal ~msg ~cmp ~printer (V3.ltr m V3.oy) (Quat.apply3 q V3.oy)
        ; assert_equal ~msg ~cmp ~printer (V3.ltr m V3.oz) (Quat.apply3 q V3.oz)
      done ;
      for i = 0 to 359 do
        let msg = Fmt.(str "Y %d degrees" i) in
        let i = d2r (float_of_int i) in
        let cosi = cos i in
        let sini = sin i in
        let m = m3_rotation_matrix i Y in
        assert_equal ~msg ~cmp ~printer V3.oy (V3.ltr m V3.oy)
        ; assert_equal ~msg ~cmp ~printer (V3.v sini 0. cosi) (V3.ltr m V3.oz)
        ; assert_equal ~msg ~cmp ~printer (V3.v cosi 0. (-. sini)) (V3.ltr m V3.ox)
      done ;
      for i = 0 to 359 do
        let msg = Fmt.(str "Z %d degrees" i) in
        let i = d2r (float_of_int i) in
        let cosi = cos i in
        let sini = sin i in
        let m = m3_rotation_matrix i Z in
        assert_equal ~msg ~cmp ~printer V3.oz (V3.ltr m V3.oz)
        ; assert_equal ~msg ~cmp ~printer (V3.v (-. sini) cosi 0.) (V3.ltr m V3.oy)
        ; assert_equal ~msg ~cmp ~printer (V3.v cosi sini 0.) (V3.ltr m V3.ox)
      done ;
    )

]
;;

(** More tests 

    (1) test action of M3 on vectors

    (2) check that action of Quat and M3 on vectors is the same

 *)




let rnd_array = [| 0.5 ; 0.8 ; 0.9 ; -0.3 |]
let quat_unnormalized = Quat.v rnd_array.(1) rnd_array.(2) rnd_array.(3) rnd_array.(0) 
let axes = Quat.[| X ; Y ; Z |]
let rnd = [| -0.92545003 ; -2.19985357 ; 6.01761209 |]
let idx = [| 0; 2; 1 |]
let mat1 =
  M3.mul
    (m3_rotation_matrix rnd.(0)  axes.(idx.(0)))
    (M3.mul
       (m3_rotation_matrix rnd.(1) axes.(idx.(1)))
       (m3_rotation_matrix rnd.(2) axes.(idx.(2))))
let quat = Quat.of_euler [X;Z;Y] (Array.to_list rnd)
let mat2 = Quat.to_m3 quat

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

let zyz_to_quat_test (zyz, explicit_q) =
  let open Quat in
  let expected_q = of_explicit explicit_q in
  let name = "zyz->quat:"^(show_explicit_t explicit_q) in
  [name >:: (fun _ ->
    assert_equal ~msg:name ~cmp:cmp_quat ~printer:printer_quat
       expected_q (Quat.of_zyz zyz))]

let twopi = 2. *. Float.pi
let rec norm_angle a =
  if compare_f a 0. < 0 then norm_angle (a +. twopi)
  else if compare_f twopi a <= 0 then norm_angle (a -. twopi)
  else a
  
let cmp_zyz a b = Angles.ZYZ.cmp_tol ~eps:1e-4 a b

let printer_zyz = [%show: Angles.ZYZ.t]

let quat_to_zyz_test (expected_angles, explicit_q) =
  let open Quat in
  let q = of_explicit explicit_q in
  let name = "quat->zyz:"^(show_explicit_t explicit_q) in
  [name >:: (fun _ ->
    assert_equal ~msg:name ~cmp:cmp_zyz ~printer:printer_zyz
       expected_angles (Quat.to_zyz q))]

let zyz_quat_test ((zyz : Angles.ZYZ.t),  q) =
  (zyz_to_quat_test (zyz,  q))
  @(quat_to_zyz_test (zyz,  q))

let zyz_quat_tests = "zyz/quat tests" >:::
let open Quat in
let open Angles.ZYZ in
(
  List.concat_map zyz_to_quat_test [
      ({z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.000000}, {w=1.0;x=0.000000;y=0.000000;z=0.000000})
    ; ({z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.100000}, {w=0.998750;x=0.000000;y=0.000000;z=0.049979})
    ; ({z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.300000}, {w=0.988771;x=0.000000;y=0.000000;z=0.149438})
    ; ({z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.400000}, {w=0.980067;x=0.000000;y=0.000000;z=0.198669})
    ]
)
@(
  List.concat_map quat_to_zyz_test
    Quat.[
    ({z_0 = 0.100000; y_1 = 0.000000; z_2 = 0.000000}, {w=0.998750;x=0.000000;y=0.000000;z=0.049979})
  ; ({z_0 = 0.300000; y_1 = 0.000000; z_2 = 0.000000}, {w=0.988771;x=0.000000;y=0.000000;z=0.149438})
  ; ({z_0 = 0.400000; y_1 = 0.000000; z_2 = 0.000000}, {w=0.980067;x=0.000000;y=0.000000;z=0.198669})
  ]
)
@(
  List.concat_map zyz_quat_test
    Quat.[
    ({z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.000000}, {w=1.000000;x=0.000000;y=0.000000;z=0.000000})
  ; ({z_0 = 0.000000; y_1 = 0.100000; z_2 = 0.000000}, {w=0.998750;x=0.000000;y=0.049979;z=0.000000})
  ; ({z_0 = 0.000000; y_1 = 0.300000; z_2 = 0.000000}, {w=0.988771;x=0.000000;y=0.149438;z=0.000000})
  ; ({z_0 = 0.000000; y_1 = 0.400000; z_2 = 0.300000}, {w=0.969061;x=0.029689;y=0.196438;z=0.146459})
  ; ({z_0 = 0.100000; y_1 = 0.200000; z_2 = 0.300000}, {w=0.975170;x=0.009967;y=0.099335;z=0.197677})
  ; ({z_0 = 0.100000; y_1 = 0.200000; z_2 = 0.400000}, {w=0.964072;x=0.014919;y=0.098712;z=0.246168})
  ]
)
;;


let compose_zyz_test (zyz1, zyz2, expected_zyz) =
  let open Quat in
  let name = "compose-zyz:"^(printer_zyz zyz1)^"*"^(printer_zyz zyz2) in
  [name >:: (fun _ ->
    assert_equal ~msg:name ~cmp:cmp_zyz ~printer:printer_zyz
       expected_zyz (Quat.to_zyz (Quat.mul (Quat.of_zyz zyz1) (Quat.of_zyz zyz2))))]


let compose_tests = "compose zyz tests" >:::
(
  List.concat_map compose_zyz_test
    [
      ({z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.100000}, {z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.000000}, {z_0 = 0.100000; y_1 = 0.000000; z_2 = 0.000000})
    ; ({z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.300000}, {z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.000000}, {z_0 = 0.300000; y_1 = 0.000000; z_2 = 0.000000})
    ; ({z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.400000}, {z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.300000}, {z_0 = 0.700000; y_1 = 0.000000; z_2 = 0.000000})
    ; ({z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.785398}, {z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.000000}, {z_0 = 0.785398; y_1 = 0.000000; z_2 = 0.000000})
    ; ({z_0 = 0.000000; y_1 = 0.000000; z_2 = 1.570796}, {z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.000000}, {z_0 = 1.570796; y_1 = 0.000000; z_2 = 0.000000})
    ; ({z_0 = 0.000000; y_1 = 0.000000; z_2 = 1.570796}, {z_0 = 0.000000; y_1 = 0.000000; z_2 = 4.712389}, {z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.000000})
    ; ({z_0 = 0.000000; y_1 = 0.000000; z_2 = 3.141593}, {z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.000000}, {z_0 = 3.141593; y_1 = 0.000000; z_2 = 0.000000})
    ; ({z_0 = 0.000000; y_1 = 0.000000; z_2 = 3.141593}, {z_0 = 0.000000; y_1 = 0.000000; z_2 = 1.570796}, {z_0 = -1.570796; y_1 = 0.000000; z_2 = 0.000000})
    ; ({z_0 = 0.000000; y_1 = 0.000000; z_2 = 3.141593}, {z_0 = 0.000000; y_1 = 0.000000; z_2 = 3.141593}, {z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.000000})
    ; ({z_0 = 0.000000; y_1 = 0.000000; z_2 = 6.283185}, {z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.000000}, {z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.000000})
    ; ({z_0 = 0.100000; y_1 = 0.000000; z_2 = 0.000000}, {z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.000000}, {z_0 = 0.100000; y_1 = 0.000000; z_2 = 0.000000})
    ; ({z_0 = 0.100000; y_1 = 0.200000; z_2 = 0.300000}, {z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.100000}, {z_0 = 0.100000; y_1 = 0.200000; z_2 = 0.400000})
    ; ({z_0 = 0.200000; y_1 = 0.000000; z_2 = 0.000000}, {z_0 = 0.100000; y_1 = 0.000000; z_2 = 0.000000}, {z_0 = 0.300000; y_1 = 0.000000; z_2 = 0.000000})
    ; ({z_0 = 1.570796; y_1 = 0.000000; z_2 = 0.785398}, {z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.000000}, {z_0 = 2.356194; y_1 = 0.000000; z_2 = 0.000000})
    ; ({z_0 = 1.570796; y_1 = 0.000000; z_2 = 3.141593}, {z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.000000}, {z_0 = -1.570796; y_1 = 0.000000; z_2 = 0.000000})
    ; ({z_0 = 1.570796; y_1 = 0.000000; z_2 = 3.141593}, {z_0 = 0.000000; y_1 = 0.000000; z_2 = 6.283185}, {z_0 = -1.570796; y_1 = 0.000000; z_2 = 0.000000})
    ; ({z_0 = 1.570796; y_1 = 0.000000; z_2 = 3.141593}, {z_0 = 1.570796; y_1 = 0.000000; z_2 = 3.141593}, {z_0 = 3.141593; y_1 = 0.000000; z_2 = 0.000000})
    ; ({z_0 = 1.570796; y_1 = 1.047198; z_2 = 0.785398}, {z_0 = 0.000000; y_1 = 0.000000; z_2 = 0.000000}, {z_0 = 1.570796; y_1 = 1.047198; z_2 = 0.785398})

    ]
)
;;


(* Run the tests in test suite *)
let _ =
if not !Sys.interactive then
  run_test_tt_main ("all_tests" >::: [
        m3_tests
      ; simple_tests
(*
      ; zyz_quat_tests
      ; compose_tests
 *)
    ])
;;
