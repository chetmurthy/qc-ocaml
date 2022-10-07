(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open OUnit2
open Pa_ppx_utils
open Pa_ppx_testutils
open Coll
open Std
open Misc_functions
open Qc_misc
open Qasm2_lexer
open Qasm2syntax
open Qasm2_parser
open Qasm_io
open Qasmpp
open Qasmdag0
open Qasm_passes

let matches ~pattern text =
  match Str.search_forward (Str.regexp pattern) text 0 with
    _ -> true
  | exception Not_found -> false

let assert_raises_exn_pattern ~msg pattern f =
  Testutil.assert_raises_exn_pred ~exnmsg:msg
    (function
       Ploc.Exc(_, exn) when matches ~pattern (Printexc.to_string exn) -> true
     | exn when matches ~pattern (Printexc.to_string exn) -> true
     | _ -> false
     )
    f


open Qlam_syntax ;;
open Qlam_parser ;;

open Qasm2syntax.AST ;;

let env0 = Qlam_parser.(with_include_path ~path:["testdata"] qelib_from_file "qelib0.qli") ;;

let roundtrip s0 =
  let (env,instrs) = with_include_path ~path:["testdata"] full_to_ast s0 in
  let s1 = Misc_functions.pp Qasmpp.ASTPP.program instrs in
  let (gates, qc) = (env, instrs) |>  Qconvert.ToLam.program  in
  let s2 = Fmt.(str "%a" SYN.pp (gates, qc)) in
  let instrs' = Qconvert.ToQasm2.program (env0 @ gates, qc) in
  let s3 = Misc_functions.pp Qasmpp.ASTPP.main ("2.0",instrs') in
  [s0;s1; s2; s3]
;;

let roundtrip_file s0 =
  let (env,instrs) = with_include_path ~path:["testdata"] full_to_ast_from_file s0 in
  let s1 = Misc_functions.pp Qasmpp.ASTPP.program instrs in
  let (gates, qc) = (env, instrs) |>  Qconvert.ToLam.program  in
  let s2 = Fmt.(str "%a" SYN.pp (gates, qc)) in
  let instrs' = Qconvert.ToQasm2.program (env0 @ gates, qc) in
  let s3 = Misc_functions.pp Qasmpp.ASTPP.main ("2.0",instrs') in
  [s0;s1; s2; s3]
;;

let tolam_file s0 =
  let (env,instrs) = with_include_path ~path:["testdata"] full_to_ast_from_file s0 in
  let s1 = Misc_functions.pp Qasmpp.ASTPP.program instrs in
  let (gates, qc) = (env, instrs) |>  Qconvert.ToLam.program  in
  let s2 = Fmt.(str "%a" SYN.pp (gates, qc)) in
  [s0;s1; s2]
;;

let printit l =
  Fmt.(pf stderr "%a%!" (list ~sep:(const string "\n================\n") string) l) ;;

let testit (name, f, txt, expect_qlam, expect_qasm2) =
  name >:: (fun ctxt ->
    let revl = txt |> f |> List.rev in
    let qasm2_s = car revl in
    let qlam_s = cadr revl in
    expect_qlam
    |> do_option (fun expect_qlam ->
           assert_equal ~printer:(fun x -> x) expect_qlam qlam_s) ;
    expect_qasm2
    |> do_option (fun expect_qasm2 ->
           assert_equal ~printer:(fun x -> x) expect_qasm2 qasm2_s)
  )

let roundtrip_tests = "roundtrip tests" >:::
let open TYCHK.Env in
let open TYCHK in
let open AST in
(
  (List.map testit [
       ("example", roundtrip_file, "testdata/example.qasm",
        None,
        None) ;
     ]
  )
)


(* Run the tests in test suite *)
let _ =
if not !Sys.interactive then
  run_test_tt_main ("all_tests" >::: [
        roundtrip_tests
    ])
;;
