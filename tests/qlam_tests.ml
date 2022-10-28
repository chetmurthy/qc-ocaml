(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open OUnit2
open Asttools
open Pa_ppx_utils
open Pa_ppx_testutils
open Coll
open Std
open Misc_functions
open Qc_misc
open Qasm_io
open Qasm2_parser
open Qlam_syntax
open Qlam_parser
module TYCHK = Qlam_tychk
module Ops = Qlam_ops

let matches ~pattern text =
  let rex = Pcre.regexp ~flags:[`DOTALL] pattern in
  Pcre.pmatch ~rex text

let assert_raises_exn_pattern ~msg pattern f =
  Testutil.assert_raises_exn_pred ~exnmsg:msg
    (function
       Ploc.Exc(_, exn) when matches ~pattern (Printexc.to_string exn) -> true
     | exn when matches ~pattern (Printexc.to_string exn) -> true
     | _ -> false
     )
    f


let id_tests = "ID tests" >:::
let printer x = Fmt.(str "<:id<%a>>" ID.pp x) in
[
  "1" >:: (fun _ ->
    assert_equal ~printer (ID.mk "a") ("a", -1)
  ; assert_equal ~printer (ID.mk "a'") ("a'", -1)
  ; assert_equal ~printer (ID.mk "a'_") ("a'_", -1)
  )
; "2" >:: (fun _ ->
  let assert_roundtrip x =
    assert_equal ~msg:x (ID.unmk (ID.mk x)) x in
    assert_roundtrip "x"
  ; assert_roundtrip "x'"
  )
]

open Qlam_syntax ;;
open Qlam_parser ;;

open Qasm2syntax.AST ;;

let env0a = Qlam_parser.(with_include_path ~path:["testdata"] qelib_from_file "qelib0.qli") ;;
let env0b = Qlam_parser.(with_include_path ~path:["testdata"] qelib_from_file "machines.qli") ;;
let env0c = Qlam_parser.(with_include_path ~path:["testdata"] qelib_from_file "layouts.qli") ;;
let env0 = env0a @ env0b @ env0c ;;

let roundtrip s0 =
  let (env,instrs) = with_include_path ~path:["testdata"] full_to_ast s0 in
  let s1 = Fmt.(str "%a" Qasmpp.ASTPP.program instrs) in
  let (gates, qc) = (env, instrs) |>  Qconvert.ToLam.program  in
  let s2 = Fmt.(str "%a" PP.top (gates, qc)) in
  let instrs' = Qconvert.ToQasm2.program (env0 @ gates, qc) in
  let s3 = Fmt.(str "%a" Qasmpp.ASTPP.main ("2.0",instrs')) in
  [s0;s1; s2; s3]
;;

let roundtrip_file s0 =
  let (env,instrs) = with_include_path ~path:["testdata"] full_to_ast_from_file s0 in
  let s1 = Fmt.(str "%a" Qasmpp.ASTPP.program instrs) in
  let (gates, qc) = (env, instrs) |>  Qconvert.ToLam.program  in
  let s2 = Fmt.(str "%a" PP.top (gates, qc)) in
  let instrs' = Qconvert.ToQasm2.program (env0 @ gates, qc) in
  let s3 = Fmt.(str "%a" Qasmpp.ASTPP.main ("2.0",instrs')) in
  [s0;s1; s2; s3]
;;

let tolam_file s0 =
  let (env,instrs) = with_include_path ~path:["testdata"] full_to_ast_from_file s0 in
  let s1 = Fmt.(str "%a" Qasmpp.ASTPP.program instrs) in
  let (gates, qc) = (env, instrs) |>  Qconvert.ToLam.program  in
  let s2 = Fmt.(str "%a" PP.top (gates, qc)) in
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
(
  (List.map testit [
       ("example", roundtrip_file, "testdata/example.qasm",
        None,
        None)
     ; ("bell2", roundtrip_file, "testdata/bell2.qasm",
        None,
        None)
     ]
  )
)

let env1 =
  let stmts = with_include_path ~path:["testdata"] PA.include_file "qelib1.inc" in
  let (_, stmts) = Qasm2syntax.TYCHK.program stmts in
  Qconvert.ToLam.env stmts
;;

let parse_tolam s0 =
  let (env,instrs) = with_include_path ~path:["testdata"] full_to_ast s0 in
  (env, instrs) |>  Qconvert.ToLam.program

let read_tolam s0 =
  let (env,instrs) = with_include_path ~path:["testdata"] full_to_ast_from_file s0 in
  (env, instrs) |>  Qconvert.ToLam.program
;;

let tychk_qlam (name, txt, expect) = 
  name >:: (fun ctxt ->
    let printer (n,m) = Fmt.(str "(%d,%d)" n m) in
    let (env, qc) = txt |> Stream.of_string |> parse_qcircuit in
    match expect with
      Left expect ->
       let (_, ty) = TYCHK.program (env0@env1@env, qc) in
      assert_equal ~printer expect ty
    | Right exnpat ->
       assert_raises_exn_pattern ~msg:("should match "^exnpat)
         exnpat
         (fun () -> TYCHK.program (env0@env1@env, qc))
  )

let lower_qlam (name, txt, expect) = 
  name >:: (fun ctxt ->
    let (env, qc) = txt |> Stream.of_string |> parse_qcircuit in
    match expect with
      Left expect ->
       let qc' = Ops.lower_circuit qc in
       let txt = Fmt.(str "%a" PP.qcirc qc') in
       let cmp s1 s2 = (collapse_ws s1) = (collapse_ws s2) in
       let printer = (fun x -> "<<"^x^">>") in
       assert_equal ~cmp ~printer expect txt
    | Right exnpat ->
       assert_raises_exn_pattern ~msg:("should match "^exnpat)
         exnpat
         (fun () -> Ops.lower_circuit qc)
  )
;;

let pp_tolam (name, qasm, qlam) = 
  name >:: (fun ctxt ->
    let (env, qc) = qasm |> parse_tolam in
    let got = Fmt.(str "%a" PP.qcirc qc) in
    let cmp s1 s2 = (collapse_ws s1) = (collapse_ws s2) in
    let printer = (fun x -> "<<"^x^">>") in
    assert_equal ~cmp ~printer qlam got
  )
;;

let pp_qlam (name, txt) = 
  name >:: (fun ctxt ->
    let (env, qc) = txt |> Stream.of_string |> parse_qcircuit in
    let got = Fmt.(str "%a" PP.qcirc qc) in
    let cmp s1 s2 = (collapse_ws s1) = (collapse_ws s2) in
    let printer = (fun x -> "<<"^x^">>") in
    assert_equal ~cmp ~printer txt got
  )
;;

let tychk_qelib (name, txt, expect) = 
  name >:: (fun ctxt ->
    let env = txt |> Stream.of_string |> parse_qelib in
    match expect with
      Left () ->
       ignore (TYCHK.mk_genv (env0@env1@env))

    | Right exnpat ->
       assert_raises_exn_pattern ~msg:("should match "^exnpat)
         exnpat
         (fun () -> TYCHK.mk_genv (env0@env1@env))
  )
;;
let tychk_qasm2_file (name, f, expect) = 
  name >:: (fun ctxt ->
    let (env, qc) = read_tolam f in
    let (_, ty) = TYCHK.program (env0@env, qc) in
    let printer (n,m) = Fmt.(str "(%d,%d)" n m) in
    assert_equal ~printer expect ty
  )
;;

let pp_tests = "pp tests" >:::
(
  (List.map pp_qlam [
       ("pp simple 0", {|
let q1 = h q0 in
()
|})
       ; ("pp simple 1", {|
let p = qubit () in
let q0 = qubit () in
let q1 = h q0 in
()
|})
       ; ("pp explicit 1", {|
let p = qubit #1 () in
let q0 = qubit #2 () in
let q1 = h q0 in
()
|})
     ]
  )
  @(List.map pp_tolam [
        ("parse bell", {|OPENQASM 2.0;
include "qelib1.inc";
qreg q[2];
h q[0];
cx q[0],q[1];
|},{|let q0 = qubit #0 () in
let q1 = qubit #1 () in
let q0 = h q0 in
let (q0, q1) = cx q0 q1 in
(q0, q1)|})
      ]
  )
)
;;


let alpha_equality (name, txt1, txt2, expect) = 
  name >:: (fun ctxt ->
    let (env, qc1) = txt1 |> Stream.of_string |> parse_qcircuit in
    let (env, qc2) = txt2 |> Stream.of_string |> parse_qcircuit in
    let cmp qc1 qc2 = Ops.AlphaEq.circuit qc1 qc2 in
    let printer qc = Fmt.(str "%a" PP.qcirc qc) in
    if expect then
      assert_equal ~msg:"not alpha-equal" ~printer ~cmp qc1 qc2
    else
      assert_bool "should not be alpha-equal" (not (cmp qc1 qc2))
  )
;;


let alpha_equality_tests = "alpha equality tests" >:::
(
    (List.map alpha_equality [
         ("0", {|()|}, {|()|}, true)
       ; ("1", {|(x)|}, {|()|}, false)
       ; ("1'", {|(x)|}, {|(y)|}, false)
       ; ("1''", {|(x)|}, {|(x)|}, true)
       ; ("2", {|let q = qubit () in (q)|},{|let p = qubit () in (p)|}, true)
     ]
  )
)
;;

let tychk_tests = "tychk tests" >:::
let open TYCHK.Env in
let open TYCHK in
(

  (List.map tychk_qlam [
       ("discard", {|
let q0 = qubit() in
let q1 = h q0 in
()
|},
        Right ".*Exc.*q1 not used")
     ; ("qbits not unique", {|
let q0 = qubit #1 () in
let q1 = qubit #1 () in
let q2 = h q0 in
(q1, q2)
|},
        Right "check_unique: qubit.*expressions are not unique")
     ; ("two", {|
let q0 = qubit() in
let q1 = h q0 in
(q0)
|},
        Right ".*Exc.*q0 used more than once")
     ; ("not distinct in let", {|
let q0 = qubit() in
let q1 = qubit() in
let q2 = h q0 and  q2 = h q1 in
(q2)
|},
        Right ".*Exc.*vars in binding MUST be distinct")
     ]
  )@
  (List.map tychk_qasm2_file [
       ("bell2", "testdata/bell2.qasm",
       (2,0))
     ; ("bell2", "testdata/example.qasm",
       (6,6))
     ]
  )@
  (List.map tychk_qelib [
       ("ok gate 0", {|gate pass () q = (q) ;|},
       Left())
     ; ("bad gate 1", {|gate pass () q = (p) ;|},
       Right {|pass.*free qvars.*p.*-1|})
     ; ("ok gate 1", {|gate pass () q : c = (q : c) ;|},
       Left ())
     ; ("ok gate 2", {|gate pass () q : c = (q) ;|},
       Left ())
     ; ("bad gate 2", {|gate pass () q = (q : c) ;|},
       Right {|pass.*free cvars.*c.*-1|})
     ]
  )@
    (List.map lower_qlam [
       ("simple", {|
let q0 = qubit () in
let q1 = h q0 in
()
|},
        Left {|
let q = qubit () in
let q0 = h q in
()
|})
     ]
  )
)
;;


let anorm_qlam (name, txt, expect) = 
  name >:: (fun ctxt ->
    let cmp qc1 qc2 = Ops.AlphaEq.circuit qc1 qc2 in
    let printer qc = Fmt.(str "%a" PP.qcirc qc) in
    let (env, qc) = txt |> Stream.of_string |> parse_qcircuit in
    match expect with
      Left expect ->
       let got_qc = Ops.ANorm.qcirc qc in
       let (_, expect_qc) = expect |> Stream.of_string |> parse_qcircuit in
      assert_equal ~cmp ~printer expect_qc got_qc
    | Right exnpat ->
       assert_raises_exn_pattern ~msg:("should match "^exnpat)
         exnpat
         (fun () -> Ops.ANorm.qcirc qc)
  )

let separate_let_tests = "separate_let tests" >:::
(

  (List.map anorm_qlam [
       ("captures", {|
let x' = let w' = E1' in E2' and y = E3 x' and z = E4 in
    E5
|},
        Right "anorm.*capture")
     ; ("simple", {|
let x' = let w' = E1' in E2' and y = E3 and z = E4 in
    E5
|},
        Left {|
let w' = E1' in
let x' = E2' in
let y = E3 and z = E4 in
    E5
|})
     ]
  )
)
;;

Pa_ppx_base.Pp_MLast.Ploc.pp_loc_verbose := true ;;
Pa_ppx_runtime.Exceptions.Ploc.pp_loc_verbose := true ;;
Pa_ppx_runtime_fat.Exceptions.Ploc.pp_loc_verbose := true ;;

(* Run the tests in test suite *)
let _ =
if not !Sys.interactive then
  run_test_tt_main ("all_tests" >::: [
        id_tests
      ; roundtrip_tests
      ; pp_tests
      ; alpha_equality_tests
      ; separate_let_tests
      ; tychk_tests
    ])
;;
