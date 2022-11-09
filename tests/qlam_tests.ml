(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open OUnit2
open Asttools
open Pa_ppx_utils
open Pa_ppx_testutils
open Coll
open Std
open Misc_functions
open Qc_misc
open Qlam_syntax
open Qc
module Ops = Qlam_ops
open Test_helpers


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

open Qasm2syntax.AST ;;

let roundtrip_program s0 =
  let (env,instrs) = with_include_path ~path:["testdata"] Qasm2.of_string s0 in
  let s1 = Fmt.(str "%a" Qasm2.pp_hum instrs) in
  let (envitems, qc) = Qlam.Prog.of_qasm2 (env, instrs) in
  let s2 = Fmt.(str "%a" Qlam.Prog.pp_hum (envitems, qc)) in
  let (genv0, (envitems, qc)) = Ops.Standard.program ~env0 (envitems, qc) in
  let instrs' = Qlam.Prog.to_qasm2 ~env0 (envitems, qc) in
  let s3 = Fmt.(str "%a" Qasm2.pp_hum instrs') in
  [s0;s1; s2; s3]
;;

let roundtrip_program_file s0 =
  let (env,instrs) = with_include_path ~path:["testdata"] Qasm2.of_file s0 in
  let s1 = Fmt.(str "%a" Qasm2.pp_hum instrs) in
  let (envitems, qc) = Qlam.Prog.of_qasm2 (env, instrs) in
  let s2 = Fmt.(str "%a" Qlam.Prog.pp_hum (envitems, qc)) in
  let (genv0, (envitems, qc)) = Ops.Standard.program ~env0 (envitems, qc) in
  let instrs' = Qlam.Prog.to_qasm2 ~env0 (envitems, qc) in
  let s3 = Fmt.(str "%a" Qasm2.pp_hum instrs') in
  [s0;s1; s2; s3]
;;

let tolam_file s0 =
  let (env,instrs) = with_include_path ~path:["testdata"] Qasm2.of_file s0 in
  let s1 = Fmt.(str "%a" Qasm2.pp_hum instrs) in
  let (gates, qc) = Qlam.Prog.of_qasm2 (env, instrs) in
  let s2 = Fmt.(str "%a" Qlam.Prog.pp_hum (gates, qc)) in
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
let open Ops.TYCHK.Env in
let open Ops.TYCHK in
(
  (List.map testit [
       ("example", roundtrip_program_file, "testdata/example.qasm",
        None,
        None)
     ; ("bell2", roundtrip_program_file, "testdata/bell2.qasm",
        None,
        None)
     ]
  )
)

let env1 =
  let stmts = with_include_path ~path:["testdata"] Qasm2.lib_of_file "qelib1.inc" in
  let (_, stmts) = Qasm2syntax.TYCHK.program stmts in
  Qlam.Environ.of_qasm2 stmts
;;

let tychk_qlam (name, txt, expect) = 
  name >:: (fun ctxt ->
    let printer (n,m) = Fmt.(str "(%d,%d)" n m) in
    let (env, qc) = txt |> Qlam.Prog.of_string in
    match expect with
      Left expect ->
       let (_, ty) = Ops.TYCHK.program ~env0:(env0@env1) (env, qc) in
      assert_equal ~printer expect ty
    | Right exnpat ->
       assert_raises_exn_pattern ~msg:("should match "^exnpat)
         exnpat
         (fun () -> Ops.TYCHK.program ~env0:(env0@env1) (env, qc))
  )

let lower_qlam (name, txt, expect) = 
  name >:: (fun ctxt ->
    let (env, qc) = txt |> Qlam.Prog.of_string in
    match expect with
      Left expect ->
       let qc' = Ops.lower_circuit qc in
       let txt = Fmt.(str "%a" Qlam.Circ.pp_hum qc') in
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
    let got = Fmt.(str "%a" Qlam.Circ.pp_hum qc) in
    let cmp s1 s2 = (collapse_ws s1) = (collapse_ws s2) in
    let printer = (fun x -> "<<"^x^">>") in
    assert_equal ~cmp ~printer qlam got
  )
;;

let pp_qlam (name, txt) = 
  name >:: (fun ctxt ->
    let (env, qc) = txt |> Qlam.Prog.of_string in
    let got = Fmt.(str "%a" Qlam.Circ.pp_hum qc) in
    let cmp s1 s2 = (collapse_ws s1) = (collapse_ws s2) in
    let printer = (fun x -> "<<"^x^">>") in
    assert_equal ~cmp ~printer txt got
  )
;;

let tychk_qelib (name, txt, expect) = 
  name >:: (fun ctxt ->
    let env = txt |> Qlam.Environ.of_string in
    match expect with
      Left () ->
       ignore (Ops.TYCHK.environ ~env0:(env0@env1) env)

    | Right exnpat ->
       assert_raises_exn_pattern ~msg:("should match "^exnpat)
         exnpat
         (fun () -> Ops.TYCHK.environ ~env0:(env0@env1) env)
  )
;;
let tychk_qasm2_file (name, f, expect) = 
  name >:: (fun ctxt ->
    let (env, qc) = read_tolam f in
    let (_, ty) = Ops.TYCHK.program ~env0 (env, qc) in
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
    let (env, qc1) = txt1 |> Qlam.Prog.of_string in
    let (env, qc2) = txt2 |> Qlam.Prog.of_string in
    let cmp qc1 qc2 = Ops.AlphaEq.circuit qc1 qc2 in
    let printer qc = Fmt.(str "%a" Qlam.Circ.pp_hum qc) in
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
let open Ops.TYCHK.Env in
let open Ops.TYCHK in
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

let anorm_qcirc (name, txt, expect) = 
  name >:: (fun ctxt ->
    let cmp qc1 qc2 = Ops.AlphaEq.circuit qc1 qc2 in
    let printer qc = Fmt.(str "%a" Qlam.Circ.pp_hum qc) in
    let (env, qc) = txt |> Qlam.Prog.of_string in
    match expect with
      Left expect ->
       let got_qc = Ops.ANorm.qcircuit qc in
       let (_, expect_qc) = expect |> Qlam.Prog.of_string in
      assert_equal ~cmp ~printer expect_qc got_qc
    | Right exnpat ->
       assert_raises_exn_pattern ~msg:("should match "^exnpat)
         exnpat
         (fun () -> Ops.ANorm.qcircuit qc)
  )

let anorm_gate (name, txt, expect) = 
  name >:: (fun ctxt ->
    let cmp qelib1 qelib2 = SYN.equal_environ_t qelib1 qelib2 in
    let printer qelib = Fmt.(str "%a" Qlam.Environ.pp_hum qelib) in
    let qelib = txt |> Qlam.Environ.of_string in
    match expect with
      Left expect ->
       let (got_qelib, _) = Ops.ANorm.program (qelib, QWIRES(Ploc.dummy, [], [])) in
       let expect_qelib = expect |> Qlam.Environ.of_string in
      assert_equal ~cmp ~printer expect_qelib got_qelib
    | Right exnpat ->
       assert_raises_exn_pattern ~msg:("should match "^exnpat)
         exnpat
         (fun () -> Ops.ANorm.program (qelib, QWIRES(Ploc.dummy, [], [])))
  )

let separate_let_tests = "separate_let tests" >:::
(

  (List.map anorm_qcirc [
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

  @(List.map anorm_gate [
       ("simple gate", {|
gate rzz(theta) a b =
  let (a1,b1) = cx q b in
  let b2 = let q = U(0,0,theta) b1 in q in
  let (a3,b3) = cx a1 b2 in
  (a3,b3)
;
|},
        Left {|
gate rzz(theta) a b =
  let (a1,b1) = cx q b in
  let q = U(0,0,theta) b1 in
  let b2 = q in
  let (a3,b3) = cx a1 b2 in
  (a3,b3)
;
|})
     ]
  )
)
;;

let nnorm_qcirc (name, txt, expect) = 
  name >:: (fun ctxt ->
    let cmp qc1 qc2 = Ops.AlphaEq.circuit qc1 qc2 in
    let printer qc = Fmt.(str "%a" Qlam.Circ.pp_hum qc) in
    let (env, qc) = txt |> Qlam.Prog.of_string in
    match expect with
      Left expect ->
       let got_qc = Ops.NameNorm.qcircuit qc in
       let (_, expect_qc) = expect |> Qlam.Prog.of_string in
      assert_equal ~cmp ~printer expect_qc got_qc
    | Right exnpat ->
       assert_raises_exn_pattern ~msg:("should match "^exnpat)
         exnpat
         (fun () -> Ops.NameNorm.qcircuit qc)
  )

let name_norm_tests = "name-norm tests" >:::
(
  (List.map nnorm_qcirc [
       ("simple", {|
let (x,y) = (y,x) in
(x,y)
|},
        Left {|
(y,x)
|})
     ; ("bug1", {|
let q60 = qubit #0 () in
              let q61 = qubit #1 () in
              let q62 = qubit #2 () in
              let q63 = qubit #3 () in
              let q64 = qubit #4 () in
              let q65 = qubit #5 () in
              let (q67, q66) = cx q62 q63 in
              let q69 = h q61 and q68 = h q60 in
              let (q71, q70) = CX q68 q69 in
              let q73 = h q70 and q72 = h q71 in
              let (q75, q74) = (q73, q72) in
              let (q77, q76) = SWAP q75 q67 in
              let (q79, q78) = SWAP q76 q66 in
              let (q81, q80) = cx q78 q64 in
              let (q83, q82) = SWAP q65 q74 in
              let (q85, q84) = SWAP q82 q78 in
              let (q87, q86) = SWAP q84 q77 in
              let (q89, q88) = cx q86 q79 in
              let q91 = h q87 and q90 = h q88 in
              let (q93, q92) = CX q90 q91 in
              let q95 = h q92 and q94 = h q93 in
              let (q97, q96) = (q95, q94) in
              (q83, q81, q97, q96, q80, q89)|},
        Left {|
let q60 = qubit #0 () in
              let q61 = qubit #1 () in
              let q62 = qubit #2 () in
              let q63 = qubit #3 () in
              let q64 = qubit #4 () in
              let q65 = qubit #5 () in
              let (q67, q66) = cx q62 q63 in
              let q69 = h q61 and q68 = h q60 in
              let (q71, q70) = CX q68 q69 in
              let q73 = h q70 and q72 = h q71 in
              let (q77, q76) = SWAP q73 q67 in
              let (q79, q78) = SWAP q76 q66 in
              let (q81, q80) = cx q78 q64 in
              let (q83, q82) = SWAP q65 q72 in
              let (q85, q84) = SWAP q82 q78 in
              let (q87, q86) = SWAP q84 q77 in
              let (q89, q88) = cx q86 q79 in
              let q91 = h q87 and q90 = h q88 in
              let (q93, q92) = CX q90 q91 in
              let q95 = h q92 and q94 = h q93 in
              (q83, q81, q95, q94, q80, q89)
|})
     ; ("bug1-simplified", {|
              let q61 = qubit #1 () in
              let (q75) = (q61) in
              let x1 = h q75 in
                 (x1)
|},
        Left {|
              let q61 = qubit #1 () in
              let x1 = h q61 in
                 (x1)
|})
     ]
  )
)
;;

let cmp s1 s2 = (collapse_ws s1) = (collapse_ws s2) ;;
let printer s = s ;;

let basic_swap_tests = "basic_swap tests" >:::
[
  "zulehner_4a.qasm" >:: (fun _ ->
    let p0 = read_tolam "testdata/zulehner_4a.qasm" in
    let (genv0, p1) = Ops.Standard.program ~env0:env0 p0 in
    let cm = GEnv.find_mach genv0 (ID.mk"ibm_qx3") in
    let l = {|
[
#0 : <physical 0>,
#1 : <physical 1>,
#2 : <physical 2>,
#3 : <physical 3>,
#4 : <physical 14>,
#5 : <physical 15>
]
|} |> Layout.of_string in
    let p2 = Ops.BasicSwap.basic_swap genv0 ~env0 ~coupling_map:cm ~layout:(Ops.LO.mk l) p1 in
    let _ = Ops.CheckLayout.check_layout genv0 ~env0 ~coupling_map:cm ~layout:(Ops.LO.mk l) p2 in
    let got = Fmt.(str "%a\n%!" Qasm2.pp_hum (Qlam.Prog.to_qasm2 ~env0 p2)) in
    assert_equal ~cmp ~printer {|
OPENQASM 2.0;
include "qelib1.inc";
qreg q[6];
cx q[2], q[3];
h q[1];
h q[0];
CX q[0], q[1];
h q[1];
h q[0];
SWAP q[1], q[0];
SWAP q[0], q[5];
cx q[5], q[4];
SWAP q[0], q[5];
SWAP q[4], q[3];
cx q[5], q[4];
SWAP q[2], q[3];
cx q[3], q[4];
|} got
  )
; "ghz-bv.qasm" >:: (fun _ ->
  let p0 = read_tolam "testdata/ghz-bv.qasm" in
  let (genv0, p1) = Ops.Standard.program ~env0:env0 p0 in
  let cm = GEnv.find_mach genv0 (ID.mk"ibmq_quito") in
  let l = {|
[
#0 : <physical 0>,
#1 : <physical 1>,
#2 : <physical 2>,
#3 : <physical 3>,
#4 : <physical 4>
]
|} |> Layout.of_string in
  let p2 = Ops.BasicSwap.basic_swap genv0 ~env0 ~coupling_map:cm ~layout:(Ops.LO.mk l) p1 in
  let _ = Ops.CheckLayout.check_layout genv0 ~env0 ~coupling_map:cm ~layout:(Ops.LO.mk l) p2 in
  let got = Fmt.(str "%a\n%!" Qasm2.pp_hum (Qlam.Prog.to_qasm2 ~env0 p2)) in
  assert_equal ~cmp ~printer {|
OPENQASM 2.0;
include "qelib1.inc";
qreg q[5];
h q[0];
cx q[0], q[1];
SWAP q[0], q[1];
cx q[1], q[2];
cx q[1], q[3];
SWAP q[1], q[3];
cx q[3], q[4];
|} got
  )
]
;;

let check_layout_tests = "check_layout tests" >:::
[
  "ghz-bv.qasm-no-swap" >:: (fun _ ->
    let p0 = read_tolam "testdata/ghz-bv.qasm" in
    let (genv0, p1) = Ops.Standard.program ~env0:env0 p0 in
    let cm = GEnv.find_mach genv0 (ID.mk"ibmq_quito") in
    let l = {|
[
#0 : <physical 0>,
#1 : <physical 1>,
#2 : <physical 2>,
#3 : <physical 3>,
#4 : <physical 4>
]
         |} |> Layout.of_string in
    assert_raises_exn_pattern ~msg:"should raise Failure(coupling_map)"
      "Failure.*check_binding: CX/cx gate not supported by coupling_map: cx q12 q7"
      (fun () ->
        Ops.CheckLayout.check_layout genv0 ~env0 ~coupling_map:cm ~layout:(Ops.LO.mk l) p1)
  )
]
;;

let cmp s1 s2 = (collapse_ws s1) = (collapse_ws s2) ;;
let printer s = s ;;

let hoist_tests = "hoist tests" >:::
[
  "simple1" >:: (fun _ ->
    let p0 = parse_tolam {|
OPENQASM 2.0;
include "qelib1.inc";
qreg q[4];
cx q[1], q[2];
cx q[0], q[1] ;
cx q[2], q[3] ;
|} in
    let (genv0, (envitems, qc)) = Ops.Standard.program ~env0:env0 p0 in
    let qc = Ops.Hoist.hoist qc in
    let got = Fmt.(str "%a" Qlam.Circ.pp_hum qc) in
    assert_equal ~printer ~cmp {|
let q4 = qubit #0 () and q5 = qubit #1 () and q6 = qubit #2 ()
    and q7 = qubit #3 () in
let (q9, q8) = cx q5 q6 in
let (q11, q10) = cx q4 q9 and (q13, q12) = cx q8 q7 in
(q11, q10, q13, q12)
|} got
  )
; "simple2" >:: (fun _ ->
    let p0 = parse_tolam {|
OPENQASM 2.0;
include "qelib1.inc";
qreg q[4];
cx q[1], q[2];
h q[2] ;
cx q[0], q[1] ;
cx q[2], q[3] ;
|} in
    let (genv0, (envitems, qc)) = Ops.Standard.program ~env0:env0 p0 in
    let qc = Ops.Hoist.hoist qc in
    let got = Fmt.(str "%a" Qlam.Circ.pp_hum qc) in
    assert_equal ~printer ~cmp {|
let q4 = qubit #0 () and q5 = qubit #1 () and q6 = qubit #2 ()
    and q7 = qubit #3 () in
let (q9, q8) = cx q5 q6 in
let q10 = h q8 and (q12, q11) = cx q4 q9 in
let (q14, q13) = cx q10 q7 in
(q12, q11, q14, q13)
|} got
  )
; "simple2-sabre-hoist" >:: (fun _ ->
    let p0 = parse_tolam {|
OPENQASM 2.0;
include "qelib1.inc";
qreg q[4];
cx q[1], q[2];
h q[2] ;
cx q[0], q[1] ;
cx q[2], q[3] ;
|} in
    let (genv0, (envitems, qc)) = Ops.Standard.program ~env0:env0 p0 in
    let qc = Ops.SabreHoist.hoist qc in
    let got = Fmt.(str "%a" Qlam.Circ.pp_hum qc) in
    assert_equal ~printer ~cmp {|
let q4 = qubit #0 () and q5 = qubit #1 () and q6 = qubit #2 ()
    and q7 = qubit #3 () in
let (q9, q8) = cx q5 q6 in
let q10 = h q8 in
let (q12, q11) = cx q4 q9 and (q14, q13) = cx q10 q7 in
(q12, q11, q14, q13)
|} got
  )
; "complex" >:: (fun _ ->
    let p0 = parse_tolam {|
OPENQASM 2.0;
include "qelib1.inc";
qreg q[3];
qreg r[3];
h q;
cx q, r;
creg c[3];
creg d[3];
barrier q;
measure q->c;
measure r->d;
|} in
    let (genv0, (envitems, qc)) = Ops.Standard.program ~env0:env0 p0 in
    let qc = Ops.Hoist.hoist qc in
    let got = Fmt.(str "%a" Qlam.Circ.pp_hum qc) in
    assert_equal ~printer ~cmp {|
let q4 = qubit #0 () and q5 = qubit #1 () and q6 = qubit #2 ()
    and r7 = qubit #3 () and r8 = qubit #4 () and r9 = qubit #5 () in
let q10 = h q4 and q11 = h q5 and q12 = h q6 in
let (q14, r13) = cx q10 r7 and (q16, r15) = cx q11 r8
    and (q18, r17) = cx q12 r9 in
let (q21, q20, q19) = barrier q14 q16 q18 and (r28 : d29) = measure r13
    and (r30 : d31) = measure r15 and (r32 : d33) = measure r17 in
let (q22 : c23) = measure q21 and (q24 : c25) = measure q20
    and (q26 : c27) = measure q19 in
(q22, q24, q26, r28, r30, r32 : c23, c25, c27, d29, d31, d33)
|} got
  )
]
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
      ; name_norm_tests
      ; tychk_tests
      ; basic_swap_tests
      ; check_layout_tests
      ; hoist_tests
    ])
;;
