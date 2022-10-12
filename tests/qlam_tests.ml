(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open OUnit2
open Asttools
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
       let txt = Fmt.(str "%a" SYN.QC.qcirc qc') in
       let cmp s1 s2 = (collapse_ws s1) = (collapse_ws s2) in
       let printer = (fun x -> "<<"^x^">>") in
       assert_equal ~cmp ~printer expect txt
    | Right exnpat ->
       assert_raises_exn_pattern ~msg:("should match "^exnpat)
         exnpat
         (fun () -> Ops.lower_circuit qc)
  )
;;

let pp_qlam (name, txt) = 
  name >:: (fun ctxt ->
    let (env, qc) = txt |> Stream.of_string |> parse_qcircuit in
    let got = Fmt.(str "%a" SYN.QC.qcirc qc) in
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
       ignore (TYCHK.env (env0@env1@env))

    | Right exnpat ->
       assert_raises_exn_pattern ~msg:("should match "^exnpat)
         exnpat
         (fun () -> TYCHK.env (env0@env1@env))
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
let p = qubit() in
let q0 = qubit() in
let q1 = h q0 in
()
|})
     ]
  )
)
;;


let alpha_equality (name, txt1, txt2, expect) = 
  name >:: (fun ctxt ->
    let (env, qc1) = txt1 |> Stream.of_string |> parse_qcircuit in
    let (env, qc2) = txt2 |> Stream.of_string |> parse_qcircuit in
    let cmp qc1 qc2 = Ops.AlphaEq.circuit qc1 qc2 in
    let printer qc = Fmt.(str "%a" SYN.QC.qcirc qc) in
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
let open AST in
(

  (List.map tychk_qlam [
       ("discard", {|
let q0 = qubit() in
let q1 = h q0 in
()
|},
        Right ".*Exc.*q1 not used")
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
let q0 = qubit() in
let q1 = h q0 in
()
|},
        Left {|
let q = qubit() in
let q0 = h q in
()
|})
     ]
  )
)
;;

Pa_ppx_base.Pp_MLast.Ploc.pp_loc_verbose := true ;;
Pa_ppx_runtime_fat.Exceptions.Ploc.pp_loc_verbose := true ;;

(* Run the tests in test suite *)
let _ =
if not !Sys.interactive then
  run_test_tt_main ("all_tests" >::: [
        roundtrip_tests
      ; pp_tests
      ; alpha_equality_tests
      ; tychk_tests
    ])
;;
