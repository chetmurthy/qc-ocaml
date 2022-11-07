(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open OUnit2
open Asttools
open Pa_ppx_utils
open Pa_ppx_testutils
open Coll
open Std
open Misc_functions
open Qc_misc
open Qc
open Qc_latex
module Ops = Qlam_ops
open Test_helpers

open Rresult.R

let latex_qasm s0 =
  let (env,instrs) = with_include_path ~path:["testdata"] Qasm2.of_string s0 in
  let (envitems, qc) = Qlam.Prog.of_qasm2 (env, instrs) in
  let (genv0, (envitems, qc)) = Ops.Standard.program ~env0 (envitems, qc) in
  Ops.Latex.latex genv0 ~env0 (envitems, qc)
;;

let latex_qasm_file s0 =
  let (env,instrs) = with_include_path ~path:["testdata"] Qasm2.of_file s0 in
  let (envitems, qc) = Qlam.Prog.of_qasm2 (env, instrs) in
  let (genv0, (envitems, qc)) = Ops.Standard.program ~env0:env0 (envitems, qc) in
  snd (Ops.Latex.latex genv0 ~env0 (envitems, qc))
;;


let latex_tests = "Latex tests" >:::
[
  "bell" >:: (fun _ ->
    let open Qc_latex.Matrix in
    let printer x = Fmt.(str "%a" pp x) in
    let open Qc_latex.ME in
    assert_equal ~printer
      (ofList [[NGHOST "q_0 :  "; LSTICK (Some "q_0 :  "); QW; GATE {|\mathrm{h}|}; CTRL 1; QW; QW];
               [NGHOST "q_1 :  "; LSTICK (Some "q_1 :  "); QW; QW; TARG; QW; QW]])
      (latex_qasm_file "testdata/bell2.qasm")
  )
]
;;

let latex_matrix m =
  let open Matrix in
  let open Exec in
  let* rvopt = Exec.latex ~display:false ~preserve:true (tolatex m) in
  Ok (rvopt |> Option.map snd)

let program_to_latex genv0 p =
  p |> Ops.Latex.latex genv0 ~env0 |> snd |> Matrix.tolatex

let printer x = Fmt.(str "%a" (Rresult.R.pp ~ok:string ~error:pp_msg) x)

let simple_tests = "simple tests" >:::
[
  "three-qubits" >:: (fun _ ->
    let open Matrix in
    let m = ofList
              ME.[[NGHOST "q_0"; LSTICK (Some "q_0"); QW; QW];
     [NGHOST "q_1"; LSTICK (Some "q_1"); QW; QW];
     [NGHOST "q_2"; LSTICK (Some "q_2"); QW; QW]] in
    assert_equal ~printer
      (Bos.OS.File.read (Fpath.v "testdata/latex/three-qubits.tex")) (Ok (tolatex m))
  )
; "classical-two-in-one" >:: (fun _ ->
    let open Matrix in
    let m = ofList
              ME.[[NGHOST "q_0"; LSTICK(Some "q_0");QW;METER;QW;QW;QW];
               [NGHOST"q_1";LSTICK (Some "q_1");QW;QW;METER;QW;QW];
               [NGHOST{|\mathrm{{c} :  }|};LSTICK(Some {|\mathrm{{c} :  }|});CWIDTH 2;DSTICK("0", -2);DSTICK("1", -1);CW;CW]] in
    assert_equal ~printer
      (Bos.OS.File.read (Fpath.v "testdata/latex/classical-two-in-one.tex")) (Ok (tolatex m))
  )
; "classical-two-separate-bits" >:: (fun _ ->
    let open Matrix in
    let m = ofList
              ME.[[NGHOST "q_0"; LSTICK(Some "q_0");QW;METER;QW;QW;QW];
 [NGHOST"q_1";LSTICK (Some "q_1");QW;QW;METER;QW;QW];
 [NGHOST{|\mathrm{{c} :  }|};LSTICK(Some {|\mathrm{{c} :  }|});CWIDTH 1;DSTICK("0", -2);CW;CW;CW];
 [NGHOST{|\mathrm{{c} :  }|};LSTICK(Some {|\mathrm{{c} :  }|});CWIDTH 1;CW;DSTICK("1", -2);CW;CW]] in
    assert_equal ~printer
      (Bos.OS.File.read (Fpath.v "testdata/latex/classical-two-separate-bits.tex")) (Ok (tolatex m))
  )
; "ghz-bv.qasm" >:: (fun _ ->
  let p0 = read_tolam "testdata/ghz-bv.qasm" in
  let (genv0, p1) = Ops.Standard.program ~env0 p0 in
    assert_equal ~printer
      (Bos.OS.File.read (Fpath.v "testdata/latex/ghz-bv.tex")) (Ok (program_to_latex genv0 p1))
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
        latex_tests
      ; simple_tests
    ])
;;
