(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open OUnit2
open Asttools
open Pa_ppx_utils
open Pa_ppx_testutils
open Coll
open Std
open Misc_functions
open Qc_misc
open Qc_latex

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

open Rresult.R

let compare_files f1 f2 =
  let open Exec in
  let* contents1 = Bos.OS.File.read f1 in
  let* contents2 = Bos.OS.File.read f2 in
  Ok (contents1 = contents2)

let compare_results r1 r2 =
  match (r1, r2) with
    (Error msg1, Error msg2) -> msg1 = msg2
  | (Ok (Some f1), Ok (Some f2)) -> (compare_files f1 f2) |> Rresult.R.get_ok
  | _ -> false

let latex_matrix m =
  let open Matrix in
  let open Exec in
  let* rvopt = Exec.latex ~display:false ~preserve:true (tolatex m) in
  Ok (rvopt |> Option.map snd)

let matrix_tests = "Matrix tests" >:::
[
  "three-qubits" >:: (fun _ ->
    let open Matrix in
    let printer x = Fmt.(str "%a" (Rresult.R.pp ~ok:string ~error:pp_msg) x) in
    let m = ofList
              ME.[[NGHOST "q_0"; LSTICK (Some "q_0"); QW; QW];
     [NGHOST "q_1"; LSTICK (Some "q_1"); QW; QW];
     [NGHOST "q_2"; LSTICK (Some "q_2"); QW; QW]] in
    assert_equal ~printer
      (Bos.OS.File.read (Fpath.v "testdata/latex/three-qubits.tex")) (Ok (tolatex m))
  )
; "classical-two-in-one" >:: (fun _ ->
    let open Matrix in
    let printer x = Fmt.(str "%a" (Rresult.R.pp ~ok:string ~error:pp_msg) x) in
    let m = ofList
              ME.[[NGHOST "q_0"; LSTICK(Some "q_0");QW;METER;QW;QW;QW];
               [NGHOST"q_1";LSTICK (Some "q_1");QW;QW;METER;QW;QW];
               [NGHOST{|\mathrm{{c} :  }|};LSTICK(Some {|\mathrm{{c} :  }|});CWIDTH 2;DSTICK("0", -2);DSTICK("1", -1);CW;CW]] in
    assert_equal ~printer
      (Bos.OS.File.read (Fpath.v "testdata/latex/classical-two-in-one.tex")) (Ok (tolatex m))
  )
; "classical-two-separate-bits" >:: (fun _ ->
    let open Matrix in
    let printer x = Fmt.(str "%a" (Rresult.R.pp ~ok:string ~error:pp_msg) x) in
    let m = ofList
              ME.[[NGHOST "q_0"; LSTICK(Some "q_0");QW;METER;QW;QW;QW];
 [NGHOST"q_1";LSTICK (Some "q_1");QW;QW;METER;QW;QW];
 [NGHOST{|\mathrm{{c} :  }|};LSTICK(Some {|\mathrm{{c} :  }|});CWIDTH 1;DSTICK("0", -2);CW;CW;CW];
 [NGHOST{|\mathrm{{c} :  }|};LSTICK(Some {|\mathrm{{c} :  }|});CWIDTH 1;CW;DSTICK("1", -2);CW;CW]] in
    assert_equal ~printer
      (Bos.OS.File.read (Fpath.v "testdata/latex/classical-two-separate-bits.tex")) (Ok (tolatex m))
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
        matrix_tests
    ])
;;
