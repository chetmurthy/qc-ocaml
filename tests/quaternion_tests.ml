(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open OUnit2
open Qc_quat

Pa_ppx_base.Pp_MLast.Ploc.pp_loc_verbose := true ;;
Pa_ppx_runtime.Exceptions.Ploc.pp_loc_verbose := true ;;
Pa_ppx_runtime_fat.Exceptions.Ploc.pp_loc_verbose := true ;;

let simple_tests = "simple tests" >:::
[
  "simple" >:: (fun _ ->
    ()
  )
]
;;


(* Run the tests in test suite *)
let _ =
if not !Sys.interactive then
  run_test_tt_main ("all_tests" >::: [
        simple_tests
    ])
;;
