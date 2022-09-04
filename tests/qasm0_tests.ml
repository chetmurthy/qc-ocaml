(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open OUnit2
open Pa_ppx_utils
open Coll
open Std
open Misc_functions
open Qasm0_lexer

let misc_tests = "misc tests" >:::
  [
    "simple" >::
      (fun ctxt ->
        ()
      );
  ]

let lexer_tests = "lexer tests" >:::
  [
    "simple" >::
      (fun ctxt ->
        ()
      )
  ]


(* Run the tests in test suite *)
let _ =
if not !Sys.interactive then
  run_test_tt_main ("all_tests" >::: [
        misc_tests ; lexer_tests
    ])
;;
