(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open OUnit2
open Pa_ppx_utils
open Coll
open Std
open Misc_functions
open Qasmlex
open Qasmsyntax
open Qasmparser
open Qasmpp
open Qasmdag0

let size_tests = "size tests" >:::
  [
    "highmem parse" >:
      (TestCase(Long,
                fun cxt ->
                let rv = full_parse_from_file ~path:["testdata"] PA.mainprogram "testdata/highmem.qasm" in
                ())
      ) ;

    "highmem typecheck" >:
      (TestCase(Long,
                fun cxt ->
                let (_, pl) = full_parse_from_file ~path:["testdata"] PA.mainprogram "testdata/highmem.qasm" in
                let (envs, pl) = TYCHK.program pl in
                ())
      ) ;

  ]

(* Run the tests in test suite *)
let _ =
if not !Sys.interactive then
  run_test_tt_main ("all_tests" >::: [
        size_tests ;
    ])
;;
