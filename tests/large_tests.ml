(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open OUnit2
open Pa_ppx_utils
open Coll
open Std
open Misc_functions
open Qasm2_lexer
open Qasm2syntax
open Qasm2_parser
open Qasmpp
open Qasmdag0

let size_tests = "size tests" >:::
  [
    "highmem parse" >:
      (TestCase(Long,
                fun cxt ->
                let rv = with_include_path ~path:["testdata"] (full_parse_from_file PA.mainprogram) "testdata/highmem.qasm" in
                ())
      ) ;

    "highmem typecheck" >:
      (TestCase(Long,
                fun cxt ->
                let (_, pl) = with_include_path ~path:["testdata"] (full_parse_from_file PA.mainprogram) "testdata/highmem.qasm" in
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
