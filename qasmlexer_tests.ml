(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open OUnit2
open Qasmlex
open Qasmsyntax

let basic_tests = "basic tests" >:::
  [
    "header" >::
      (fun ctxt ->
        let ll = make_lexer {|OPENQASM 2.0;
// argle bargle
|} in
        assert_equal (list_of_stream ll) [(T_OPENQASM "2.0", "// argle bargle\n")]
      )
  ]

(* Run the tests in test suite *)
let _ = 
  run_test_tt_main ("all_tests" >::: [ basic_tests ])
;;
