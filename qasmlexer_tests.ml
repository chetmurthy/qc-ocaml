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
      ) ;
    "simple qasm" >::
      (fun ctxt ->
        let ll = make_lexer {|OPENQASM 2.0;
include "qelib1.inc";
qreg q[2];

h q[0];
CX q[0],q[1];
|} in
        assert_equal (list_of_stream ll)
          [(T_OPENQASM "2.0", "");
           (T_INCLUDE "qelib1.inc", "\n"); (T_QREG, "");
           (T_ID "q", ""); (T_LBRACKET, "");
           (T_INTEGER 2, ""); (T_RBRACKET, "");
           (T_SEMICOLON, ""); (T_ID "h", "");
           (T_ID "q", ""); (T_LBRACKET, "");
           (T_INTEGER 0, ""); (T_RBRACKET, "");
           (T_SEMICOLON, ""); (T_CX, "");
           (T_ID "q", ""); (T_LBRACKET, "");
           (T_INTEGER 0, ""); (T_RBRACKET, "");
           (T_COMMA, ""); (T_ID "q", "");
           (T_LBRACKET, ""); (T_INTEGER 1, "");
           (T_RBRACKET, ""); (T_SEMICOLON, "")]
      ) ;
  ]

(* Run the tests in test suite *)
let _ = 
  run_test_tt_main ("all_tests" >::: [ basic_tests ])
;;
