(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open OUnit2
open Qasmlex
open Qasmsyntax
open Qasmparser

let lexer_tests = "lexer tests" >:::
  [
    "header" >::
      (fun ctxt ->
        let ll = make_lexer {|OPENQASM 2.0;
// argle bargle
|} in
        assert_equal (list_of_stream ll) 
          [("", Qasmsyntax.T_OPENQASM "2.0");
           ("// argle bargle\n", Qasmsyntax.T_EOF)]
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
          [("", Qasmsyntax.T_OPENQASM "2.0"); ("", Qasmsyntax.T_INCLUDE "qelib1.inc");
           ("", Qasmsyntax.T_QREG); ("", Qasmsyntax.T_ID "q");
           ("", Qasmsyntax.T_LBRACKET); ("", Qasmsyntax.T_INTEGER 2);
           ("", Qasmsyntax.T_RBRACKET); ("", Qasmsyntax.T_SEMICOLON);
           ("", Qasmsyntax.T_ID "h"); ("", Qasmsyntax.T_ID "q");
           ("", Qasmsyntax.T_LBRACKET); ("", Qasmsyntax.T_INTEGER 0);
           ("", Qasmsyntax.T_RBRACKET); ("", Qasmsyntax.T_SEMICOLON);
           ("", Qasmsyntax.T_CX); ("", Qasmsyntax.T_ID "q");
           ("", Qasmsyntax.T_LBRACKET); ("", Qasmsyntax.T_INTEGER 0);
           ("", Qasmsyntax.T_RBRACKET); ("", Qasmsyntax.T_COMMA);
           ("", Qasmsyntax.T_ID "q"); ("", Qasmsyntax.T_LBRACKET);
           ("", Qasmsyntax.T_INTEGER 1); ("", Qasmsyntax.T_RBRACKET);
           ("", Qasmsyntax.T_SEMICOLON)]
      ) ;
    "qasm body" >::
      (fun ctxt ->
        let ll = make_body_lexer {|
// argle
qreg q[2];

h q[// bargle
0];
|} in
        assert_equal (list_of_stream ll)
          [("// argle\n", Qasmsyntax.T_QREG); ("", Qasmsyntax.T_ID "q");
           ("", Qasmsyntax.T_LBRACKET); ("", Qasmsyntax.T_INTEGER 2);
           ("", Qasmsyntax.T_RBRACKET); ("", Qasmsyntax.T_SEMICOLON);
           ("", Qasmsyntax.T_ID "h"); ("", Qasmsyntax.T_ID "q");
           ("", Qasmsyntax.T_LBRACKET); ("// bargle\n", Qasmsyntax.T_INTEGER 0);
           ("", Qasmsyntax.T_RBRACKET); ("", Qasmsyntax.T_SEMICOLON)]
      ) ;
  ]

let test_parse_expr (name, txt, ast) =
  name >:: (fun ctx ->
    let ll = make_body_lexer txt in
      assert_equal ast (expr ll)    
  )

let expr_parser_tests = "expr parser tests" >:::
  (List.map test_parse_expr 
     [
       ("nnint", "1", Ast.NNINT 1);
       ("real 0", "0.e+0", Ast.REAL "0.e+0");
       ("id", "x", Ast.ID "x");
       ("uminus", "-x", Ast.UMINUS (Ast.ID "x")) ;
     ]
  )

let parser_tests = "parser tests" >:::
  [
  ]

(* Run the tests in test suite *)
let _ = 
  run_test_tt_main ("all_tests" >::: [ lexer_tests; expr_parser_tests; parser_tests ])
;;
