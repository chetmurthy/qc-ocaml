(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open OUnit2
open Misc_functions
open Qasmlex
open Qasmsyntax
open Qasmparser
open Qasmpp

let misc_tests = "misc tests" >:::
  [
    "pcre whitespace 1" >::
      (fun ctxt ->
        assert_equal (cleanws "")  ""
      );
    "pcre whitespace 2" >::
      (fun ctxt ->
        assert_equal (cleanws "a")  "a"
      ) ;
    "pcre whitespace 3" >::
      (fun ctxt ->
        assert_equal (cleanws "a b")  "a b"
      ) ;
    "pcre whitespace 4" >::
      (fun ctxt ->
        assert_equal (cleanws "\ta b\t")  "a b"
      ) ;
    "pcre whitespace 5" >::
      (fun ctxt ->
        assert_equal (cleanws "\n")  "\n"
      ) ;
    "pcre whitespace 6" >::
      (fun ctxt ->
        assert_equal (cleanws " \t// argle\n \t")  "// argle\n"
      ) ;
  ]

let extract_tokens ll =
  List.map
    (fun (a, tok) ->
      (TA.comment_string a,
       (TA.startpos a).Lexing.pos_fname,
       tok))
    (list_of_stream ll)

let lexer_tests = "lexer tests" >:::
  [
    "fanme" >::
      (fun ctxt ->
        let ll = make_lexer ~fname:"foo" {|OPENQASM 2.0;
// argle bargle
|} in
        let toks = list_of_stream ll in
        assert_equal (List.length toks) 1 ;
        let (aux, tok) = List.hd toks in
        assert_equal tok (T_OPENQASM "2.0") ;
        assert_equal (TA.comment_string aux) "" ;
        assert_equal (TA.startpos aux).Lexing.pos_fname "foo" ;
        assert_equal (TA.endpos aux).Lexing.pos_fname "foo" ;
      ) ;
    "header" >::
      (fun ctxt ->
        let ll = make_lexer {|OPENQASM 2.0;
// argle bargle
|} in
        assert_equal (extract_tokens ll) 
          [("", "", T_OPENQASM "2.0")]
      ) ;
    "simple qasm" >::
      (fun ctxt ->
        let ll = make_lexer {|OPENQASM 2.0;
include "qelib1.inc";
qreg q[2];

h q[0];
CX q[0],q[1];
|} in
        assert_equal (extract_tokens ll)
          [("", "", T_OPENQASM "2.0"); ("", "", T_INCLUDE "qelib1.inc");
           ("", "", T_QREG); ("", "", T_ID "q");
           ("", "", T_LBRACKET); ("", "", T_INTEGER 2);
           ("", "", T_RBRACKET); ("", "", T_SEMICOLON);
           ("", "", T_ID "h"); ("", "", T_ID "q");
           ("", "", T_LBRACKET); ("", "", T_INTEGER 0);
           ("", "", T_RBRACKET); ("", "", T_SEMICOLON);
           ("", "", T_CX); ("", "", T_ID "q");
           ("", "", T_LBRACKET); ("", "", T_INTEGER 0);
           ("", "", T_RBRACKET); ("", "", T_COMMA);
           ("", "", T_ID "q"); ("", "", T_LBRACKET);
           ("", "", T_INTEGER 1); ("", "", T_RBRACKET);
           ("", "", T_SEMICOLON)]
      ) ;
    "qasm body" >::
      (fun ctxt ->
        let ll = make_body_lexer {|
// argle
qreg q[2];

h q[// bargle
0];
|} in
        assert_equal (extract_tokens ll)
          [("// argle\n", "", T_QREG); ("", "", T_ID "q");
           ("", "", T_LBRACKET); ("", "", T_INTEGER 2);
           ("", "", T_RBRACKET); ("", "", T_SEMICOLON);
           ("", "", T_ID "h"); ("", "", T_ID "q");
           ("", "", T_LBRACKET); ("// bargle\n", "", T_INTEGER 0);
           ("", "", T_RBRACKET); ("", "", T_SEMICOLON)]
      ) ;
  ]

let test_parse_expr (name, txt, expect) =
  name >:: (fun ctx ->
    let (aux, e) = body_parse PA.expr txt in
      assert_equal expect (TA.comment_string aux, e)
  )

let expr_parser_tests = "expr parser tests" >:::
  (List.map test_parse_expr 
     [
       ("nnint", "1", ("", Ast.NNINT 1));
       ("real 0", "0.e+0", ("", Ast.REAL "0.e+0"));
       ("id", "x", ("", Ast.ID "x"));
       ("uminus", "-x", ("", Ast.UMINUS (Ast.ID "x"))) ;
       ("uminus comment", " // argle\n -x", ("// argle\n", Ast.UMINUS (Ast.ID "x"))) ;
       ("uminus comment 2", "-// argle\n x", ("// argle\n", Ast.UMINUS (Ast.ID "x"))) ;
     ]
  )

let test_parse_instruction (name, txt, expect) =
  name >:: (fun ctx ->
    let (aux, e) = body_parse PA.instruction txt in
      assert_equal expect (TA.comment_string aux, e)
  )

let test_parse_statement (name, txt, expect) =
  name >:: (fun ctx ->
    let (aux, e) = body_parse PA.statement txt in
      assert_equal expect (TA.comment_string aux, e)
  )

let statement_parser_tests = "statement parser tests" >:::
  (List.map test_parse_statement
     [
       ("qreg", "qreg q[1];", ("", Ast.STMT_QREG("q", 1)));
       ("qreg", {|
qreg //argle
q[//bargle
1];|}, ("//argle\n//bargle\n", Ast.STMT_QREG("q", 1)));
       ("CX", "CX q, b;", ("", Ast.STMT_INSTRUCTION(Ast.CX(Ast.REG "q", Ast.REG "b")))) ;
       ("if", "if(c==1) CX q, b;", ("", Ast.STMT_IF("c", 1, Ast.CX(Ast.REG "q", Ast.REG "b")))) ;
       ("if comment", 
{|if(c==//bargle
1) CX //argle
q, b;|},
 ("//bargle\n//argle\n", Ast.STMT_IF("c", 1, Ast.CX(Ast.REG "q", Ast.REG "b")))) ;
     ]
  )

let test_roundtrip_main_buf (name, txt, expect) =
  name >:: (fun ctxt ->
    let rv = full_parse PA.mainprogram txt in
    let pretty = PP.pp PP.main rv in
    assert_equal expect pretty
  )

let test_roundtrip_main_file (name, fname, expect) =
  name >:: (fun ctxt ->
    let rv = full_parse_from_file PA.mainprogram fname in
    let pretty = PP.pp PP.main rv in
    assert_equal expect pretty
  )

let parser_tests = "parser tests" >:::
  [
    test_roundtrip_main_buf ("header",{|OPENQASM 2.0;
qreg q[1] ;
|},
{|OPENQASM 2.0;
qreg q[1] ;
|}) ;
    test_roundtrip_main_buf ("include 0", {|OPENQASM 2.0;
include "testdata/empty.inc";
qreg q[1] ;
|},
{|OPENQASM 2.0;
qreg q[1] ;
|}) ;
    test_roundtrip_main_buf ("include 1", {|OPENQASM 2.0;
include "testdata/oneline.inc";
|},
{|OPENQASM 2.0;
qreg q[1] ;
|}) ;
    test_roundtrip_main_file ("example", "testdata/example.qasm",
{|OPENQASM 2.0;
qreg q[1] ;
|}) ;
  ]

(* Run the tests in test suite *)
let _ =
if invoked_as "qasmsyntax_tests" then
  run_test_tt_main ("all_tests" >::: [ misc_tests ; lexer_tests; expr_parser_tests; statement_parser_tests; parser_tests ])
;;
