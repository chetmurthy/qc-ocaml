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
    "header fail" >::
      (fun ctxt ->
        assert_raises ~msg:"should raise SyntaxError(lexing)"
          (SyntaxError "lexing: failed in file \"\" at char 9") (fun () ->
            list_of_stream (make_lexer {|OPENQASM 2.0|})
          ) ;
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
       ("nnint", "1", ("", CST.NNINT 1));
       ("real 0", "0.e+0", ("", CST.REAL "0.e+0"));
       ("id", "x", ("", CST.ID "x"));
       ("uminus", "-x", ("", CST.UMINUS (CST.ID "x"))) ;
       ("uminus comment", " // argle\n -x", ("// argle\n", CST.UMINUS (CST.ID "x"))) ;
       ("uminus comment 2", "-// argle\n x", ("// argle\n", CST.UMINUS (CST.ID "x"))) ;
     ]
  )

let test_parse_qop (name, txt, expect) =
  name >:: (fun ctx ->
    let (aux, e) = body_parse PA.qop txt in
      assert_equal expect (TA.comment_string aux, e)
  )

let aux2comment_mapper = {
    CST.AuxMap.stmt = (fun aux _ ->
      TA.comment_string aux
    ) ;
    gop = (fun aux _ ->
      TA.comment_string aux
    )
  }

let test_parse_statement (name, txt, expect) =
  name >:: (fun ctx ->
    let rv = CST.AuxMap.stmt aux2comment_mapper (body_parse PA.statement txt) in
    assert_equal expect rv
  )

let statement_parser_tests = "statement parser tests" >:::
  ((List.map test_parse_statement
     [
       ("qreg", "qreg q[1];", ("", CST.STMT_QREG("q", 1)));
       ("qreg", {|
qreg //argle
q[//bargle
1];|}, ("//argle\n//bargle\n", CST.STMT_QREG("q", 1)));
       ("CX", "CX q, b;", ("", CST.STMT_QOP(CST.UOP (CST.CX(CST.REG "q", CST.REG "b"))))) ;
       ("cx", "cx a, b;", ("", CST.STMT_QOP(CST.UOP (CST.COMPOSITE_GATE("cx", [], [CST.REG "a"; CST.REG "b"]))))) ;
       ("if", "if(c==1) CX q, b;", ("", CST.STMT_IF("c", 1, CST.UOP (CST.CX(CST.REG "q", CST.REG "b"))))) ;
       ("if comment", 
{|if(c==//bargle
1) CX //argle
q, b;|},
 ("//bargle\n//argle\n", CST.STMT_IF("c", 1, CST.UOP (CST.CX(CST.REG "q", CST.REG "b"))))) ;
       ("gate 1", "gate g a, b { cx a, b; }", ("", CST.STMT_GATEDECL ("g", [], ["a"; "b"],
                                                                      [("",
                                                                        CST.GATE_UOP
                                                                          (CST.COMPOSITE_GATE ("cx", [],
                                                                                               [CST.REG "a"; CST.REG "b"])))]))) ;

     ]) @ [
    "fail">:: (fun ctxt ->
      assert_raises ~msg:"should raise SyntaxError(parsing)"
        (SyntaxError "parse error in file \"\" at char 23")
               (fun () ->
                 body_parse PA.statement "gate g a, b { cx a, b; measure a->b ;}")
    ) ;
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

let test_roundtrip_program_file (name, fname, expect) =
  name >:: (fun ctxt ->
    let rv = body_parse_from_file PA.mainprogram fname in
    let pretty = PP.pp PP.main rv in
    assert_equal expect pretty
  )

let parser_tests = "parser tests" >:::
  [
    test_roundtrip_main_buf ("header",{|OPENQASM 2.0;
qreg q[1];
|},
{|OPENQASM 2.0;
qreg q[1];
|}) ;
    test_roundtrip_main_buf ("include 0", {|OPENQASM 2.0;
include "testdata/empty.inc";
qreg q[1];
|},
{|OPENQASM 2.0;
qreg q[1];
|}) ;
    test_roundtrip_main_buf ("include 1", {|OPENQASM 2.0;
include "testdata/oneline.inc";
|},
{|OPENQASM 2.0;
qreg q[1];
|}) ;
    test_roundtrip_main_file ("example", "testdata/example.qasm", file_contents "testdata/example.qasm-result") ;
    "fail">:: (fun ctxt ->
      assert_raises ~msg:"should raise SyntaxError(lexing)"
        (SyntaxError "lexing: failed in file \"testdata/example_fail.qasm\" at char 9")
               (fun () ->
                 full_parse_from_file PA.mainprogram "testdata/example_fail.qasm")
    ) ;
  ]

let typechecker_tests = "typechecker tests" >:::
  [
  ]

(* Run the tests in test suite *)
let _ =
if invoked_as "qasmsyntax_tests" then
  run_test_tt_main ("all_tests" >::: [
        misc_tests ; lexer_tests; expr_parser_tests;
        statement_parser_tests; parser_tests;
        typechecker_tests;
    ])
;;
