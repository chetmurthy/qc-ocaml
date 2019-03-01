(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open OUnit2
open Misc_functions
open Coll
open Qasmlex
open Qasmsyntax
open Qasmparser
open Qasmpp
open Qasmdag0

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
    let pretty = CSTPP.(pp main rv) in
    assert_equal expect pretty
  )

let test_roundtrip_main_file (name, fname, expect) =
  name >:: (fun ctxt ->
    let rv = full_parse_from_file PA.mainprogram fname in
    let pretty = CSTPP.(pp main rv) in
    assert_equal expect pretty
  )

let test_roundtrip_program_file (name, fname, expect) =
  name >:: (fun ctxt ->
    let rv = body_parse_from_file PA.mainprogram fname in
    let pretty = CSTPP.(pp main rv) in
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
let aux2unit_mapper = {
    AST.AuxMap.stmt = (fun aux _ ->
      ()
    ) ;
    gop = (fun aux _ ->
      ()
    )
  }

let test_typecheck (name, txt, expect_env, expect_ast) =
  name >:: (fun ctxt ->
    let pl = body_parse PA.program txt in
    let (envs, p) = TYCHK.program pl in
    let p = AST.AuxMap.program aux2unit_mapper p in
    let envs = TYCHK.Env.auxmap aux2unit_mapper envs in
    do_option (fun expect_env ->
        assert_equal ~cmp:TYCHK.Env.equal expect_env envs)
    expect_env ;
    do_option (fun expect_ast ->
        assert_equal expect_ast p)
      expect_ast
  )

let test_typecheck_file (name, fname, expect_env, expect_ast) =
  name >:: (fun ctxt ->
    let _,pl = full_parse_from_file PA.mainprogram fname in
    let (envs, p) = TYCHK.program pl in
    let envs = TYCHK.Env.auxmap aux2unit_mapper envs in
    let p = AST.AuxMap.program aux2unit_mapper p in
    do_option (fun expect_env ->
        assert_equal ~cmp:TYCHK.Env.equal expect_env envs)
    expect_env ;
    do_option (fun expect_ast ->
        assert_equal expect_ast p)
      expect_ast
  )


let test_typecheck_roundtrip (name, txt, expect_env, expect) =
  name >:: (fun ctxt ->
    let pl = body_parse PA.program txt in
    let (envs, p) = TYCHK.program pl in
    let envs = TYCHK.Env.auxmap aux2unit_mapper envs in
    let pretty_p = ASTPP.(pp program p) in

    do_option (fun expect_env ->
        assert_equal ~cmp:TYCHK.Env.equal expect_env envs)
    expect_env ;
    do_option (fun expect ->
        assert_equal expect pretty_p)
      expect
  )


let test_typecheck_roundtrip_file (name, fname, expect_env, expect) =
  name >:: (fun ctxt ->
    let vers,pl = full_parse_from_file PA.mainprogram fname in
    let (envs, p) = TYCHK.program pl in
    let envs = TYCHK.Env.auxmap aux2unit_mapper envs in
    let pretty_p = ASTPP.(pp main (vers, p)) in

    do_option (fun expect_env ->
        assert_equal ~cmp:TYCHK.Env.equal expect_env envs)
    expect_env ;
    do_option (fun expect ->
        assert_equal expect pretty_p)
      expect
  )


let test_typecheck_fail (name, txt, msg, exn) =
  name >:: (fun ctxt ->
    let pl = body_parse PA.program txt in
    assert_raises ~msg exn
      (fun () ->
        TYCHK.program pl)
  )

let typechecker_tests = "typechecker tests" >:::
let open TYCHK.Env in
let open TYCHK in
let open AST in
(
  (List.map test_typecheck [
       ("qreg", "qreg q[1]; qreg r[1];",
        Some { qregs = LM.ofList()["q",1;"r",1] ;
               gates = LM.mk() ;
               cregs = LM.mk() ;
          },
        Some [((), STMT_QREG ("q", 1));
              ((), STMT_QREG ("r", 1))]) ;

       ("cx", "qreg q[1]; qreg r[1]; CX q[0], r[0]; CX q, r;",
        None,
        None) ;
     ]
  ) @
  (List.map test_typecheck_file [
       ("example", "testdata/example.qasm",
        None,
       None) ;
     ]
  ) @
  (List.map test_typecheck_fail [
       ("qreg fail", "qreg q[1]; qreg q[1];",
        "should have caught repeated qreg declaration",
        (TypeError (true,"Error file \"\", chars 11-21: qreg q already declared"))) ;

       ("CX fail", "qreg q[1]; qreg r[2]; CX q, r;",
        "should have caught mismatched args to CX",
        (TypeError (true,"Error file \"\", chars 22-30: registers with different dimensions in qargs"))) ;

       ("CX fail 2", "qreg q[1]; qreg r[2]; CX q, q;",
        "should have caught mismatched args to CX",
        (TypeError (true,"Error file \"\", chars 22-30: qargs are not distinct"))) ;

       ("CX fail 3", "gate h a,b,c,d { CX a,a ; }",
        "should have caught mismatched args to CX",
        (TypeError (true,"Error file \"\", chars 17-25: qargs are not distinct"))) ;

       ("CX fail 3", "gate h a,b,c,d { CX a,a ; }",
        "should have caught mismatched args to CX",
        (TypeError (true,"Error file \"\", chars 17-25: qargs are not distinct"))) ;

       ("gate fail 1", "qreg q[2]; qreg r[2]; qreg s[2]; gate h a,b,c,d { CX a,b ; } h q,q,s[0], s[1];",
        "should have caught mismatched args to CX",
        (TypeError (true,"Error file \"\", chars 61-78: qargs are not distinct"))) ;

       ("gate fail 2", "qreg q[2]; qreg r[2]; qreg s[2]; gate h a,b,c,d { CX a,b ; } h q,r,q[0], s[1];",
        "should have caught mismatched args to CX",
        (TypeError (true,"Error file \"\", chars 61-78: bit q[0] conflicts with register of same name"))) ;

       ("gate fail 3", "qreg q[2]; qreg r[2]; qreg s[2]; gate h a,b,c,d { CX a,b ; } h q,r,s[0], s[0];",
        "should have caught mismatched args to CX",
        (TypeError (true,"Error file \"\", chars 61-78: qargs are not distinct"))) ;

       ("gate fail 3", "qreg q[2]; qreg r[2]; qreg s[2]; gate h a,b,c,d { CX a,b ; } h q,r,s[0], s[2];",
        "should have caught mismatched args to CX",
        (TypeError (true,"Error file \"\", chars 61-78: bit s[2] out of dimension [0..2)"))) ;
     ]
  )
  @
  (List.map test_typecheck_roundtrip_file [
       ("example", "testdata/example.qasm", None, None) ;
     ]
  )
)

let to_dag0 txt =
  let pl = body_parse PA.program txt in
  let (envs, p) = TYCHK.program pl in
  let dag = DAG.make envs p in
  dag

let parse_to_dag0_to_ast txt =
  let pl = body_parse PA.program txt in
  let (envs, p) = TYCHK.program pl in
  let (dag, _) = DAG.make envs p in
  let pl = DAG.to_ast envs dag in  
  pp ASTPP.program pl

let test_dag0 (name, txt) =
  name >:: (fun ctxt ->
    let dag = to_dag0 txt in
    pp DAG.pp_both dag ;
    ()
  )

let test_dag0_file (name, fname) =
  name >:: (fun ctxt ->
    let vers,pl = full_parse_from_file PA.mainprogram fname in
    let (envs, p) = TYCHK.program pl in
    let dag = DAG.make envs p in
    ()
  )

let dag0_tests = "dag0 tests" >:::
  (
    (List.map test_dag0 [
         ("qreg",  "qreg q[1]; qreg r[1];") ;
       ] ;
    )
    @
    (List.map test_dag0_file [
         ("example",  "testdata/example.qasm") ;
       ] ;
    )
    @
    [
      "tsort" >:: (fun ctxt ->
        let atxt = {| qreg r[2] ; CX r[0], r[1] ; qreg q[2] ; CX q[0], q[1] ; |} in
        let btxt = {| qreg q[2] ; CX q[0], q[1] ; qreg r[2] ; CX r[0], r[1] ; |} in
        assert_equal (parse_to_dag0_to_ast atxt) (parse_to_dag0_to_ast btxt)
      )
    ]
  )

(* Run the tests in test suite *)
let _ =
if invoked_as "qasmsyntax_tests" then
  run_test_tt_main ("all_tests" >::: [
        misc_tests ; lexer_tests; expr_parser_tests;
        statement_parser_tests; parser_tests;
        typechecker_tests; dag0_tests ;
    ])
;;
