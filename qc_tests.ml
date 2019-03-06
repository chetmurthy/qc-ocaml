(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open OUnit2
open Misc_functions
open Coll
open Qasmlex
open Qasmsyntax
open Qasmparser
open Qasm_io
open Qasmpp
open Qasmdag0
open Qasm_passes

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
    let (aux, e) = body_parse ~path:[] PA.expr txt in
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
    let (aux, e) = body_parse ~path:[] PA.qop txt in
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
    let rv = CST.AuxMap.stmt aux2comment_mapper (body_parse ~path:[] PA.statement txt) in
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
                 body_parse ~path:[] PA.statement "gate g a, b { cx a, b; measure a->b ;}")
    ) ;
   ]
  )

let test_roundtrip_main_buf (name, txt, expect) =
  name >:: (fun ctxt ->
    let rv = full_parse ~path:["testdata"] PA.mainprogram txt in
    let pretty = CSTPP.(pp (main ~skip_qelib:true) rv) in
    assert_equal expect pretty
  )

let test_roundtrip_main_file (name, fname, expect) =
  name >:: (fun ctxt ->
    let rv = full_parse_from_file ~path:["testdata"] PA.mainprogram fname in
    let pretty = CSTPP.(pp (main ~skip_qelib:true) rv) in
    assert_equal expect pretty
  )

let test_roundtrip_program_file (name, fname, expect) =
  name >:: (fun ctxt ->
    let rv = body_parse_from_file ~path:["testdata"] PA.mainprogram fname in
    let pretty = CSTPP.(pp (main ~skip_qelib:true) rv) in
    assert_equal expect pretty
  )

let parser_tests = "parser tests" >:::
  [
    test_roundtrip_main_buf ("header",{|OPENQASM 2.0;
qreg q[1];
|},
{|OPENQASM 2.0;
include "qelib1.inc";
qreg q[1];
|}) ;
    test_roundtrip_main_buf ("include 0", {|OPENQASM 2.0;
include "empty.inc";
qreg q[1];
|},
{|OPENQASM 2.0;
include "qelib1.inc";
qreg q[1];
|}) ;
    test_roundtrip_main_buf ("include 1", {|OPENQASM 2.0;
include "oneline.inc";
|},
{|OPENQASM 2.0;
include "qelib1.inc";
qreg q[1];
|}) ;
    test_roundtrip_main_file ("example", "testdata/example.qasm", file_contents "testdata/example.qasm-result") ;
    "fail">:: (fun ctxt ->
      assert_raises ~msg:"should raise SyntaxError(lexing)"
        (SyntaxError "lexing: failed in file \"testdata/example_fail.qasm\" at char 9")
               (fun () ->
                 full_parse_from_file ~path:[] PA.mainprogram "testdata/example_fail.qasm")
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
    let pl = body_parse ~path:["testdata"] PA.program txt in
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
    let _,pl = full_parse_from_file ~path:["testdata"] PA.mainprogram fname in
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
    let pl = body_parse ~path:["testdata"] PA.program txt in
    let (envs, p) = TYCHK.program pl in
    let envs = TYCHK.Env.auxmap aux2unit_mapper envs in
    let pretty_p = ASTPP.(pp (program ~skip_qelib:true) p) in

    do_option (fun expect_env ->
        assert_equal ~cmp:TYCHK.Env.equal expect_env envs)
    expect_env ;
    do_option (fun expect ->
        assert_equal expect pretty_p)
      expect
  )


let test_typecheck_roundtrip_file (name, fname, expect_env, expect) =
  name >:: (fun ctxt ->
    let vers,pl = full_parse_from_file ~path:["testdata"] PA.mainprogram fname in
    let (envs, p) = TYCHK.program pl in
    let envs = TYCHK.Env.auxmap aux2unit_mapper envs in
    let pretty_p = ASTPP.(pp (main ~skip_qelib:true) (vers, p)) in

    do_option (fun expect_env ->
        assert_equal ~cmp:TYCHK.Env.equal expect_env envs)
    expect_env ;
    do_option (fun expect ->
        assert_equal expect pretty_p)
      expect
  )


let test_typecheck_fail (name, txt, msg, exn) =
  name >:: (fun ctxt ->
    let pl = body_parse ~path:["testdata"] PA.program txt in
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

let test_dag0 (name, txt) =
  name >:: (fun ctxt ->
    let _,dag = program_to_dag0 ~path:["testdata"] txt in
    pp DAG.pp_dag dag ;
    ()
  )


let test_dag0_file (name, fname) =
  name >:: (fun ctxt ->
    let (envs, dag) = full_to_dag0_from_file ~path:["testdata"] fname in
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
        assert_equal (parse_to_dag0_to_ast ~path:[] atxt) (parse_to_dag0_to_ast ~path:[] btxt)
      ) ;
    ]
  )

let unroll ~only txt =
  let pl = body_parse ~path:["testdata"] PA.program txt in
  let (envs, p) = TYCHK.program pl in
  let dag = DAG.make envs p in
  let dag = Unroll.execute ~only envs dag in
  let pl = DAG.to_ast envs dag in  
  pp (ASTPP.program ~skip_qelib:true) pl

let test_unroll (name, only, txt, expect) =
  name >:: (fun ctxt ->
    let txt = unroll ~only txt in
    assert_equal txt expect
  )

let unroll_tests = "unroll tests" >:::
  (
    (List.map test_unroll [
         ("unroll 0", ["g"],
          "gate g a, b { CX a, b; } qreg q[2]; g q[0], q[1]; ",
          "qreg q[2];\nCX q[0], q[1];\n")
       ]
    )
  )

open Qrpc_api
let credentials_tests = "credentials tests" >:::
  [
    "basic url" >:: (fun ctxt ->
      let url = "foo" in
      assert_equal (url, None, None, None)
        (Credentials.Single._unify_ibmq_url (url, None, None, None))
    ) ;
    "single 0" >:: (fun ctxt ->
      let url = "https://q-console-api.mybluemix.net/api/Hubs/ibmq/Groups/qc-ware/Projects/default" in
      assert_equal (url, Some "ibmq", Some "qc-ware", Some "default")
        (Credentials.Single._unify_ibmq_url (url, None, None, None))
    ) ;
    "single 0b" >:: (fun ctxt ->
      let url = "https://Q-CONSOLE-API.MYBLUEMIX.NET/API/HUBS/IBMQ/GROUPS/QC-WARE/PROJECTS/DEFAULT" in
      assert_equal (url, Some "IBMQ", Some "QC-WARE", Some "DEFAULT")
        (Credentials.Single._unify_ibmq_url (url, None, None, None))
    ) ;
    "single 1" >:: (fun ctxt ->
      let url = "https://quantumexperience.ng.bluemix.net/api" in
      assert_equal (url, None, None, None)
        (Credentials.Single._unify_ibmq_url (url, None, None, None))
    ) ;
    "credentials 0" >:: (fun ctxt ->
      let accts = Credentials.mk() in
      Credentials.add_rcfile ~fname:"testdata/qiskitrc.chet" accts ;
      assert_equal (Credentials.export accts)
        [("ibmq",
          {Qrpc_api.Credentials.Single.token =
             "4d128c911fa9b7624a0073f29c72eaba59d2206fe68049cdd06c9cd49f508479a918eef102035dc9be98e83be9cbac495f34ec863274324dcf21a06cfa10e27b";
           url = "https://quantumexperience.ng.bluemix.net/api"; hub = None;
           group = None; project = None; verify = true})]
    ) ;
    "credentials 1" >:: (fun ctxt ->
      let accts = Credentials.mk() in
      Credentials.add_rcfile ~fname:"testdata/qiskitrc.2" accts ;
      assert_equal (Credentials.export accts)
        [("ibmq",
          {Qrpc_api.Credentials.Single.token = "975c";
           url = "https://quantumexperience.ng.bluemix.net/api"; hub = None;
           group = None; project = None; verify = true});
         ("ibmq_ibmq_qc-ware_default",
          {Qrpc_api.Credentials.Single.token = "dcbd";
           url =
             "https://q-console-api.mybluemix.net/api/Hubs/ibmq/Groups/qc-ware/Projects/default";
           hub = Some "ibmq"; group = Some "qc-ware"; project = Some "default";
           verify = true})]
    ) ;
    "credentials 2 busted" >:: (fun ctxt ->
      assert_raises ~msg:"we're going with verify field is required"
        (Failure("invalid ini file section ibmq (no verify attribute)"))
        (fun () ->
          let accts = Credentials.mk() in
          Credentials.add_rcfile ~fname:"testdata/qiskitrc.2.BUSTED" accts)
    ) ;
  ]

open Qc_layout
let layout_tests = "layout tests" >:::
  [
    "bfs" >:: (fun ctxt ->
      let cmap = [( 1,  0);
       ( 1,  2);
       ( 2,  3);
       ( 4,  3);
       ( 4, 10);
       ( 5,  4);
       ( 5,  6);
       ( 5,  9);
       ( 6,  8);
       ( 7,  8);
       ( 9,  8);
       ( 9, 10);
       (11,  3);
       (11, 10);
       (11, 12);
       (12,  2);
       (13,  1);
       (13, 12)] in
      let g = Layout.cmap_to_graph cmap in
      assert_equal (Layout.bfs g 0) [0; 1; 2; 13; 3; 12; 4; 11; 5; 10; 6; 9; 8; 7]
    )
  ]

open Qobj_compile

let (fuzzy_compare: Yojson_helpers.user_comparator_t) = fun ~cmp0 f1 f2 ->
  match f1, f2 with
  | `Float 3.14159265358979312, `Float 3.14159265358979 -> true
  | `Float 3.14159265358979, `Float 3.14159265358979312  -> true
  | `String s1, `String s2 ->
     starts_with ~pat:"OPENQASM" s1 && starts_with ~pat:"OPENQASM" s2 
  | _ -> false

let compile_tests = "compile tests" >:::
  [
    "basis" >:: (fun ctxt ->
      let (envs, dag) = program_to_dag0 ~path:["testdata"] {|
                                 include "qelib1.inc";
                                 qreg q[14];
                                 creg c0[2];
                                 u2(0,pi) q[1];
                                 cx q[1],q[0];
                                 u2(0,pi) q[1];
                                 u2(0,pi) q[0];
                                 barrier q[0],q[1];
                                 measure q[1] -> c0[1];
                                 measure q[0] -> c0[0];
                                 |} in
      let expected_basis = [("U", (1, 0, 3)); ("CX", (2, 0, 0)); ("measure", (1, 1, 0)); ("reset", (1, 0, 0)); ("barrier", (-1, 0, 0)); ("u3", (1, 0, 3)); ("u2", (1, 0, 2)); ("u1", (1, 0, 1)); ("cx", (2, 0, 0)); ("id", (1, 0, 0)); ("u0", (1, 0, 1)); ("x", (1, 0, 0)); ("y", (1, 0, 0)); ("z", (1, 0, 0)); ("h", (1, 0, 0)); ("s", (1, 0, 0)); ("sdg", (1, 0, 0)); ("t", (1, 0, 0)); ("tdg", (1, 0, 0)); ("rx", (1, 0, 1)); ("ry", (1, 0, 1)); ("rz", (1, 0, 1)); ("cz", (2, 0, 0)); ("cy", (2, 0, 0)); ("swap", (2, 0, 0)); ("ch", (2, 0, 0)); ("ccx", (3, 0, 0)); ("cswap", (3, 0, 0)); ("crz", (2, 0, 1)); ("cu1", (2, 0, 1)); ("cu3", (2, 0, 3)); ("rzz", (2, 0, 1))] in
      let canon l = List.sort Pervasives.compare l in
      assert_equal (canon (JSON.mk_basis envs)) (canon expected_basis) ;
    ) ;

    "emit 0" >:: (fun ctxt ->
        let (envs, dag) = full_to_dag0_from_file ~path:["testdata"] "testdata/qobj/bell0.qasm" in
        let circuit = Compile.circuit_to_experiment  ~name:"circuit0" envs dag in
        let expected_circuit_txt = file_contents "testdata/qobj/bell0.exp" in
        let expected_circuit_json = Yojson.Safe.from_string expected_circuit_txt in
        assert_equal ~cmp:(Yojson_helpers.compare ~explain:true ~usercmp:fuzzy_compare)
          (circuit |> Qobj_types.Experiment.to_yojson |> Yojson_helpers.canon)
          (expected_circuit_json |> Yojson_helpers.canon) ;

        let (qobj: Qobj_types.Qobj.t) = Compile.circuits_to_qobj ~backend_name:"ibmq_16_melbourne"
                     ~shots:1024 ~max_credits:10 ~qobj_id:"168a65c1-f83b-4346-8643-6aa9eea59234"
                     ~memory:false ["circuit0",envs, dag] in
        let expected_qobj_txt = file_contents "testdata/qobj/bell0.qobj" in
        let expected_qobj_json = Yojson.Safe.from_string expected_qobj_txt in
        assert_equal ~cmp:(Yojson_helpers.compare ~explain:true ~usercmp:fuzzy_compare)
          (qobj |> Qobj_types.Qobj.to_yojson |> Yojson_helpers.canon)
          (expected_qobj_json |> Yojson_helpers.canon) ;

    )
  ]

let do_trip_test_circuit_to_qasm name dir =
  let qasm1 = Printf.sprintf "testdata/extracted-unit-tests/%s/1-orig.qasm" dir in
  let qasm2 = Printf.sprintf "testdata/extracted-unit-tests/%s/2-from-circuit.qasm" dir in
  let rv = full_parse_from_file ~path:["testdata"] PA.mainprogram qasm1 in
  let pretty = CSTPP.(pp (main ~skip_qelib:true) rv) in
  if pretty <> (file_contents qasm2) then begin
      Printf.printf "\n================================ %s ================================\n" name ;
      Printf.printf "%s\n" pretty ;
      Printf.printf "================================ %s ================================\n" name ;
      Printf.printf "%s\n" (file_contents qasm2) ;
      Printf.printf "================================ %s ================================\n" name ;
      try DAG.WeaklyIsomorphic.iso (snd (full_to_dag0 ~path:["testdata"] pretty))
            (snd (full_to_dag0 ~path:["testdata"] (file_contents qasm2))) ;
          Printf.printf "But DAGs were isomorphic!!\n" ;
          Printf.printf "================================ %s ================================\n" name
      with Failure _ ->
        Printf.printf "Could not prove DAGs were isomorphic\n" ;
        assert_failure "Could not prove DAGs were isomorphic\n"
    end

let fuzzy_normalize_experiment e =
  Qobj_types.Experiment.{
      e with
      instructions = List.sort Pervasives.compare e.instructions ;
      header = { e.header with compiled_circuit_qasm = Some "" }
  }

let fuzzy_normalize_qobj qobj =
  Qobj_types.Qobj.{
      qobj with
      experiments = List.map fuzzy_normalize_experiment qobj.experiments
  }

let qobj_fuzzy_equality qobj1 qobj2 =
  (fuzzy_normalize_qobj qobj1) = (fuzzy_normalize_qobj qobj2)

let trip_test_circuit_to_qasm name dir =
  name >:: (fun ctxt ->
    do_trip_test_circuit_to_qasm name dir
  )

let do_trip_test_circuit_to_qobj name dir backend =
  let qasm1 = Printf.sprintf "testdata/extracted-unit-tests/%s/3-optimized-%s.qasm" dir backend in
  let qobj2 = Printf.sprintf "testdata/extracted-unit-tests/%s/4-compiled-%s.qobj" dir backend in

  let (envs, dag) = full_to_dag0_from_file ~path:["testdata"] qasm1 in

  let expected_qobj_txt = file_contents qobj2 in
  let expected_qobj_json = Yojson.Safe.from_string expected_qobj_txt in
  let expected_qobj =
    expected_qobj_json
    |> Qobj_types.Qobj.of_yojson
    |> error_to_failure ~msg:"Qobj_types.Qobj.of_yojson failed"
  in
  let qobj_id = expected_qobj.Qobj_types.Qobj.qobj_id in

  let (qobj: Qobj_types.Qobj.t) = Compile.circuits_to_qobj ~backend_name:backend
                                    ~shots:1024 ~max_credits:10 ~qobj_id
                                    ~memory:false ["circuit0",envs, dag] in
  if expected_qobj = qobj then ()
  else if qobj_fuzzy_equality expected_qobj qobj then (
    Printf.printf "================================ %s ================================\n" name ;
    Printf.printf "qobjs were equal under fuzzy equality\n" ;
    Printf.printf "================================ %s ================================\n" name
  )
  else (
    Printf.printf "================================ %s ================================\n" name ;
    Printf.printf "qobjs were NOT equal under fuzzy equality\n" ;
    Printf.printf "================================ %s ================================\n" name ;
    assert_equal ~cmp:(Yojson_helpers.compare ~explain:true ~usercmp:fuzzy_compare)
      (qobj |> Qobj_types.Qobj.to_yojson |> Yojson_helpers.canon)
      (expected_qobj |> Qobj_types.Qobj.to_yojson |> Yojson_helpers.canon)
  )
let trip_test_circuit_to_qobj name dir backend =
  (name^"-"^backend) >:: (fun ctxt ->
    do_trip_test_circuit_to_qobj name dir backend
  )

let trip_tests = "trip tests" >::: [
      trip_test_circuit_to_qasm "bell/circuit" "Bell" ;
      trip_test_circuit_to_qobj "bell/qobj" "Bell" "ibmq_16_melbourne" ;
      trip_test_circuit_to_qobj "bell/qobj" "Bell" "ibmq_qasm_simulator" ;

      trip_test_circuit_to_qasm "bell/circuit" "Bell2" ;
      trip_test_circuit_to_qobj "bell/qobj" "Bell2" "ibmq_16_melbourne" ;
      trip_test_circuit_to_qobj "bell/qobj" "Bell2" "ibmq_qasm_simulator" ;

      trip_test_circuit_to_qasm "bell/circuit" "Bell3" ;
      trip_test_circuit_to_qobj "bell/qobj" "Bell3" "ibmq_16_melbourne" ;
      trip_test_circuit_to_qobj "bell/qobj" "Bell3" "ibmq_qasm_simulator" ;

      trip_test_circuit_to_qasm "dj-00/circuit" "dj-00" ;
      trip_test_circuit_to_qasm "dj-01/circuit" "dj-01" ;
      trip_test_circuit_to_qasm "dj-10/circuit" "dj-10" ;
      trip_test_circuit_to_qasm "dj-11/circuit" "dj-11" ;
      trip_test_circuit_to_qasm "grover-1/circuit" "grover-1" ;
      trip_test_circuit_to_qasm "grover-2/circuit" "grover-2" ;
      trip_test_circuit_to_qasm "grover-3/circuit" "grover-3" ;
      trip_test_circuit_to_qasm "shor/circuit" "shor" ;
      trip_test_circuit_to_qasm "simon-3/circuit" "simon" ;
  ]

(* Run the tests in test suite *)
let _ =
if invoked_as "qc_tests" then
  run_test_tt_main ("all_tests" >::: [
        misc_tests ; lexer_tests; expr_parser_tests;
        statement_parser_tests; parser_tests;
        typechecker_tests; dag0_tests ;
        unroll_tests ;
        credentials_tests ;
        layout_tests ; compile_tests ;
        trip_tests ;
    ])
;;
