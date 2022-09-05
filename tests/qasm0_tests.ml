(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open OUnit2
open Pa_ppx_utils
open Coll
open Std
open Misc_functions
open Qasm0_lexer
open Qasm0_2circ

let diff_files f1 f2 outf =
  ignore(Unix.system(Fmt.(str "diff -Bwiu %s %s > %s" f1 f2 outf)))

let file_contents f =
  f
  |> Fpath.v
  |> Bos.OS.File.read
  |> Rresult.R.get_ok

let diff_file_string f1 s2 =
  let (tmpf,  oc) = Filename.open_temp_file "qasm" "" in
  let out_tmpf = Filename.temp_file "qasm" ".diff-output" in
  output_string oc s2 ;
  close_out oc ;
  diff_files f1 tmpf out_tmpf ;
  let result = file_contents out_tmpf in
  Unix.unlink tmpf ;
  Unix.unlink out_tmpf ;
  result

let compare_diff_file_string f1 s2 = 
  let s1 : string = file_contents f1 in
  if s1 = s2 then true
  else begin
      let diffres = diff_file_string f1 s2 in
      Fmt.(pf stderr "\n================ DIFFERENCES ================\n%s" diffres) ;
      false
    end

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
        let _ = Top.full_parse list_of_stream {|
	def	c-P,1,'\m{e^{i\alpha} & 0 \cr 0 & e^{-i\alpha}}'
	def	Ryt,0,'\m{\cos{\theta}&-\sin{\theta}\cr\sin{\theta}&\cos{\theta}}'

	qubit	j0
	qubit	j1

	c-P	j0,j1
	Ryt	j0
|} in
        ()
      )
  ]


let parser_tests = "parser tests" >:::
  [
    "simple" >::
      (fun ctxt ->
        let _ = Top.(full_parse parse1) {|# 
# File:   test1.qasm
# Date:   22-Mar-04
# Author: I. Chuang <ichuang@mit.edu>
#
# Sample qasm input file - EPR creation
#
        qubit 	q0
        qubit 	q1

	h	q0	# create EPR pair
	cnot	q0,q1
|} in
        ()
      )
  ]

let test_tex_file base =
  let qasmfile = Fmt.(str "testdata/qasm0/%s.qasm" base) in
  let texfile = Fmt.(str "testdata/qasm0-tex/%s.tex" base) in
  base >:: (fun ctxt ->
    let l = Top.(full_parse_from_file document qasmfile) in
    let s = String.concat "" l in
    assert_equal ~cmp:compare_diff_file_string texfile s
  )

let tex_tests = "tex tests" >:::
  (["test1"
   ; "test2"
   ; "test3"
   ; "test4"
   ; "test5"
   ; "test6"
   ; "test7"
   ; "test8"
   ; "test9"
   ; "test10"
   ; "test11"
   ; "test12"
   ; "test13"
   ; "test14"
   ; "test15"
   ; "test16"
   ; "test17"
   ; "test18"
   ]
   |> List.map test_tex_file)

let test_parse_file fname =
  fname >:: (fun ctxt ->
    let rv = Top.(full_parse_from_file parse1) fname in
    ()
  )

let parse_file_tests = "parse file tests" >:::
  (["testdata/qasm0/test1.qasm"
   ; "testdata/qasm0/test2.qasm"
   ; "testdata/qasm0/test3.qasm"
   ; "testdata/qasm0/test4.qasm"
   ; "testdata/qasm0/test5.qasm"
   ; "testdata/qasm0/test6.qasm"
   ; "testdata/qasm0/test7.qasm"
   ; "testdata/qasm0/test8.qasm"
   ; "testdata/qasm0/test9.qasm"
]
   |> List.map test_parse_file)


(* Run the tests in test suite *)
let _ =
if not !Sys.interactive then
  run_test_tt_main ("all_tests" >::: [
        misc_tests
      ; lexer_tests
      ; parser_tests
      ; parse_file_tests
      ; tex_tests
    ])
;;
