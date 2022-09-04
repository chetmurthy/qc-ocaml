(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open OUnit2
open Pa_ppx_utils
open Coll
open Std
open Misc_functions
open Qasm0_lexer
open Qasm0_2circ

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
        let _ = full_parse list_of_stream {|
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
        let _ = full_parse parse1 {|# 
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


let test_parse_file fname =
  fname >:: (fun ctxt ->
    let rv = full_parse_from_file parse1 fname in
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
    ])
;;
