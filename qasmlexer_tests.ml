(* Copyright 2019 Chetan Murthy *)

open OUnit2

let basic_tests = "basic tests" >:::
  [
  ]

(* Run the tests in test suite *)
let _ = 
  run_test_tt_main ("all_tests" >::: [ basic_tests ])
;;
