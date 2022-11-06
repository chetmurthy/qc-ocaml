(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open OUnit2
open Asttools
open Pa_ppx_utils
open Pa_ppx_testutils
open Coll
open Std
open Misc_functions
open Qlam_syntax
open Qc
module Ops = Qlam_ops

let matches ~pattern text =
  let rex = Pcre.regexp ~flags:[`DOTALL] pattern in
  Pcre.pmatch ~rex text

let assert_raises_exn_pattern ~msg pattern f =
  Testutil.assert_raises_exn_pred ~exnmsg:msg
    (function
       Ploc.Exc(_, exn) when matches ~pattern (Printexc.to_string exn) -> true
     | exn when matches ~pattern (Printexc.to_string exn) -> true
     | _ -> false
     )
    f

let env0a = with_include_path ~path:["testdata"] Qlam.Environ.of_file "qelib0.qli" ;;
let env0b = with_include_path ~path:["testdata"] Qlam.Environ.of_file "machines.qli" ;;
let env0c = with_include_path ~path:["testdata"] Qlam.Environ.of_file "layouts.qli" ;;
let env0 = env0a @ env0b @ env0c ;;

let parse_tolam s0 =
  let (env,instrs) = with_include_path ~path:["testdata"] Qasm2.of_string s0 in
  (env, instrs) |>  Qconvert.ToLam.program

let read_tolam s0 =
  let (env,instrs) = with_include_path ~path:["testdata"] Qasm2.of_file s0 in
  (env, instrs) |>  Qconvert.ToLam.program
;;
