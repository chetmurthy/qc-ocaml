(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open OUnit2
open Asttools
open Pa_ppx_utils
open Pa_ppx_testutils
open Coll
open Std
open Misc_functions
open Qc_misc
open Qc_dot
open Qlam_env
module Ops = Qlam_ops

open Rresult.R

let compare_files f1 f2 =
  let open Exec in
  let* contents1 = Bos.OS.File.read f1 in
  let* contents2 = Bos.OS.File.read f2 in
  Ok (contents1 = contents2)

let compare_results r1 r2 =
  match (r1, r2) with
    (Error msg1, Error msg2) -> msg1 = msg2
  | (Ok (Some f1), Ok (Some f2)) -> (compare_files f1 f2) |> Rresult.R.get_ok
  | _ -> false

let envitems = Qlam_parser.(with_include_path ~path:["testdata"] qelib_from_file "machines.qli") ;;
let genv0 = Ops.Upgrade.environ (GEnv.mk()) envitems ;;

let todot genv name =
  let cm = GEnv.find_mach genv0 (ID.mk"ibmq_quito") in
  Odot.string_of_graph (Ops.CM.dot cm)
;;

let printer x = Fmt.(str "%a" (Rresult.R.pp ~ok:string ~error:pp_msg) x)

let dot_tests = "dot tests" >:::
[
  "ibmq_quito" >:: (fun _ ->
    assert_equal ~printer
      (Bos.OS.File.read (Fpath.v "testdata/graphviz/ibmq_quito.dot")) (Ok (todot genv0 "ibmq_quito"))

  )
]
;;

Pa_ppx_base.Pp_MLast.Ploc.pp_loc_verbose := true ;;
Pa_ppx_runtime.Exceptions.Ploc.pp_loc_verbose := true ;;
Pa_ppx_runtime_fat.Exceptions.Ploc.pp_loc_verbose := true ;;

(* Run the tests in test suite *)
let _ =
if not !Sys.interactive then
  run_test_tt_main ("all_tests" >::: [
        dot_tests
    ])
;;
