(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open OUnit2
open Misc_functions
open Coll
open Qasmlex
open Qasmsyntax
open Qasmparser
open Qasmpp
open Qasmdag0
open Qasm_passes


let to_dag0 txt =
  let pl = body_parse PA.program txt in
  let (envs, p) = TYCHK.program pl in
  let dag = DAG.make envs p in
  (envs, dag)

let parse_to_dag0_to_ast txt =
  let pl = body_parse PA.program txt in
  let (envs, p) = TYCHK.program pl in
  let dag = DAG.make envs p in
  let pl = DAG.to_ast envs dag in  
  pp ASTPP.program pl

let dag0_from_file fname =
  let vers,pl = full_parse_from_file PA.mainprogram fname in
  let (envs, p) = TYCHK.program pl in
  (envs, DAG.make envs p)

