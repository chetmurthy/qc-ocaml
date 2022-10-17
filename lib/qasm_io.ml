(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open Pa_ppx_utils
open Std
open Coll
open Misc_functions
open Qasm2_lexer
open Qasm2syntax
open Qasm2_parser
open Qasmpp
open Qasmdag0
open Qasm_passes


let full_to_ast txt =
  let (_, pl) = full_parse PA.mainprogram txt in
  let (envs, p) = TYCHK.program pl in
  (envs, p)

let full_to_dag0 txt =
  let (_, pl) = full_parse PA.mainprogram txt in
  let (envs, p) = TYCHK.program pl in
  let dag = DAG.make envs p in
  (envs, dag)

let program_to_ast txt =
  let pl = body_parse PA.program txt in
  let (envs, p) = TYCHK.program pl in
  (envs, p)

let program_to_dag0 txt =
  let pl = body_parse PA.program txt in
  let (envs, p) = TYCHK.program pl in
  let dag = DAG.make envs p in
  (envs, dag)

let parse_to_dag0_to_ast txt =
  let pl = body_parse PA.program txt in
  let (envs, p) = TYCHK.program pl in
  let dag = DAG.make envs p in
  let pl = DAG.to_ast envs dag in  
  Fmt.(str "%a" ASTPP.program pl)

let full_to_dag0_to_ast_from_file fname =
  let vers,pl = full_parse_from_file PA.mainprogram fname in
  let (envs, p) = TYCHK.program pl in
  let dag = DAG.make envs p in
  let pl = DAG.to_ast envs dag in  
  Fmt.(str "%a" ASTPP.program pl)

let full_to_ast_from_file fname =
  let vers,pl = full_parse_from_file PA.mainprogram fname in
  let (envs, p) = TYCHK.program pl in
  (envs, p)

let full_to_dag0_from_file fname =
  let vers,pl = full_parse_from_file PA.mainprogram fname in
  let (envs, p) = TYCHK.program pl in
  (envs, DAG.make envs p)

