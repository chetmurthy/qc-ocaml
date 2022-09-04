(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open Pa_ppx_utils
open Std
open Coll
open Qasmsyntax
open Qasmparser
open Qasmpp
open Qasmdag0

(*
 * a composite gate unroller.
 *
 * One can specify either those composite gates TO NOT UNROLL [~except] or those TO UNROLL [~only].
 *
 * If we specify [~only], it is converted into [~except]
 *)

module Unroll = struct

let subst_expr cparamenv e =
  let rec subst1 = function
    | AST.ID (AST.CPARAMVAR id) -> LM.map cparamenv id
    | REAL n -> AST.REAL n
    | NNINT n -> AST.NNINT n
    | PI -> AST.PI
    | ADD (e1, e2) -> AST.ADD(subst1 e1, subst1 e2)
    | SUB (e1, e2) -> AST.SUB(subst1 e1, subst1 e2)
    | MUL (e1, e2) -> AST.MUL(subst1 e1, subst1 e2)
    | DIV (e1, e2) -> AST.DIV(subst1 e1, subst1 e2)
    | UMINUS e -> AST.UMINUS (subst1 e)
    | POW (e1, e2) -> AST.POW(subst1 e1, subst1 e2)
    | SIN e -> AST.SIN (subst1 e)
    | COS e -> AST.COS (subst1 e)
    | TAN e -> AST.TAN (subst1 e)
    | EXP e -> AST.EXP (subst1 e)
    | LN e -> AST.LN (subst1 e)
    | SQRT e -> AST.SQRT (subst1 e)
  in
  subst1 e

let subst_raw_gop cparamenv qubitenv = function
  | AST.GATE_UOP(U(el, AST.QUBIT qid)) ->
     AST.STMT_QOP(AST.UOP(U(List.map (subst_expr cparamenv) el,
                            LM.map qubitenv qid)))

  | AST.GATE_UOP(AST.CX(QUBIT id1, QUBIT id2)) ->
    AST.STMT_QOP(AST.UOP(AST.CX(LM.map qubitenv id1, LM.map qubitenv id2)))

  | AST.GATE_UOP(AST.COMPOSITE_GATE(gateid, el, actual_qargs)) ->
     let el = List.map (subst_expr cparamenv) el in
     let new_qargs = List.map (fun (AST.QUBIT id) ->
                         LM.map qubitenv id) actual_qargs in
     AST.STMT_QOP(AST.UOP(AST.COMPOSITE_GATE(gateid, el, new_qargs)))

  | AST.GATE_BARRIER l ->
     AST.STMT_BARRIER (List.map (fun (AST.QUBIT id) -> LM.map qubitenv id) l)

let subst_gop cparamenv qubitenv (aux, gop) = (aux, subst_raw_gop cparamenv qubitenv gop)

(*
 * unroll one node.
 *
 * Algorithm:
 *
 * (0) verify that the node is an unrollable node
 *
 * (1) gather predecessors and successors.
 *
 * (2) The map [edge-label -> PRED] is the "frontier"
 *
 * (3) the map [edge-label -> SUCC] is the "target"
 *
 * (4) So delete all pred and succ edges, then delete the node itself
 *
 * (5) macro-expand the body of the selected gate-decl, substituting in actuals for formals
 *
 * (6) then just execute the steps of dag0 creation, on this body and [(dag, frontier)] to stitch in the new body
 *
 * (7) then stitch back together the resulting frontier and the [target].
 *)
let unroll1 envs dag node =
  let info = LM.map dag.DAG.node_info node in
  let (gateid, actual_params, actual_qargs) =
    match info.label with
    | DAG.INPUT _ | DAG.OUTPUT _ -> failwith "cannot unroll INPUT/OUTPUT node"
    | DAG.STMT (AST.STMT_QOP(AST.UOP (AST.COMPOSITE_GATE(gateid, actual_params, actual_qargs)))) ->
       (gateid, actual_params, actual_qargs)
    | _ -> failwith "cannot unroll a node that's not a composite gate instance" in
  let (_, formal_params, formal_qargs, gate_body) =
    LM.map envs.TYCHK.Env.gates gateid in
  assert (List.length formal_params = List.length actual_params) ;
  assert (List.length formal_qargs = List.length actual_qargs) ;
  let cparamenv = LM.ofList() (combine formal_params actual_params) in
  let qubitenv = LM.ofList() (combine formal_qargs actual_qargs) in
  let stmts = List.map (subst_gop cparamenv qubitenv) gate_body in
  let pred_edges = DAG.pred_e dag node in
  let succ_edges = DAG.succ_e dag node in
  let dag = List.fold_left DAG.remove_edge_e dag pred_edges in
  let dag = List.fold_left DAG.remove_edge_e dag succ_edges in
  let dag = DAG.remove_vertex dag node in
  let frontier =
    let frontier = List.map (fun (src, edgelabel, dst) ->
                       assert (dst = node) ;
                       (edgelabel, src)
                     ) pred_edges in
    LM.ofList() frontier in
  let target =
    let target = List.map (fun (src, edgelabel, dst) ->
                     assert (src = node) ;
                     (edgelabel, dst)
                   ) succ_edges in
    LM.ofList() target in
  let odag = DAG.reopen_dag ~frontier ~target dag in
  let odag =
    List.fold_left (fun odag (_, stmt) -> (DAG.add_stmt envs) odag stmt) odag stmts in
  let remaining_edges = LM.dom odag.frontier in
  let odag =
    List.fold_left DAG.close_frontier_1 odag remaining_edges in
  DAG.close_odag odag

let select_unroll_nodes ~except dag =
  LM.fold (fun acc (n, info) ->
      match info.DAG.label with
      | DAG.INPUT _ | OUTPUT _ -> acc
      | DAG.STMT(AST.STMT_QOP (AST.UOP (AST.COMPOSITE_GATE (gateid, _, _)))) when not (List.mem gateid except) ->
         n::acc
      | _ -> acc
    ) [] dag.DAG.node_info

let execute ?except ?only envs dag =
  let except =
    match except, only with
    | None, None -> failwith "must specify AT LEAST one of either ~except or ~only"
    | Some _, Some _ -> failwith "must specify ONLY one of either ~except or ~only"
    | Some except, None -> except
    | None, Some only ->
       subtract (LM.dom envs.TYCHK.Env.gates) only in
  let rec unrec dag =
    let l = select_unroll_nodes ~except dag in
    if l = [] then dag
    else
      let dag = List.fold_left (fun dag n ->
                    unroll1 envs dag n
                  ) dag l in
      unrec dag
  in unrec dag

end
