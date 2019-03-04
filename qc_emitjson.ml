(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open Sexplib0.Sexp_conv

open Misc_functions
open Coll
open Qc_environment
open Qasmsyntax
open Qasmsyntax
open Qasmparser
open Qasmdag0
open Qasmpp
open Qc_symbolic

module JSON = struct

  type header_t = {
      n_qubits: int ;
      memory_slots: int ;
      qubit_labels : (string * int) list ;
      clbit_labels : (string * int) list ;
      qreg_sizes : (string * int) list ;
      creg_sizes : (string * int) list ;
    } [@@deriving yojson, sexp]

  type instruction_t = {
      name : string ;
      params : float list ;
      texparams : string list ;
      qubits : int list ;
      memory : int list ;
    } [@@deriving yojson, sexp]

  type circuit_t = {
      instructions : instruction_t list ;
      header : header_t ;
    } [@@deriving yojson, sexp]

  type state_t = {
      circuit : circuit_t ;
      _number_of_qubits : int ;
      _number_of_clbits : int ;
      _qreg_sizes : (string * int) list ;
      _creg_sizes : (string * int) list ;
      _qubit_order : (string * int) list ;
      _clbit_order : (string * int) list ;
      _qubit_order_internal : ((string * int), int) LM.t ;
      _cbit_order_internal : ((string * int), int) LM.t ;
      basis : (string, (int * int * int)) LM.t ;
    }

  let mk_basis envs =
    envs.TYCHK.Env.gates
    |> LM.toList 
    |> List.fold_left (fun basis (gateid, (_, formal_params, formal_qubits, _)) ->
           LM.add basis (gateid, (List.length formal_qubits, 0, List.length formal_params)))
         (LM.ofList () Defaults._DEFAULT_DAG0_BASIS)

  let mk envs =
    let basis = mk_basis envs in
    {
      circuit = {
        instructions = [] ;
        header = {
            n_qubits = 0 ;
            memory_slots = 0 ;
            qubit_labels = [] ;
            clbit_labels = [] ;
            qreg_sizes = [] ;
            creg_sizes = [] ;
          } ;
      } ;
      _number_of_qubits  = 0 ;
      _number_of_clbits  = 0 ;
      _qreg_sizes = [] ;
      _creg_sizes = [] ;
      _qubit_order = [] ;
      _clbit_order = [] ;
      _qubit_order_internal = LM.mk() ;
      _cbit_order_internal = LM.mk() ;
      basis ;
    }

  let add_creg st (name, size) =
    let _creg_sizes = st._creg_sizes @ [(name, size)] in
    let _number_of_clbits = st._number_of_clbits + size in
    let _clbit_order = st._clbit_order @ (
        (interval 0 size)
        |> List.map (fun j -> (name, j))
      ) in
    let circuit = {
        st.circuit with
        header = {
          st.circuit.header with
          memory_slots = _number_of_clbits ;
          creg_sizes = _creg_sizes ;
          clbit_labels = _clbit_order ;
        }
      } in
    {
      st with
      _creg_sizes ;
      _clbit_order ;
      _number_of_clbits ;
      _cbit_order_internal =
        (interval 0 size)
        |> List.fold_left (fun m j ->
               LM.add m ((name, j), st._number_of_clbits + j))
             st._cbit_order_internal ;
      circuit ;
    }

  let add_qreg st (name, size) =
    let _qreg_sizes = st._qreg_sizes @ [(name, size)] in
    let _number_of_qubits = st._number_of_qubits + size in
    let _qubit_order = st._qubit_order @ (
        (interval 0 size)
        |> List.map (fun j -> (name, j))
      ) in
    let circuit = {
        st.circuit with
        header = {
          st.circuit.header with
          n_qubits = _number_of_qubits ;
          qreg_sizes = _qreg_sizes ;
          qubit_labels = _qubit_order ;
        }
      } in
    {
      st with
      _qreg_sizes ;
      _qubit_order ;
      _number_of_qubits ;
      _qubit_order_internal =
        (interval 0 size)
        |> List.fold_left (fun m j ->
               LM.add m ((name, j), st._number_of_qubits + j))
             st._qubit_order_internal ;
      circuit ;
    }

  let raw_uop_name = function
    | AST.U _ -> "U"
    | CX _ -> "CX"
    | COMPOSITE_GATE (gateid, _, _) -> gateid

  let raw_qop_name = function
    | AST.UOP u -> raw_uop_name u
    | MEASURE _ -> "measure"
    | RESET _ -> "reset"

  let raw_stmt_name = function
    | AST.STMT_GATEDECL _ -> assert false
    | STMT_OPAQUEDECL _ -> assert false
    | STMT_QOP q -> raw_qop_name q
    | STMT_IF (_, _, q) -> raw_qop_name q
    | STMT_BARRIER _ -> "barrier"
    | STMT_QREG _ -> assert false
    | STMT_CREG _ -> assert false

  let raw_uop_args = function
    | AST.U (_, q) -> ([q], [])
    | CX (a, b) -> ([a;b], [])
    | COMPOSITE_GATE (gateid, _, ql) -> (ql, [])

  let raw_qop_args = function
    | AST.UOP u -> raw_uop_args u
    | MEASURE (q,c) -> ([q], [c])
    | RESET q -> ([q], [])

  let raw_stmt_args = function
    | AST.STMT_GATEDECL _ -> assert false
    | STMT_OPAQUEDECL _ -> assert false
    | STMT_QOP q -> raw_qop_args q
    | STMT_IF (_, _, q) -> assert false
    | STMT_BARRIER l -> (l, [])
    | STMT_QREG _ -> assert false
    | STMT_CREG _ -> assert false


  let raw_uop_params = function
    | AST.U (l, _) -> l
    | CX _ -> []
    | COMPOSITE_GATE (gateid, params, _) -> params

  let raw_qop_params = function
    | AST.UOP u -> raw_uop_params u
    | MEASURE _ | RESET _ -> []

  let raw_stmt_params = function
    | AST.STMT_GATEDECL _ -> assert false
    | STMT_OPAQUEDECL _ -> assert false
    | STMT_QOP q -> raw_qop_params q
    | STMT_IF (_, _, q) -> assert false
    | STMT_BARRIER _ | STMT_QREG _ | STMT_CREG _ -> []




  let add_stmt st stmt =
    let name = raw_stmt_name stmt in
    let (qargs, cargs) = raw_stmt_args stmt in
    let qubit_indices =
      List.map (function
          | AST.IT _ -> assert false
          | AST.INDEXED(AST.QREG name, i) ->
             LM.map st._qubit_order_internal (name, i)) qargs in
    let clbit_indices =
      List.map (function
          | AST.IT _ -> assert false
          | AST.INDEXED(AST.CREG name, i) ->
             LM.map st._cbit_order_internal (name, i)) cargs in
    let params = raw_stmt_params stmt in
    let gate_instruction = {
        name ;
        params = List.map Eval.expr params ;
        texparams = List.map Latex.expr params ;
        qubits = qubit_indices ;
        memory = clbit_indices ;
      } in
    {
      st with
      circuit = {
        st.circuit with
        instructions = st.circuit.instructions @ [gate_instruction] ;
      }
    }

  let to_json envs dag =
    let st = mk envs in
    let st = List.fold_left add_qreg st envs.TYCHK.Env.qregs in
    let st = List.fold_left add_creg st envs.TYCHK.Env.cregs in
    let nodel = DAG.tsort dag in
    List.fold_left (fun st n ->
        match (LM.map dag.node_info n).label with
        | INPUT _| OUTPUT _ -> st
        | STMT stmt -> add_stmt st stmt)
      st nodel
end
