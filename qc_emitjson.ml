(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open Misc_functions
open Coll
open Qc_environment
open Qasmsyntax
open Qasmsyntax
open Qasmparser
open Qasmdag0
open Qasmpp


module JSON = struct

  type header_t = {
      n_qubits: int ;
      memory_slots: int ;
      qubit_labels : (string * int) list ;
      clbit_labels : (string * int) list ;
      qreg_sizes : (string * int) list ;
      creg_sizes : (string * int) list ;
    }

  type circuit_t = {
      instructions : unit list ;
      header : header_t ;
    }

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

  let add_stmt st stmt = st

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
