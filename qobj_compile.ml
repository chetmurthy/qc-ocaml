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
open Qobj_types

module JSON = struct

  type state_t = {
      circuit_header : Experiment.header_t ;
      circuit_instructions : Instruction.t list ;
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
      circuit_instructions = [] ;
      circuit_header = {
          name = None ;
          n_qubits = 0 ;
          memory_slots = 0 ;
          qubit_labels = [] ;
          clbit_labels = [] ;
          qreg_sizes = [] ;
          creg_sizes = [] ;
          compiled_circuit_qasm = None ;
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
        (interval 0 (size-1))
        |> List.map (fun j -> (name, j))
      ) in
    {
      st with
      _creg_sizes ;
      _clbit_order ;
      _number_of_clbits ;
      _cbit_order_internal =
        (interval 0 (size-1))
        |> List.fold_left (fun m j ->
               LM.add m ((name, j), st._number_of_clbits + j))
             st._cbit_order_internal ;
      circuit_header = {
          st.circuit_header with
          memory_slots = _number_of_clbits ;
          creg_sizes = _creg_sizes ;
          clbit_labels = _clbit_order ;
          compiled_circuit_qasm = None ;
        }
    }

  let add_qreg st (name, size) =
    let _qreg_sizes = st._qreg_sizes @ [(name, size)] in
    let _number_of_qubits = st._number_of_qubits + size in
    let _qubit_order = st._qubit_order @ (
        (interval 0 (size-1))
        |> List.map (fun j -> (name, j))
      ) in
    {
      st with
      _qreg_sizes ;
      _qubit_order ;
      _number_of_qubits ;
      _qubit_order_internal =
        (interval 0 (size-1))
        |> List.fold_left (fun m j ->
               LM.add m ((name, j), st._number_of_qubits + j))
             st._qubit_order_internal ;
      circuit_header = {
          st.circuit_header with
          n_qubits = _number_of_qubits ;
          qreg_sizes = _qreg_sizes ;
          qubit_labels = _qubit_order ;
        }
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
    let gate_instruction = Instruction.{
        name ;
        params = List.map Eval.expr params ;
        texparams = List.map Latex.expr params ;
        qubits = qubit_indices ;
        memory = clbit_indices ;
      } in
    {
      st with
      circuit_instructions = st.circuit_instructions @ [gate_instruction] ;
    }

  let to_json envs dag =
    let st = mk envs in
    let st = List.fold_left add_qreg st envs.TYCHK.Env.qregs in
    let st = List.fold_left add_creg st envs.TYCHK.Env.cregs in
    let nodel = DAG.tsort dag in
    let st =
      List.fold_left (fun st n ->
          match (LM.map dag.node_info n).label with
          | INPUT _| OUTPUT _ -> st
          | STMT stmt -> add_stmt st stmt)
        st nodel in
    let config = ExperimentConfig.{
        n_qubits = LM.fold (fun acc (_, dim) -> acc + dim) 0 envs.TYCHK.Env.qregs ;
        memory_slots = LM.fold (fun acc (_, dim) -> acc + dim) 0 envs.TYCHK.Env.cregs ;
      } in
    let ast = DAG.to_ast envs dag in
    let qasm = pp ASTPP.main ("2.0", ast) in
    let header = {
        st.circuit_header with
        compiled_circuit_qasm = Some qasm ;
      } in
    Experiment.{
        header ;
        instructions = st.circuit_instructions ;
        config = Some config ;
    }
end

module Compile = struct

  let circuit_to_experiment ~name envs dag =
    let circuit = JSON.to_json envs (fst dag) in
    {
      circuit with
      Experiment.header = {
        circuit.header with
        name = Some name ;
      }
    }

  let circuit_to_qobj ~backend_name ~shots ~max_credits ?qobj_id ?basis_gates ?coupling_map ?seed ~memory (envs, dag) =
    let config =
      Qobj.{
          n_qubits = 0 ;
          seed = (match seed with Some n -> n | None -> 1) ;
          max_credits ;
          memory_slots = 0 ;
          shots ;
          memory ;
      } in
    let qobj_id = match qobj_id with
      | Some id -> id
      | None -> Uuidm.(to_string (v `V4)) in
    let header = Qobj.{
        backend_name ;
        backend_version = None ;
                 } in
    let experiment = JSON.to_json envs dag in
    let qobj =
      Qobj.{
          qobj_id ;
          config ;
          experiments = [experiment] ;
          header ;
          _type = QASM ;
          schema_version = Defaults._QOBJ_VERSION ;
      } in
    ()
end
