
open Pa_ppx_utils ;
open Std ;
open Pa_ppx_base ;
open Ppxutil ;

open Qc_misc ;
open Qlam_syntax ;

module ToLam = struct
open Qasm2syntax.AST ;

value conv_cparamvar = fun [ (CPARAMVAR s) -> PE.ID Ploc.dummy (PV Ploc.dummy (ID.mk s)) ] ;
value empty _ = assert False ;

value rec param conv_id = fun [
  ID cp -> conv_id cp
| REAL r -> PE.CONST Ploc.dummy (PC.REAL r) 
| NNINT n -> PE.CONST Ploc.dummy (PC.NNINT n)
| PI -> PE.CONST Ploc.dummy PC.PI
| ADD e1 e2 -> PE.(BINOP Ploc.dummy ADD (param conv_id e1) (param conv_id e2))
| SUB e1 e2 -> PE.(BINOP Ploc.dummy SUB (param conv_id e1) (param conv_id e2))
| DIV e1 e2 -> PE.(BINOP Ploc.dummy DIV (param conv_id e1) (param conv_id e2))
| MUL e1 e2 -> PE.(BINOP Ploc.dummy MUL (param conv_id e1) (param conv_id e2))
| POW e1 e2 -> PE.(BINOP Ploc.dummy POW (param conv_id e1) (param conv_id e2))
| UMINUS e -> PE.(UNOP Ploc.dummy UMINUS (param conv_id e))
| SIN e -> PE.(UFUN Ploc.dummy SIN (param conv_id e))
| COS e -> PE.(UFUN Ploc.dummy COS (param conv_id e))
| TAN e -> PE.(UFUN Ploc.dummy TAN (param conv_id e))
| EXP e -> PE.(UFUN Ploc.dummy EXP (param conv_id e))
| LN e -> PE.(UFUN Ploc.dummy LN (param conv_id e))
| SQRT e -> PE.(UFUN Ploc.dummy SQRT (param conv_id e))
] ;

value conv_qreg_or_indexed = fun [
      IT (QREG s) -> QC.QV Ploc.dummy (ID.mk s)
    | INDEXED (QREG s) n -> QC.QV Ploc.dummy (ID.mk0 s n)
    ]
;

value conv_qubit = fun [
      QUBIT s -> QC.QV Ploc.dummy (ID.mk s)
    ]
;

value wrap_uop conv_pv conv_qv ins rhs =
  match ins with [
      U pel q ->
      let qv = conv_qv q in
      let pel = List.map (param conv_pv) pel in
      let gateapp = QC.QGATEAPP Ploc.dummy (QC.QGATE Ploc.dummy (QC.QG Ploc.dummy (ID.mk "U"))) pel [qv] [] in
      QC.QLET Ploc.dummy [([qv], [], gateapp)] rhs
    | CX q1 q2 ->
      let qv1 = conv_qv q1 in
      let qv2 = conv_qv q2 in
      let gateapp = QC.QGATEAPP Ploc.dummy (QC.QGATE Ploc.dummy (QC.QG Ploc.dummy (ID.mk "CX"))) [] [qv1;qv2] [] in
      QC.QLET Ploc.dummy [([qv1;qv2], [], gateapp)] rhs
    | COMPOSITE_GATE gname pel ql ->
      let qvl = List.map conv_qv ql in
      let pel = List.map (param conv_pv) pel in
      let gateapp = QC.QGATEAPP Ploc.dummy (QC.QGATE Ploc.dummy (QC.QG Ploc.dummy (ID.mk gname))) pel qvl [] in
      QC.QLET Ploc.dummy [(qvl, [], gateapp)] rhs
    ]
;

value wrap_gate_op ins rhs =
  let to_qvar = fun [ (QUBIT s) -> QC.QV Ploc.dummy (ID.mk s) ] in
  match ins with [
      GATE_BARRIER ql ->
      let qvl = List.map to_qvar ql in
      QC.QLET Ploc.dummy [(qvl, [], QC.QBARRIER Ploc.dummy qvl)] rhs
    | GATE_UOP uop ->
       wrap_uop conv_cparamvar conv_qubit uop rhs
    ]
;

value gate_circuit qubits clbits instrs =
  let rhs = QC.QWIRES Ploc.dummy qubits clbits in
  List.fold_right wrap_gate_op (List.map snd instrs) rhs
;

value wrap_stmt conv_pv ins rhs =
  let to_qvar = fun [
        IT (QREG s) -> QC.QV Ploc.dummy (ID.mk s)
      | INDEXED (QREG s) n -> QC.QV Ploc.dummy (ID.mk0 s n)
      ] in
  let to_cvar = fun [
        IT (CREG s) -> QC.CV Ploc.dummy (ID.mk s)
      | INDEXED (CREG s) n -> QC.CV Ploc.dummy (ID.mk0 s n)
      ] in
  match ins with [
      STMT_BARRIER ql ->
      let qvl = List.map to_qvar ql in
      QC.QLET Ploc.dummy [(qvl, [], QC.QBARRIER Ploc.dummy qvl)] rhs
    | STMT_QOP(RESET q) ->
      let qv = to_qvar q in
      QC.QLET Ploc.dummy [([qv], [], QC.QRESET Ploc.dummy [qv])] rhs
    | STMT_QOP(MEASURE q c) ->
      let qv = to_qvar q in
      let cv = to_cvar c in
      QC.QLET Ploc.dummy [([qv], [cv], QC.QRESET Ploc.dummy [qv])] rhs
    | STMT_QOP(UOP uop) ->
       wrap_uop conv_pv conv_qreg_or_indexed uop rhs
    ]
;

value circuit qubits clbits instrs =
  let rhs = QC.QWIRES Ploc.dummy qubits clbits in
  List.fold_right (wrap_stmt empty) (List.map snd instrs) rhs
;

value env_item = fun [
  STMT_GATEDECL (gname, pvl, qvl, instrs) ->
   let pvl = List.map (fun s -> PV Ploc.dummy (ID.mk s)) pvl in
   let qvl = List.map (fun s -> QC.QV Ploc.dummy (ID.mk s)) qvl in
   let qc = gate_circuit qvl [] instrs in
   QEnv.QGATEDEF (QC.QG Ploc.dummy (ID.mk gname)) ((pvl, qvl, []), qc)


| STMT_OPAQUEDECL gname pvl qvl ->
   let pvl = List.map (fun s -> PV Ploc.dummy (ID.mk s)) pvl in
   let qvl = List.map (fun s -> QC.QV Ploc.dummy (ID.mk s)) qvl in
   QEnv.QGATEOPAQUE (QC.QG Ploc.dummy (ID.mk gname)) (pvl, qvl, [])
] ;

value program stmts =
  let (gates, instrs) =
    stmts |> filter_split (fun [ (_, (STMT_GATEDECL _ | STMT_OPAQUEDECL _ _ _)) -> True | _ -> False ]) in

  let env = List.map env_item (List.map snd gates) in
  let (regs, instrs) =
    instrs |>  filter_split (fun [ (_, (STMT_QREG _ _ | STMT_CREG _ _)) -> True | _ -> False ]) in
  let (qregs, cregs) =
    regs |> filter_split (fun [ (_, STMT_QREG _ _) -> True | (_, STMT_CREG _ _) -> False ]) in
  let qubits =
    qregs
    |> List.concat_map (fun [
      (_,STMT_QREG s count) ->
      (interval 0 (count-1))
      |> List.map (fun n -> QC.QV Ploc.dummy (ID.mk0 s n)) ]) in
  let clbits =
    cregs
    |> List.concat_map (fun [
      (_,STMT_CREG s count) ->
      (interval 0 (count-1))
      |> List.map (fun n -> QC.CV Ploc.dummy (ID.mk0 s n)) ]) in
  let qc = circuit qubits clbits instrs in
  (env, qc)
;

end ;

