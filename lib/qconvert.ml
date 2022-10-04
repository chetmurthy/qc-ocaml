
open Pa_ppx_utils ;
open Std ;
open Pa_ppx_base ;
open Ppxutil ;

open Qc_misc ;
open Qlam_syntax ;

module ToLam = struct
open Qasm2syntax.AST ;

value conv_id = fun [ (CPARAMVAR s) -> PE.ID Ploc.dummy (PV Ploc.dummy (ID.mk s)) ] ;
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

value wrap_instr conv_pv ins rhs =
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
    | STMT_QOP(UOP (U pel q)) ->
      let qv = to_qvar q in
      let pel = List.map (param conv_pv) pel in
      let gateapp = QC.QGATEAPP Ploc.dummy (QC.QGATE Ploc.dummy (QC.QG Ploc.dummy (ID.mk "U"))) pel [qv] [] in
      QC.QLET Ploc.dummy [([qv], [], gateapp)] rhs
    | STMT_QOP(UOP (CX q1 q2)) ->
      let qv1 = to_qvar q1 in
      let qv2 = to_qvar q2 in
      let gateapp = QC.QGATEAPP Ploc.dummy (QC.QGATE Ploc.dummy (QC.QG Ploc.dummy (ID.mk "CX"))) [] [qv1;qv2] [] in
      QC.QLET Ploc.dummy [([qv1;qv2], [], gateapp)] rhs
    | STMT_QOP(UOP (COMPOSITE_GATE gname pel ql)) ->
      let qvl = List.map to_qvar ql in
      let pel = List.map (param conv_pv) pel in
      let gateapp = QC.QGATEAPP Ploc.dummy (QC.QGATE Ploc.dummy (QC.QG Ploc.dummy (ID.mk gname))) pel qvl [] in
      QC.QLET Ploc.dummy [(qvl, [], gateapp)] rhs
    ]
;

value circuit conv_pv qubits clbits instrs =
  let rhs = QC.QWIRES Ploc.dummy qubits clbits in
  List.fold_right (wrap_instr conv_pv) (List.map snd instrs) rhs
;

value env_item = fun [
  STMT_GATEDECL (gname, pvl, qvl, instrs) ->
  assert False
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
    qregs
    |> List.concat_map (fun [
      (_,STMT_CREG s count) ->
      (interval 0 (count-1))
      |> List.map (fun n -> QC.CV Ploc.dummy (ID.mk0 s n)) ]) in
  circuit empty qubits clbits instrs
;

end ;

