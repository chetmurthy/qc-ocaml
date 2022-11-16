open Misc_functions ;
open Pa_ppx_utils ;
open Std ;
open Pa_ppx_base ;
open Ppxutil ;

open Qc_misc ;
open Qlam_misc ;
open Qlam_syntax ;
open Qlam_ops ;
open Qlam_env ;

module ToLam = struct
open Qasm2syntax.AST ;

value conv_cparamvar = fun [ (CPARAMVAR s) -> SYN.ID Ploc.dummy (PV Ploc.dummy (ID.mk s)) ] ;
value empty _ = assert False ;

value rec param conv_id = fun [
  ID cp -> conv_id cp
| REAL r -> SYN.CONST Ploc.dummy (SYN.REAL r) 
| NNINT n -> SYN.CONST Ploc.dummy (SYN.NNINT n)
| PI -> SYN.CONST Ploc.dummy SYN.PI
| ADD e1 e2 -> SYN.(BINOP Ploc.dummy ADD (param conv_id e1) (param conv_id e2))
| SUB e1 e2 -> SYN.(BINOP Ploc.dummy SUB (param conv_id e1) (param conv_id e2))
| DIV e1 e2 -> SYN.(BINOP Ploc.dummy DIV (param conv_id e1) (param conv_id e2))
| MUL e1 e2 -> SYN.(BINOP Ploc.dummy MUL (param conv_id e1) (param conv_id e2))
| POW e1 e2 -> SYN.(BINOP Ploc.dummy POW (param conv_id e1) (param conv_id e2))
| UMINUS e -> SYN.(UNOP Ploc.dummy UMINUS (param conv_id e))
| SIN e -> SYN.(UFUN Ploc.dummy SIN (param conv_id e))
| COS e -> SYN.(UFUN Ploc.dummy COS (param conv_id e))
| TAN e -> SYN.(UFUN Ploc.dummy TAN (param conv_id e))
| EXP e -> SYN.(UFUN Ploc.dummy EXP (param conv_id e))
| LN e -> SYN.(UFUN Ploc.dummy LN (param conv_id e))
| SQRT e -> SYN.(UFUN Ploc.dummy SQRT (param conv_id e))
] ;

value conv_qreg_or_indexed = fun [
      IT (QREG s) -> SYN.QV Ploc.dummy (ID.mk s)
    | INDEXED (QREG s) n -> SYN.QV Ploc.dummy (ID.mk0 s n)
    ]
;

value conv_qubit = fun [
      QUBIT s -> SYN.QV Ploc.dummy (ID.mk s)
    ]
;

value wrap_uop loc conv_pv conv_qv ins rhs =
  match ins with [
      U pel q ->
      let qv = conv_qv q in
      let pel = List.map (param conv_pv) pel in
      let gateapp = SYN.QGATEAPP Ploc.dummy (SYN.QG.ofID (ID.mk "U")) pel [qv] [] in
      SYN.QLET loc [(Ploc.dummy,  [qv], [], gateapp)] rhs
    | CX q1 q2 ->
      let qv1 = conv_qv q1 in
      let qv2 = conv_qv q2 in
      let gateapp = SYN.QGATEAPP Ploc.dummy (SYN.QG.ofID (ID.mk "CX")) [] [qv1;qv2] [] in
      SYN.QLET loc [(Ploc.dummy, [qv1;qv2], [], gateapp)] rhs
    | COMPOSITE_GATE gname pel ql ->
      let qvl = List.map conv_qv ql in
      let pel = List.map (param conv_pv) pel in
      let gateapp = SYN.QGATEAPP Ploc.dummy (SYN.QG.ofID (ID.mk gname)) pel qvl [] in
      SYN.QLET loc [(Ploc.dummy, qvl, [], gateapp)] rhs
    ]
;

value wrap_gate_op ins rhs =
  let to_qvar = fun [ (QUBIT s) -> SYN.QV Ploc.dummy (ID.mk s) ] in
  match ins with [
      (loc, GATE_BARRIER ql) ->
      let qvl = List.map to_qvar ql in
      SYN.QLET loc [(Ploc.dummy, qvl, [], SYN.QBARRIER Ploc.dummy qvl)] rhs
    | (loc, GATE_UOP uop) ->
       wrap_uop loc conv_cparamvar conv_qubit uop rhs
    ]
;

value gate_circuit qubits clbits instrs =
  let rhs = SYN.QWIRES Ploc.dummy qubits clbits in
  List.fold_right wrap_gate_op instrs rhs
;

value wrap_stmt conv_pv ins rhs =
  let to_qvar = fun [
        IT (QREG s) -> SYN.QV Ploc.dummy (ID.mk s)
      | INDEXED (QREG s) n -> SYN.QV Ploc.dummy (ID.mk0 s n)
      ] in
  let to_cvar = fun [
        IT (CREG s) -> SYN.CV Ploc.dummy (ID.mk s)
      | INDEXED (CREG s) n -> SYN.CV Ploc.dummy (ID.mk0 s n)
      ] in
  match ins with [
      (loc, STMT_BARRIER ql) ->
      let qvl = List.map to_qvar ql in
      SYN.QLET loc [(Ploc.dummy, qvl, [], SYN.QBARRIER Ploc.dummy qvl)] rhs
    | (loc, STMT_QOP(RESET q)) ->
      let qv = to_qvar q in
      SYN.QLET loc [(Ploc.dummy, [qv], [], SYN.QRESET Ploc.dummy qv)] rhs
    | (loc, STMT_QOP(MEASURE q c)) ->
      let qv = to_qvar q in
      let cv = to_cvar c in
      SYN.QLET loc [(Ploc.dummy, [qv], [cv], SYN.QMEASURE Ploc.dummy qv)] rhs
    | (loc, STMT_QOP(UOP uop)) ->
       wrap_uop loc conv_pv conv_qreg_or_indexed uop rhs
    ]
;

value circuit qubits clbits instrs =
  let rhs = SYN.QWIRES Ploc.dummy qubits clbits in
  List.fold_right (wrap_stmt empty) instrs rhs
;

value rec env_item = fun [
  (loc, STMT_GATEDECL (gname, pvl, qvl, instrs)) ->
   let pvl = List.map (fun s -> SYN.PV Ploc.dummy (ID.mk s)) pvl in
   let qvl = List.map (fun s -> SYN.QV Ploc.dummy (ID.mk s)) qvl in
   let qc = gate_circuit qvl [] instrs in
   SYN.QGATE loc (DEF loc (SYN.QG.ofID (ID.mk gname)) ((pvl, qvl, []), qc))

| (loc, STMT_OPAQUEDECL (gname, pvl, qvl)) ->
   let pvl = List.map (fun s -> SYN.PV Ploc.dummy (ID.mk s)) pvl in
   let qvl = List.map (fun s -> SYN.QV Ploc.dummy (ID.mk s)) qvl in
   SYN.QGATE loc (OPAQUE loc (SYN.QG.ofID (ID.mk gname)) (pvl, qvl, []))

| (loc, STMT_INCLUDE ty fname (Some l)) ->
   SYN.QINCLUDE loc ty fname (List.map env_item l)

| (loc, STMT_INCLUDE ty fname None) ->
   Fmt.(raise_failwithf loc "Qconvert.env_item: can only round-trip once with an include statement")
] ;

value expand_qregs loc qasm2env ql =
  let open Qasm2syntax.TYCHK in
  let lengths =
    ql |> List.filter_map (fun [
      INDEXED _ _ -> None
    | IT (QREG s) ->
       if not(Env.has_qreg qasm2env s) then
         Fmt.(raise_failwithf loc "qreg %s not declared in QASM2" s)
       else let n = Env.lookup_qreg qasm2env s in
            Some n
    ]) in
  let uniq_lengths = Std.uniquize lengths in
  if uniq_lengths = [] then
    [ql]
  else if List.length uniq_lengths > 1 then
    Fmt.(raise_failwithf loc "more than one length of qreg is forbidden")
  else
    let len = List.hd uniq_lengths in
    (Std.interval 0 (len-1))
    |> List.map (fun i ->
           ql |> List.map (fun [ IT (QREG s) -> INDEXED (QREG s) i | x -> x ])
         )
;

value expand_cregs loc qasm2env cl =
  let open Qasm2syntax.TYCHK in
  let lengths =
    cl |> List.filter_map (fun [
      INDEXED _ _ -> None
    | IT (CREG s) ->
       if not(Env.has_creg qasm2env s) then
         Fmt.(raise_failwithf loc "creg %s not declared in QASM2" s)
       else let n = Env.lookup_creg qasm2env s in
            Some n
    ]) in
  let uniq_lengths = Std.uniquize lengths in
  if uniq_lengths = [] then
    [cl]
  else if List.length uniq_lengths > 1 then
    Fmt.(raise_failwithf loc "more than one length of creg is forbidden")
  else
    let len = List.hd uniq_lengths in
    (Std.interval 0 (len-1))
    |> List.map (fun i ->
           cl |> List.map (fun [ IT (CREG s) -> INDEXED (CREG s) i | x -> x ])
         )
;

value expand_stmt qasm2env stmt = match stmt with [
  (loc,STMT_BARRIER ql) ->
  let qll = expand_qregs loc qasm2env ql in
  let ql = List.concat qll in
  [(loc, STMT_BARRIER ql)]

| (loc,STMT_QOP (MEASURE q c)) ->
   let qll = expand_qregs loc qasm2env [q] in
   let cll = expand_cregs loc qasm2env [c] in
   if List.length qll <> List.length cll then
     Fmt.(raise_failwithf loc "expand_stmt: qreg and creg arguments to measure of incompatible lengths")
   else
     let ll = Std.combine List.(map hd qll) List.(map hd cll) in
     ll |> List.map (fun (q,c) -> (loc, STMT_QOP (MEASURE q c)))

| (loc,STMT_QOP (RESET q)) ->
   let qll = expand_qregs loc qasm2env [q] in
   let l = List.(map hd qll) in
   l |> List.map (fun q -> (loc, STMT_QOP (RESET q)))

| (loc,STMT_QOP (UOP (U pel q))) ->
   let qll = expand_qregs loc qasm2env [q] in
   let l = List.(map hd qll) in
   l |> List.map (fun q -> (loc, STMT_QOP (UOP (U pel q))))

| (loc,STMT_QOP (UOP (CX q r))) ->
   let qll = expand_qregs loc qasm2env [q;r] in
   qll |> List.map (fun [ [q;r] -> (loc, STMT_QOP (UOP (CX q r))) ])

| (loc,STMT_QOP (UOP (COMPOSITE_GATE gn pel ql))) ->
   let qll = expand_qregs loc qasm2env ql in
   qll |> List.map (fun ql -> (loc, STMT_QOP (UOP (COMPOSITE_GATE gn pel ql))))

| x -> [x]
] ;

value env stmts =
  let qlam_env = List.map env_item stmts in
  qlam_env
;

value program (qasm2env, stmts) =
  let (gates, instrs) =
    stmts |> filter_split (fun [ (_, (STMT_INCLUDE _ _ _ | STMT_GATEDECL _ | STMT_OPAQUEDECL _)) -> True | _ -> False ]) in

  let qlam_env = List.map env_item gates in
  let (regs, instrs) =
    instrs |>  filter_split (fun [ (_, (STMT_QREG _ _ | STMT_CREG _ _)) -> True | _ -> False ]) in
  let (qregs, cregs) =
    regs |> filter_split (fun [ (_, STMT_QREG _ _) -> True | (_, STMT_CREG _ _) -> False ]) in
  let qubits_ids =
    qregs
    |> List.concat_map (fun [
      (_,STMT_QREG s count) ->
      (interval 0 (count-1))
      |> List.map (fun n -> SYN.QV Ploc.dummy (ID.mk0 s n)) ])
    |> List.mapi (fun i q -> (q,SYN.BI.EXPLICIT i)) in
  let clbits =
    cregs
    |> List.concat_map (fun [
      (_,STMT_CREG s count) ->
      (interval 0 (count-1))
      |> List.map (fun n -> SYN.CV Ploc.dummy (ID.mk0 s n)) ]) in
  let instrs = List.concat_map (expand_stmt qasm2env) instrs in
  let qc = circuit (List.map fst qubits_ids) clbits instrs in
  let qc = List.fold_right (fun (qv, id) rhs ->
               SYN.QLET Ploc.dummy [(Ploc.dummy, [qv], [], SYN.QCREATE Ploc.dummy id)] rhs)
             qubits_ids qc in
  (qlam_env, qc)
;

end ;

module ToQasm2 = struct

(**
  Convert from LQIR to qasm2:

  (0) ASSUMED: TYCHK/Fresh/ANorm/NameNorm/TYCHK the env+circuit

      WHY: b/c it's just easier to re-typecheck after normalizing.

  (1) compute bits for env+circuit (Ops.AssignBits)

  (2) to convert circuit:

  (2a) map bits to qreg/creg declarations

  (2b) convert instructions

  (3) to convert each gate definitions:

  (3a) set up vars as "bits"

  (3b) convert instructions

  (3c) build wrapper

 *)

open Qasm2syntax.AST ;
module AB = Qlam_ops.AssignBits ;
module CE = struct
type t 'a 'b = {
    quenv : SYN.QVMap.t 'a
  ; clenv : SYN.CVMap.t 'b
  ; qubit2reg : AB.QUBMap.t 'a
  ; clbit2reg : AB.CLBMap.t 'b
  } ;

value empty =
  {
    quenv = SYN.QVMap.empty
  ; clenv = SYN.CVMap.empty
  ; qubit2reg = AB.QUBMap.empty
  ; clbit2reg = AB.CLBMap.empty
  } ;

value lookup_qv ?{loc=Ploc.dummy} ce x =
  let open SYN in let open QV in
  match QVMap.find x ce.quenv with [
      exception Not_found ->
                Fmt.(raise_failwithf loc "lookup_qv: cannot find %a" pp_hum x)
    | x -> x
    ]
;

value lookup_cv ?{loc=Ploc.dummy} ce x =
  let open SYN in let open CV in
  match CVMap.find x ce.clenv with [
      exception Not_found ->
                Fmt.(raise_failwithf loc "lookup_cv: cannot find %a" pp_hum x)
    | x -> x
    ]
;

value upsert_qv ce (qv, qr) =
  { (ce) with quenv = SYN.QVMap.add qv qr ce.quenv } ;

value upsert_cv ce (cv, cr) =
  { (ce) with clenv = SYN.CVMap.add cv cr ce.clenv } ;

value lookup_qubit ?{loc=Ploc.dummy} ce x =
  match AB.QUBMap.find x ce.qubit2reg with [
      exception Not_found ->
                Fmt.(raise_failwithf loc "lookup_qubit: cannot find %a" AB.QUBit.pp_hum x)
    | x -> x
    ]
;

value lookup_clbit ?{loc=Ploc.dummy} ce x =
  match AB.CLBMap.find x ce.clbit2reg with [
      exception Not_found ->
                Fmt.(raise_failwithf loc "lookup_clbit: cannot find %a" AB.CLBit.pp_hum x)
    | x -> x
    ]
;
end ;

value make_uop loc gn pel qrl =
  let gn = gn |> SYN.QG.toID |> ID.unmk in
  match (gn, pel, qrl) with [
      ("U", pel, [qr]) -> U pel qr
    | ("CX", [], [qr1; qr2]) -> CX qr1 qr2
    | ("U", _, _) ->
       Fmt.(raise_failwithf loc "make_uop: malformed args to U")
    | ("CX", _, _) ->
       Fmt.(raise_failwithf loc "make_uop: malformed args to CX")
    | (gn, pel, qrl) ->
       COMPOSITE_GATE gn pel qrl
    ]
;

value conv_paramconst = fun [
  SYN.REAL r -> REAL r
| NNINT n -> NNINT n
| PI -> PI
] ;

value conv_empty pv : expr empty_t = match pv with [
  SYN.PV loc _ -> Fmt.(raise_failwithf loc "conv_param: variables forbidden here")
] ;
value conv_cparamvar pv = ID (CPARAMVAR (pv |> SYN.PV.toID |> ID.unmk)) ;

value conv_param conv_paramvar e =
  let rec crec = fun [
        SYN.ID _ pv -> conv_paramvar pv
      | CONST _ pc -> conv_paramconst pc
      | BINOP _ ADD pe1 pe2 -> ADD (crec pe1) (crec pe2)
      | BINOP _ SUB pe1 pe2 -> SUB (crec pe1) (crec pe2)
      | BINOP _ MUL pe1 pe2 -> MUL (crec pe1) (crec pe2)
      | BINOP _ DIV pe1 pe2 -> DIV (crec pe1) (crec pe2)
      | BINOP _ POW pe1 pe2 -> POW (crec pe1) (crec pe2)
      | UNOP _ UMINUS pe -> UMINUS (crec pe)
      | UFUN _ SIN pe -> SIN (crec pe)
      | UFUN _ COS pe -> COS (crec pe)
      | UFUN _ TAN pe -> TAN (crec pe)
      | UFUN _ EXP pe -> EXP (crec pe)
      | UFUN _ LN pe -> LN (crec pe)
      | UFUN _ SQRT pe -> SQRT (crec pe)
      ] in
  crec e
;

value qcircuit aenv env qc =
  let rec qbrec env (loc, qformals, cformals, qc) = match qc with [
    SYN.QWIRES loc qvl cvl ->
     let qrl = List.map (CE.lookup_qv ~{loc=loc} env) qvl in
     let crl = List.map (CE.lookup_cv ~{loc=loc} env) cvl in
     ((Std.combine qformals qrl, Std.combine cformals crl), [])

  | QGATEAPP loc gn pactuals qactuals cactuals ->
     if cactuals <> [] then
       Fmt.(raise_failwithf loc "qcircuit: unimplemented error: gates cannot take classical bits in qasm2 YET")
     else
     let qrl = List.map (CE.lookup_qv ~{loc=loc} env) qactuals in
     let crl = List.map (CE.lookup_cv ~{loc=loc} env) cactuals in
     let gate_result = GEnv.find_gate ~{loc=loc} aenv gn in
     let (gate_qresults, gate_cresults) = gate_result.AB.result in
     let (_, gate_qformals, gate_cformals) = gate_result.AB.args in
     if not (List.for_all2 SYN.QV.equal gate_qresults gate_qformals) then
       Fmt.(raise_failwithf loc "qcircuit: cannot convert LQIR gate to qasm that reorders qubits in output")
     else if not (List.for_all2 SYN.CV.equal gate_cresults gate_cformals) then
       Fmt.(raise_failwithf loc "qcircuit: cannot convert LQIR gate to qasm that reorders clbits in output")
     else
     let insn = 
       let pel = List.map (conv_param conv_empty) pactuals in
       (loc, STMT_QOP (UOP (make_uop loc gn pel qrl))) in
     ((Std.combine qformals qrl, Std.combine cformals crl), [insn])

  | QBARRIER _ qactuals ->
     let _ = assert (cformals = []) in
     let qrl = List.map (CE.lookup_qv ~{loc=loc} env) qactuals in
     let insn = (loc, STMT_BARRIER qrl) in
     ((Std.combine qformals qrl, []), [insn])

  | QCREATE _ bi ->
     let _ = assert (List.length qformals = 1) in
     let _ = assert (cformals = []) in
     let qr = CE.lookup_qubit ~{loc=loc} env (AB.QUBIT bi) in
     ((Std.combine qformals [qr], []), [])

  | QDISCARD _ _ -> (([], []), [])
  | QMEASURE loc qactual ->
     let _ = assert (1 = List.length qformals) in
     let _ = assert (1 = List.length cformals) in
     let qformal = List.hd qformals in
     let cformal = List.hd cformals in
     let qr = CE.lookup_qv ~{loc=loc} env qactual in
     let cr = CE.lookup_clbit ~{loc=loc} env (AB.CLBIT cformal) in
     let insns = [(loc, STMT_QOP (MEASURE qr cr))] in
     (([(qformal,qr)], [(cformal,cr)]), insns)

  | QRESET _ qv ->
     let _ = assert (1 = List.length qformals) in
     let _ = assert (cformals = []) in
     let qformal = List.hd qformals in
     let qr = CE.lookup_qv ~{loc=loc} env qformal in
     let insns = [(loc, STMT_QOP (RESET qr))] in
     (([(qformal,qr)], []),  insns)

      ]
  and qcrec env qc = match qc with [
    SYN.QLET _ bl qc ->
    let bl = List.map (qbrec env) bl in
    let _ = assert (Std.distinct (bl |> List.concat_map (fun ((ql, _), _) -> List.map fst ql))) in
    let _ = assert (Std.distinct (bl |> List.concat_map (fun ((_, cl), _) -> List.map fst cl))) in
    let env = List.fold_left (fun env ((ql, cl), _) ->
                  let env = List.fold_left CE.upsert_qv env ql in
                  let env = List.fold_left CE.upsert_cv env cl in
                  env)
                env bl in
    let bl_insns = List.concat_map snd bl in
    let (res, insns) = qcrec env qc in
    (res, bl_insns @ insns)

  | QWIRES loc qvl cvl ->
     let qrl = List.map (CE.lookup_qv ~{loc=loc} env) qvl in
     let crl = List.map (CE.lookup_cv ~{loc=loc} env) cvl in
     ((qrl, crl), [])

  | _ -> Fmt.(raise_failwithf (SYN.loc_of_qcirc qc) "circuit not in normal form")
  ] in
  qcrec env qc
;

value gate_qcircuit aenv env qc =
  let rec qbrec env (loc, qformals, cformals, qc) = match qc with [
    SYN.QWIRES loc qvl cvl ->
     let qrl = List.map (CE.lookup_qv ~{loc=loc} env) qvl in
     let crl = List.map (CE.lookup_cv ~{loc=loc} env) cvl in
     ((Std.combine qformals qrl, Std.combine cformals crl), [])

  | QGATEAPP loc gn pactuals qactuals cactuals ->
     if cactuals <> [] then
       Fmt.(raise_failwithf loc "qcircuit: unimplemented error: gates cannot take classical bits in qasm2 YET")
     else
     let qrl = List.map (CE.lookup_qv ~{loc=loc} env) qactuals in
     let crl = List.map (CE.lookup_cv ~{loc=loc} env) cactuals in
     let gate_result = GEnv.find_gate ~{loc=loc} aenv gn in
     let (gate_qresults, gate_cresults) = gate_result.AB.result in
     let (_, gate_qformals, gate_cformals) = gate_result.AB.args in
     if not (List.for_all2 SYN.QV.equal gate_qresults gate_qformals) then
       Fmt.(raise_failwithf loc "qcircuit: cannot convert LQIR gate to qasm that reorders qubits in output")
     else if not (List.for_all2 SYN.CV.equal gate_cresults gate_cformals) then
       Fmt.(raise_failwithf loc "qcircuit: cannot convert LQIR gate to qasm that reorders clbits in output")
     else
     let insn : gate_op_t loc = 
       let pel : list (expr cparamvar_t) = List.map (conv_param conv_cparamvar) pactuals in
       (loc, GATE_UOP (make_uop loc gn pel qrl)) in
     ((Std.combine qformals qrl, Std.combine cformals crl), [insn])

  | QBARRIER _ qactuals ->
     let _ = assert (cformals = []) in
     let qrl = List.map (CE.lookup_qv ~{loc=loc} env) qactuals in
     let insn = (loc, GATE_BARRIER qrl) in
     ((Std.combine qformals qrl, []), [insn])

  ]
  and qcrec env qc = match qc with [
    SYN.QLET _ bl qc ->
    let bl = List.map (qbrec env) bl in
    let _ = assert (Std.distinct (bl |> List.concat_map (fun ((ql, _), _) -> List.map fst ql))) in
    let _ = assert (Std.distinct (bl |> List.concat_map (fun ((_, cl), _) -> List.map fst cl))) in
    let env = List.fold_left (fun env ((ql, cl), _) ->
                  let env = List.fold_left CE.upsert_qv env ql in
                  let env = List.fold_left CE.upsert_cv env cl in
                  env)
                env bl in
    let bl_insns = List.concat_map snd bl in
    let (res, insns) = qcrec env qc in
    (res, bl_insns @ insns)

  | QWIRES loc qvl cvl ->
     let qrl = List.map (CE.lookup_qv ~{loc=loc} env) qvl in
     let crl = List.map (CE.lookup_cv ~{loc=loc} env) cvl in
     ((qrl, crl), [])

  | _ -> Fmt.(raise_failwithf (SYN.loc_of_qcirc qc) "circuit not in normal form")
  ] in
  qcrec env qc
;

value env_item aenv it = match it with [
  SYN.QINCLUDE loc QASM2 fn _ -> [(loc, STMT_INCLUDE QASM2 fn None)]
| QINCLUDE loc _ _ _ ->
   Fmt.(raise_failwithf loc "cannot convert QLAM include into QASM")
| QGATE _ (OPAQUE _ (U _) _) -> [] 
| QGATE _ (OPAQUE _ (CX _) _) -> []
| QGATE loc (DEF _ gn (_, qc)) ->
   let gate_result = GEnv.find_gate ~{loc=loc} aenv gn in
   let (gate_qresults, gate_cresults) = gate_result.AB.result in
   let (gate_pformals, gate_qformals, gate_cformals) = gate_result.AB.args in
   let _ = assert (gate_cformals = []) in
   let _ = assert (gate_cresults = []) in
   if not (List.for_all2 SYN.QV.equal gate_qresults gate_qformals) then
     Fmt.(raise_failwithf loc "qcircuit: cannot convert LQIR gate to qasm that reorders qubits in output")
   else if not (List.for_all2 SYN.CV.equal gate_cresults gate_cformals) then
     Fmt.(raise_failwithf loc "qcircuit: cannot convert LQIR gate to qasm that reorders clbits in output")
   else
   let env = CE.empty in
(*
   let qubit2reg =
     gate_qformals
     |>  List.map (fun qv -> (AB.QVAR qv, QUBIT (qv |> SYN.QV.toID |> ID.unmk)))
     |> AB.QUBMap.ofList in
   let clbit2reg = AB.CLBMap.empty in
   let env = { (env) with CE.qubit2reg = qubit2reg ; clbit2reg = clbit2reg } in
 *)
   let env = List.fold_left (fun  env qv -> CE.upsert_qv env (qv, QUBIT (qv |> SYN.QV.toID |> ID.unmk))) env gate_qformals in
   let (_, qc_insns) = gate_qcircuit aenv env qc in
   let pformals = gate_pformals |> List.map SYN.PV.toID |> List.map ID.unmk in
   let qformals = gate_qformals |> List.map SYN.QV.toID |> List.map ID.unmk in
   let qformals = gate_qformals |> List.map SYN.QV.toID |> List.map ID.unmk in
   [(loc, STMT_GATEDECL (gn |> SYN.QG.toID |> ID.unmk, pformals, qformals, qc_insns))]


| QCOUPLING_MAP loc mname _ -> do {
    Fmt.(pf stderr "%a: ToQasm2.env_item: coupling map %a skipped@.%!"
           Pa_ppx_runtime_fat.Exceptions.Ploc.pp loc
           ID.pp_hum mname) ;
    []
  }
| QLAYOUT loc mname _ -> do {
    Fmt.(pf stderr "%a: ToQasm2.env_item: layout %a skipped@.%!"
           Pa_ppx_runtime_fat.Exceptions.Ploc.pp loc
           ID.pp_hum mname) ;
    []
  }

| _ -> Fmt.(failwithf "Qconvert.ToQasm2.env_item: unexpected declaration %a" PP.item it)
] ;

value environ aenv gates =
  let gate_instrs = List.concat_map (env_item aenv) gates in
  gate_instrs ;

value program ?{env0=[]} (envitems, qc) =
  let genv0 = TYCHK.environ ~{env0=env0} envitems in
  let (gate_assign_env, qc_assign_env) = AssignBits.program genv0 ~{env0=env0} (envitems, qc) in
  let env_insns = environ gate_assign_env envitems in

  let qc_qubits = AB.Env.qubits qc_assign_env in
  let qreg_size = List.length qc_qubits in
  let qreg_name = "q" in
  let qreg_insns =
    if qreg_size <> 0 then
      [(Ploc.dummy, STMT_QREG qreg_name qreg_size)]
    else [] in
  let qubit2reg =
    qc_qubits
    |> List.mapi (fun n qb -> (qb, INDEXED (QREG qreg_name) n))
    |> AB.QUBMap.ofList in

  let qc_clbits = AB.Env.clbits qc_assign_env in
  let creg_size = List.length qc_clbits in
  let creg_name = "c" in
  let creg_insns =
    if creg_size <> 0 then
      [(Ploc.dummy, STMT_CREG creg_name creg_size)]
    else [] in
  let clbit2reg =
    qc_clbits
    |> List.mapi (fun n cb -> (cb, INDEXED (CREG creg_name) n))
    |> AB.CLBMap.ofList in
  
  let env = { (CE.empty) with CE.qubit2reg = qubit2reg; clbit2reg = clbit2reg } in
  let (_, qc_insns) = qcircuit gate_assign_env env qc in
  env_insns @ qreg_insns @ creg_insns @ qc_insns
;
end ;
