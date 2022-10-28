open Misc_functions ;
open Pa_ppx_utils ;
open Std ;
open Pa_ppx_base ;
open Ppxutil ;

open Qc_misc ;
open Qlam_syntax ;

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
      SYN.QLET loc [(Ploc.dummy, [qv], [], SYN.QRESET Ploc.dummy [qv])] rhs
    | (loc, STMT_QOP(MEASURE q c)) ->
      let qv = to_qvar q in
      let cv = to_cvar c in
      SYN.QLET loc [(Ploc.dummy, [qv], [cv], SYN.QMEASURE Ploc.dummy [qv])] rhs
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

| (loc, STMT_OPAQUEDECL gname pvl qvl) ->
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
    stmts |> filter_split (fun [ (_, (STMT_INCLUDE _ _ _ | STMT_GATEDECL _ | STMT_OPAQUEDECL _ _ _)) -> True | _ -> False ]) in

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
open Qasm2syntax.AST ;

module BC = struct
  type t = { s : string ; ctr : ref int } ;
  value mk s = { s=s ; ctr = ref 0 } ;
  value next bc =
    let n = bc.ctr.val in do {
      incr bc.ctr ;
      (bc.s, n)
  } ;
  value size bc = bc.ctr.val ;
end ;
module BitCounter = BC ;

open Asttools ;
module CE = struct
type t = {
    qubits : BC.t
  ; clbits : BC.t
  ; genv : IDMap.t (choice SYN.qgatelam_t SYN.qgateargs_t)
  ; qenv : IDMap.t (or_indexed qreg_t)
  ; clenv : IDMap.t (or_indexed creg_t)
  ; emit : bool
  } ;

value mk qbase clbase =
  {
    qubits = BC.mk qbase
  ; clbits = BC.mk clbase
  ; genv = IDMap.empty
  ; qenv = IDMap.empty
  ; clenv = IDMap.empty
  ; emit = True
  } ;

value qreg_size ce = BC.size ce.qubits ;
value clreg_size ce = BC.size ce.clbits ;
value next_qreg ce =
  let (s,n) = BC.next ce.qubits in
  INDEXED (QREG s) n
;

value next_creg ce =
  let (s,n) = BC.next ce.clbits in
  INDEXED (CREG s) n
;

value if_emit ce f = if ce.emit then f() else () ;
value stop_emit ce = { (ce) with emit = False } ;
value reset_vars ce = { (ce) with qenv = IDMap.empty ; clenv = IDMap.empty } ;

value lookup_gate ce gn =
  let x = SYN.QG.toID gn in
  match IDMap.find x ce.genv with [
      exception Not_found ->
                Fmt.(raise_failwithf (SYN.QG.to_loc gn) "lookup_gate: cannot find %s" (ID.unmk x))
    | x -> x
    ]
;

value lookup_qv ce = fun [
  (SYN.QV loc x) ->
  match IDMap.find x ce.qenv with [
      exception Not_found ->
                Fmt.(raise_failwithf loc "lookup_qv: cannot find %s" (ID.unmk x))
    | x -> x
    ]
] ;

value lookup_cv ce = fun [
  (SYN.CV loc x) ->
  match IDMap.find x ce.clenv with [
      exception Not_found ->
                Fmt.(raise_failwithf loc "lookup_cv: cannot find %s" (ID.unmk x))
    | x -> x
    ]
] ;

value upsert_gate ce (gn, glam) =
  { (ce) with genv = IDMap.add gn glam ce.genv } ;

value upsert_qv ce (qv, qr) =
  { (ce) with qenv = IDMap.add qv qr ce.qenv } ;

value upsert_cv ce (cv, cr) =
  { (ce) with clenv = IDMap.add cv cr ce.clenv } ;

end ;
module ConvEnv = CE ;

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
value conv_cparamvar pv = ID (SYN.PV.toID pv) ;

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

value rec conv_circuit loc0 acc env = fun [
  SYN.QLET loc bl qc ->
  let loc = ploc_encl_with_comments loc0 loc in
  let env = conv_bindings loc acc env bl in
  conv_circuit Ploc.dummy acc env qc
| QWIRES _ qvl cvl ->
   (List.map (CE.lookup_qv env) qvl, List.map (CE.lookup_cv env) cvl)
| QCREATE _ _ ->
   ([CE.next_qreg env], [])
| QDISCARD _ _ -> ([], [])
| QRESET loc qvl -> do {
    let loc = ploc_encl_with_comments loc0 loc in
    let qrl = List.map (CE.lookup_qv env) qvl in
    CE.if_emit env (fun () ->
        qrl |> List.iter (fun qr ->
                   Std.push acc (loc, STMT_QOP (RESET qr)))) ;
    (qrl, [])
  }
| QBARRIER loc qvl -> do {
    let loc = ploc_encl_with_comments loc0 loc in
    let qrl = List.map (CE.lookup_qv env) qvl in
    CE.if_emit env (fun () -> Std.push acc (loc, STMT_BARRIER qrl)) ;
    (qrl, [])
  }
| QMEASURE loc qvl -> do {
    let loc = ploc_encl_with_comments loc0 loc in
    let qrl = List.map (CE.lookup_qv env) qvl in
    let crl = List.map (fun _ -> CE.next_creg env) qrl in
    if List.length qrl <> List.length crl then
      Fmt.(raise_failwithf loc "conv_circuit: length mismatch in qreg/creg args to measure")
    else () ;
    (Std.combine qrl crl)
    |> List.iter (fun (qr, cr) ->
           CE.if_emit env (fun () -> Std.push acc (loc, STMT_QOP (MEASURE qr cr)))
         ) ;
    (qrl, crl)
  }
| QGATEAPP loc gn pel qvl cvl ->
    let loc = ploc_encl_with_comments loc0 loc in
   if cvl <> [] then
     Fmt.(raise_failwithf loc "conv_circuit: gates cannot take classical bits in qasm2")
   else do {
    let qrl = List.map (CE.lookup_qv env) qvl in
    CE.if_emit env (fun () ->
        let pel = List.map (conv_param conv_empty) pel in
        Std.push acc (loc, STMT_QOP (UOP (make_uop loc gn pel qrl)))) ;
    match CE.lookup_gate env gn with [
        Left ((_, qvl,  cvl), qc) ->
        let qvl = qvl |> List.map (fun  [ SYN.QV _ id -> id ]) in
        if List.length qrl <> List.length qvl then
          Fmt.(raise_failwithf loc "conv_circuit: length mismatch in qreg args to gate: %a" SYN.QG.pp gn)
        else if [] <> cvl then
          Fmt.(raise_failwithf loc "conv_circuit: composite gate %a should not accept classical bits" SYN.QG.pp gn)
        else
          let env = CE.reset_vars env in
          let env = List.fold_left CE.upsert_qv env (Std.combine qvl qrl) in
          let env = CE.stop_emit env in
          conv_circuit Ploc.dummy acc env qc
      | Right (_, qvl,  cvl) ->
         if List.length qrl <> List.length qvl then
           Fmt.(raise_failwithf loc "conv_circuit: length mismatch in qreg args to gate: %a" SYN.QG.pp gn)
         else if [] <> cvl then
           Fmt.(raise_failwithf loc "conv_circuit: opaque gate %a should not accept classical bits" SYN.QG.pp gn)
         else
           (qrl, [])
      ]
  }
]

and conv_bindings loc acc env bl =
 List.fold_left (conv_binding loc acc) env bl

and conv_binding loc0 acc env b =
  let (loc, qvl, cvl, qc) = b in
  let loc = ploc_encl_with_comments loc0 loc in
  let qvl = qvl |> List.map (fun  [ SYN.QV _ id -> id ]) in
  let cvl = cvl |> List.map (fun  [ SYN.CV _ id -> id ]) in
  let (qrl, crl) = conv_circuit loc acc env qc in
  if List.length qrl <> List.length qvl then
    Fmt.(raise_failwithf loc "conv_binding: length mismatch in qvars: %a" SYN.pp_qbinding_t b)
  else if List.length crl <> List.length cvl then
    Fmt.(raise_failwithf loc "conv_binding: length mismatch in clvars: %a" SYN.pp_qbinding_t b)
  else
    let env = List.fold_left CE.upsert_qv env (Std.combine qvl qrl) in
    let env = List.fold_left CE.upsert_cv env (Std.combine cvl crl) in
    env
;

value circuit (gates, qc) =
  let env = CE.mk "q" "c" in
  let env = List.fold_left CE.upsert_gate env gates in
  let acc = ref [] in do  {
    conv_circuit Ploc.dummy acc env qc ;
    let qreg_size = CE.qreg_size env in
    let clreg_size = CE.clreg_size env in
    (if clreg_size <> 0 then
       [(Ploc.dummy, STMT_CREG "c" clreg_size)]
     else [])
    @(if qreg_size <> 0 then
        [(Ploc.dummy, STMT_QREG "q" qreg_size)]
      else [])
    @(List.rev acc.val)
  }
;

value rec extract_gates env =
  env |>
    List.concat_map (fun [
      SYN.QGATE _ (DEF _ qg glam) -> [(SYN.QG.toID qg, Left glam)]
    | QGATE _ (OPAQUE _ qg gargs) -> [(SYN.QG.toID qg, Right gargs)]
    | QINCLUDE _ QASM2 _ l -> extract_gates l
    | QCOUPLING_MAP loc mname _ -> do {
        Fmt.(pf stderr "%a: ToQasm2.extract_gates: coupling map %a skipped@.%!"
               Pa_ppx_runtime_fat.Exceptions.Ploc.pp loc
               ID.pp_hum mname) ;
        []
    }
    | QLAYOUT loc mname _ -> do {
        Fmt.(pf stderr "%a: ToQasm2.extract_gates: layout %a skipped@.%!"
               Pa_ppx_runtime_fat.Exceptions.Ploc.pp loc
               ID.pp_hum mname) ;
        []
    }
    ])
;
(*
value env_item gates it = [] ;
 *)
value env_item gates it = match it with [
  SYN.QINCLUDE loc QASM2 fn _ -> [(loc, STMT_INCLUDE QASM2 fn None)]
| QINCLUDE loc _ _ _ ->
   Fmt.(raise_failwithf loc "cannot convert QLAM include into QASM")
| QGATE _ (OPAQUE _ (U _) _) -> [] 
| QGATE _ (OPAQUE _ (CX _) _) -> []
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

| _ -> Fmt.(failwithf "Qconvert.env_item: unexpected declaration %a" PP.item it)
] ;

value env gates =
  let gate_instrs = List.concat_map (env_item gates) gates in
  gate_instrs ;

value program (env, qc) =
  let gates = extract_gates env in
  let gate_instrs = List.concat_map (env_item gates) env in
  let instrs = circuit (gates, qc) in
  gate_instrs @ instrs ;

end ;

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

module ToQasm2' = struct
open Qasm2syntax.AST ;

end ;
