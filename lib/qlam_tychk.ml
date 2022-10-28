
open Pa_ppx_utils ;
open Pa_ppx_base ;
open Ppxutil ;
open Qc_misc ;
open Qlam_misc ;
open Qlam_syntax ;
open Qlam_syntax.SYN ;
open Qlam_env ;
open Qlam_ops ;

module Env = struct

type qvar_binding_t = {
    used : mutable bool
  ; loc : Ploc.t
  ; it : option qbinding_t
  } ;

type cvar_binding_t = option qbinding_t ;
type pvar_binding_t = unit ;

type t = {
    genv : GEnv.t (qgateargs_t * (int * int))
  ; qvars : QVMap.t qvar_binding_t
  ; cvars : CVMap.t cvar_binding_t
  ; pvars : PVMap.t pvar_binding_t
  }
;

value mk genv = {
  genv = genv
; qvars = QVMap.empty
; cvars = CVMap.empty
; pvars = PVMap.empty
} ;

value add_qvar loc env (id, qvb) =
  { (env) with qvars = QVMap.add id qvb env.qvars }
;

value add_cvar loc env (id, cv) =
  { (env) with cvars = CVMap.add id cv env.cvars }
;

value add_pvar loc env (id, pv) =
  { (env) with pvars = PVMap.add id pv env.pvars }
;

value has_gate loc env id = GEnv.has_gate loc env.genv id ;
value has_qvar loc env id = QVMap.mem id env.qvars ;
value has_cvar loc env id = CVMap.mem id env.cvars ;
value has_pvar loc env id = PVMap.mem id env.pvars ;

value find_mach ?{loc=Ploc.dummy} env mid = GEnv.find_mach ~{loc=loc} env.genv mid ;
value find_gate ?{loc=Ploc.dummy} env gid = GEnv.find_gate ~{loc=loc} env.genv gid ;

value find_qvar ?{loc=Ploc.dummy} env qid = match QVMap.find qid env.qvars with [
  x -> x
| exception Not_found ->
   Fmt.(raise_failwithf loc "find_qvar: qvar %a not found" QV.pp_hum qid)
] ;

value find_cvar ?{loc=Ploc.dummy} env cid = match CVMap.find cid env.cvars with [
  x -> x
| exception Not_found ->
   Fmt.(raise_failwithf loc "find_cvar: cvar %a not found" CV.pp_hum cid)
] ;

value find_pvar ?{loc=Ploc.dummy} env pid = match PVMap.find pid env.pvars with [
  x -> x
| exception Not_found ->
   Fmt.(raise_failwithf loc "find_pvar: pvar %a not found" PV.pp_hum pid)
] ;

end ;

value qvar_find_mark_used loc env qv =
  match Env.find_qvar ~{loc=loc} env qv with [
      exception Not_found ->
                Fmt.(raise_failwithf loc "circuit: undeclared qvar %a" QV.pp_hum qv)
    | x -> if x.used then
             Fmt.(raise_failwithf loc "circuit: qvar %a used more than once" QV.pp_hum qv)
           else x.used := True ]
;

value cvar_find loc env cv = 
  if not (Env.has_cvar loc env cv) then
    Fmt.(raise_failwithf loc "circuit: undeclared cvar %a" CV.pp_hum cv)
  else ()
;

value rec circuit env qc = match qc with [
  QWIRES loc qvl cvl -> do {
    qvl |> List.iter (qvar_find_mark_used loc env) ;
    cvl |> List.iter (cvar_find loc env) ;
    (List.length qvl, List.length cvl)
  }

| QCREATE _ _ -> (1, 0)
| QDISCARD loc qvl -> do {
    qvl |> List.iter (qvar_find_mark_used loc env) ;
    (0,0)
  }
| QBARRIER loc qvl -> do {
    qvl |> List.iter (qvar_find_mark_used loc env) ;
    (List.length qvl,0)
  }
| QRESET loc qvl -> do {
    qvl |> List.iter (qvar_find_mark_used loc env) ;
    (List.length qvl,0)
  }
| QMEASURE loc qvl -> do {
    qvl |> List.iter (qvar_find_mark_used loc env) ;
    (List.length qvl,List.length qvl)
  }
| QGATEAPP loc gn pal qal cal ->
   let ((pfl, qfl, cfl), ty) =
     match Env.find_gate ~{loc=loc} env gn with [
         exception Not_found ->
           Fmt.(raise_failwithf loc "gate-application: gate %a not found" QG.pp_hum gn)
       | x -> x
       ] in
   if List.length pal <> List.length pfl then
     Fmt.(raise_failwithf loc "circuit: gate-application %a with param-var length mismatch" QG.pp_hum gn)
   else if List.length qal <> List.length qal then
     Fmt.(raise_failwithf loc "circuit: gate-application %a with qvar length mismatch" QG.pp_hum gn)
   else if List.length cal <> List.length cal then
     Fmt.(raise_failwithf loc "circuit: gate-application %a with cvar length mismatch" QG.pp_hum gn)
   else do {
    qal |> List.iter (qvar_find_mark_used loc env) ;
    ty
  }
| QLET loc bl qc -> do {
    let qvars = bl |>  List.concat_map (fun (_, qvl, cvl, _) ->
      (List.map QV.toID qvl)) in
    if not (Std.distinct qvars) then
      Fmt.(raise_failwithf loc "TYCHK.circuit: qvars in binding MUST be distinct")
    else () ;
    let cvars = bl |>  List.concat_map (fun (_, qvl, cvl, _) ->
      (List.map CV.toID cvl)) in
    if not (Std.distinct cvars) then
      Fmt.(raise_failwithf loc "TYCHK.circuit: cvars in binding MUST be distinct")
    else () ;
    let bl =
      bl
      |>  List.map (fun ((loc, qvl,cvl,qc) as b) ->
              let (qlen, clen) = circuit env qc in
              if qlen <> List.length qvl then
                Fmt.(raise_failwithf loc "circuit: binding qvar length differs from circuit")
              else if clen <> List.length cvl then
                Fmt.(raise_failwithf loc "circuit: binding cvar length differs from circuit")
              else
                b) in
    let cv_bindings =
      bl |> List.concat_map (fun ((loc, qvl, cvl, qc) as b) ->
                cvl |> List.map (fun cv -> (cv, Some b))) in
    let qv_bindings =
      bl |> List.concat_map (fun ((loc, qvl, cvl, qc) as b) ->
                qvl |> List.map (fun qv -> (qv, { Env.used = False ; loc = loc ; it = Some b }))) in
    let env = List.fold_left (Env.add_cvar loc) env cv_bindings in
    let env = List.fold_left (Env.add_qvar loc) env qv_bindings in
    let ty = circuit env qc in
    qv_bindings
    |> List.iter (fun (qv, qvb) ->
           if not qvb.Env.used then
             Fmt.(raise_failwithf qvb.Env.loc "TYCHK.circuit: qvar %a not used" QV.pp_hum qv)
           else ()) ;
    ty
  }
] ;

value gather_qubits qc =
  let acc = ref [] in
  let open Qlam_migrate in
  let dt = make_dt() in
  let old_migrate_qcirc_t = dt.migrate_qcirc_t in
  let migrate_qcirc_t dt qc = match qc with [
        SYN.QCREATE _ u -> do { Std.push acc u ; qc }
      | _ -> old_migrate_qcirc_t dt qc
      ] in
  let dt = { (dt) with migrate_qcirc_t = migrate_qcirc_t } in do {
    ignore (dt.migrate_qcirc_t dt qc) ;
    acc.val
  }
;

value check_unique qc =
  if not (Std.distinct (gather_qubits qc)) then
    Fmt.(raise_failwithf (loc_of_qcirc qc) "check_unique: qubit() expressions are not unique")
  else ()
;

value top_circuit env qc = do {
    check_unique qc ;
    circuit env qc
}
;

value gate_item genv gitem = match gitem with [
  DEF loc gn (((pvl, qvl, cvl) as glam), qc) -> do {
    let (fv_pvs, fv_qvs, fv_cvs) = circuit_freevars qc in
    let fv_pvs = PVFVS.subtract fv_pvs (PVFVS.ofList pvl) in
    let fv_qvs = QVFVS.subtract fv_qvs (QVFVS.ofList qvl) in
    let fv_cvs = CVFVS.subtract fv_cvs (CVFVS.ofList cvl) in
    if PVFVS.mt <> fv_pvs then
      Fmt.(raise_failwithf loc "TYCHK.gate_item: gate %a has free param-vars %a" QG.pp_hum gn PVFVS.pp fv_pvs)
    else if QVFVS.mt <> fv_qvs then
      Fmt.(raise_failwithf loc "TYCHK.gate_item: gate %a has free qvars %a" QG.pp_hum gn QVFVS.pp fv_qvs)
    else if CVFVS.mt <> fv_cvs then
      Fmt.(raise_failwithf loc "TYCHK.gate_item: gate %a has free cvars %a" QG.pp_hum gn CVFVS.pp fv_cvs)
    else
    let env' = Env.mk genv in
    let qvbl = qvl |> List.map (fun qv -> (qv, { Env.used = False ; loc = loc ; it = None })) in
    let env' = List.fold_left (Env.add_qvar loc) env' qvbl in
    let env' = List.fold_left (fun env cv -> Env.add_cvar loc env (cv, None)) env' cvl in
    let env' = List.fold_left (fun env pv -> Env.add_pvar loc env (pv, ())) env' pvl in
    let ty = top_circuit env' qc in
    if not (qvbl |> List.for_all (fun (_, qvb) -> qvb.Env.used)) then
      Fmt.(raise_failwithf loc "gate_item: not all qvars were used (failure of linearity)")
    else
      (glam, ty)
  }
| OPAQUE loc gn ((pvl, qvl, cvl) as glam) ->
   let ty = (List.length qvl, List.length cvl) in
   (glam, ty)
] ;

value environ env_items = GEnv.mk_of_environ gate_item env_items ;

value program (env_items, qc) =
  let genv = environ env_items in
  let ty = top_circuit (Env.mk genv) qc in
  (genv, ty)
;
