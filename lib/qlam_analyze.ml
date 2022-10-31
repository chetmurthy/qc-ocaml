
open Misc_functions ;
open Pa_ppx_utils ;
open Pa_ppx_base ;
open Ppxutil ;
open Qc_misc ;
open Qlam_syntax ;
open Qlam_env ;
open Qlam_ops ;

open SYN ;

module BasicLayout = struct
(** BasicLayout: given a circuit a layout, and a coupling-map, verify
    that the circuit can be executed with that layout.

    This module doesn't insert swaps; it merely checks that whatever
    gates have been inserted, can be executed on the hardware

    Method:


    Abstract interpretation: interpret the circuit, maintaining the
    environment mapping qvars to logical qubits, and separately a
    logical->physical layout.

 *)

module Env = struct

type qvar_binding_t = {
    used : mutable bool
  ; loc : Ploc.t
  ; it : option qbinding_t
  } ;

type cvar_binding_t = option qbinding_t ;
type pvar_binding_t = unit ;

type t = {
    genv : GEnv.t TYCHK.env_gate_t
  ; qvars : QVMap.t BI.t
  ; layout : LO.t
  ; coupling_map : CM.t
  }
;

value mk genv ~{layout=lid} ~{machine=mid} =
  let layout = GEnv.find_layout genv lid in
  let mach = GEnv.find_mach genv mid in
 {
   genv = genv
 ; qvars = QVMap.empty
 ; layout = LO.mk layout
 ; coupling_map = CM.mk mach
 } ;

value layout env = env.layout ;
value update_layout env l' = { (env) with layout = l' } ;

value add_qvar loc env (id, qvb) =
  { (env) with qvars = QVMap.add id qvb env.qvars }
;

value has_gate loc env id = GEnv.has_gate loc env.genv id ;
value has_qvar loc env id = QVMap.mem id env.qvars ;

value find_mach ?{loc=Ploc.dummy} env mid = GEnv.find_mach ~{loc=loc} env.genv mid ;
value find_gate ?{loc=Ploc.dummy} env gid = GEnv.find_gate ~{loc=loc} env.genv gid ;

value find_qvar ?{loc=Ploc.dummy} env qid = match QVMap.find qid env.qvars with [
  x -> x
| exception Not_found ->
   Fmt.(raise_failwithf loc "find_qvar: qvar %a not found" QV.pp_hum qid)
] ;

value logical_to_physical it lbit = LO.logical_to_physical it.layout lbit ;
value physical_to_logical it physbit = LO.physical_to_logical it.layout physbit ;
value has_pair env phy1 phy2 =
  match (phy1, phy2) with [
      (PQ.Physical phy1, PQ.Physical phy2) ->
      CM.has_pair env.coupling_map phy1 phy2
    ]
;

end ;

(** [interp ~layout ~machine (env_items, qc)] will return three things:

    (1) the set of logical qubits touched by this circuit
    (2) the set of physical qubits touched by this circuit
    (3) the list of logical qubits corresponding to the result-qubits of circuit
    (4)) the final layout after executing the circuit

    NOTE WELL that in a LET, since all bindings operate over disjoint sets of qubits
    (which we re-verify) we can safely/CORRECTLY pass the layout produced by one binding
    to the next.

 *)
value interp ~{layout=lid} ~{machine=mid} (env_items, qc) =
  let genv = TYCHK.environ env_items in
  let env = Env.mk genv ~{layout=lid} ~{machine=mid} in

  let rec irec env qc = match qc with [
    QLET loc bl qc ->
    let interp_binding (layout,logset0, physet0, newbl) (loc, qvl, cvl, qc) =
      let env = Env.update_layout env layout in
      let (logset, physet, lbits, layout) = irec env qc in
      if BISet.(mt <> (intersect logset0 logset)) then
        Fmt.(raise_failwithf loc "BasicLayout.interp: internal error: binding (in let) overlaps logical qubits with another binding: %a intersects %a"
               BISet.pp_hum logset0 BISet.pp_hum logset)
      else if PQSet.(mt <> (intersect physet0 physet)) then
        Fmt.(raise_failwithf loc "BasicLayout.interp: internal error: binding (in let) overlaps physical qubits with another binding: %a intersects %a"
               PQSet.pp_hum physet0 PQSet.pp_hum physet)
      else if List.length qvl <> List.length lbits then
        Fmt.(raise_failwithf loc "BasicLayout.interp: internal error: # binding vars differs from length of result of qcirc: %d <> %d"
               (List.length qvl) (List.length lbits))
      else
        (layout,
         BISet.(union logset0 logset),
         PQSet.(union physet0 physet),
         (Std.combine qvl lbits)@newbl) in
    let (layout, logset, physet, newbl) =
      List.fold_left interp_binding (Env.layout env, BISet.mt, PQSet.mt, []) bl in
    let env = Env.update_layout env layout in
    let (logset', physet', lbits, layout) = irec env qc in
    (BISet.(union logset logset'), PQSet.(union physet physet'), lbits, layout)

  | QGATEAPP _ (U _) [_;_;_] [qv] [] ->
     let bi = Env.find_qvar env qv in
     let phy = Env.logical_to_physical env bi in
     (BISet.ofList [bi], PQSet.ofList [phy], [bi], Env.layout env)

  | QGATEAPP loc (U _) _ _ _ ->
     Fmt.(raise_failwithf loc "BasicLayout.interp: internal error: U gate must have exactly 3 params, one qvar, zero cvars")

  | QGATEAPP loc (CX _) [] [qv1;qv2] [] ->
     let bi1 = Env.find_qvar env qv1 in
     let bi2 = Env.find_qvar env qv2 in
     let phy1 = Env.logical_to_physical env bi1 in
     let phy2 = Env.logical_to_physical env bi2 in
     if not(Env.has_pair env phy1 phy2) then
       Fmt.(raise_failwithf loc "BasicLayout.interp: CX gate cannot be applied to physical qubits %a -> %a"
              PQ.pp_hum phy1 PQ.pp_hum phy2)
     else
       (BISet.ofList [bi1;bi2], PQSet.ofList [phy1;phy2], [bi1;bi2], Env.layout env)

  | QGATEAPP loc (CX _) _ _ _ ->
     Fmt.(raise_failwithf loc "BasicLayout.interp: internal error: CX gate must have exactly zero params, two qvars, zero cvars")

  | QGATEAPP loc (SWAP _) [] [qv1;qv2] [] ->
     let bi1 = Env.find_qvar env qv1 in
     let bi2 = Env.find_qvar env qv2 in
     let phy1 = Env.logical_to_physical env bi1 in
     let phy2 = Env.logical_to_physical env bi2 in
     if not(Env.has_pair env phy1 phy2) then
       Fmt.(raise_failwithf loc "BasicLayout.interp: CX gate cannot be applied to physical qubits %a -> %a"
              PQ.pp_hum phy1 PQ.pp_hum phy2)
     else
       (BISet.ofList [bi1;bi2], PQSet.ofList [phy1;phy2], [bi1;bi2], Env.layout env)

  | QGATEAPP loc (SWAP _) _ _ _ ->
     Fmt.(raise_failwithf loc "BasicLayout.interp: internal error: SWAP gate must have exactly zero params, two qvars, zero cvars")


  | QGATEAPP _ (GENGATE _ _) _ [qv] [] ->
     let bi = Env.find_qvar env qv in
     let phy = Env.logical_to_physical env bi in
     (BISet.ofList [bi], PQSet.ofList [phy], [bi], Env.layout env)

  | QGATEAPP loc (GENGATE _ _) _ _ _ ->
     Fmt.(raise_failwithf loc "BasicLayout.interp: internal error: only one-qvar general gates are supported")

  | (QWIRES loc qvl _
    | QBARRIER loc qvl) ->
     let lbits = List.map (Env.find_qvar env) qvl in
     let logset = BISet.ofList lbits in
     let physet = PQSet.ofList (List.map (Env.logical_to_physical env) lbits) in
     (logset, physet, lbits, Env.layout env)

  | (QMEASURE loc qv
    | QRESET loc  qv) ->
     let lbits = List.map (Env.find_qvar env) [qv] in
     let logset = BISet.ofList lbits in
     let physet = PQSet.ofList (List.map (Env.logical_to_physical env) lbits) in
     (logset, physet, lbits, Env.layout env)

  | QCREATE loc bi ->
     let phy = Env.logical_to_physical env bi in
     (BISet.ofList [bi], PQSet.ofList [phy], [bi], Env.layout env)
   
  | QDISCARD _ qv ->
     let lbits = List.map (Env.find_qvar env) [qv] in
     let logset = BISet.ofList lbits in
     let physet = PQSet.ofList (List.map (Env.logical_to_physical env) lbits) in
     (logset, physet, [], Env.layout env)
   
  ] in
  irec env qc
;
end ;
