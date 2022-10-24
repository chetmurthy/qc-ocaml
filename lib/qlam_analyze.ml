
open Misc_functions ;
open Pa_ppx_utils ;
open Pa_ppx_base ;
open Ppxutil ;
open Qc_misc ;
open Qlam_syntax ;
open Qlam_env ;
open Qlam_ops ;
open Qlam_tychk ;

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
    genv : GEnv.t
  ; qvars : QVMap.t BI.t
  ; layout : LO.t
  }
;

value mk genv lid =
  let layout = GEnv.find_layout genv lid in
 {
   genv = genv
 ; qvars = QVMap.empty
 ; layout = LO.mk layout
 } ;

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

end ;

(*
let interp lid (env_items, qc) =
  let genv = TYCHK.mk_genv env_items in
  let env = Env.mk genv lid in

  let rec irec env qc = match qc with [
  QLET of loc and list qbinding_t and qcirc_t
| QGATEAPP of loc and qgn_t and list pexpr_t and list qvar_t and list cvar_t
| (QWIRES loc qvl _
   | QBARRIER loc qvl
  | QMEASURE loc qvl
  | QRESET loc  qvl) ->
   List.map (Env.find_qvar env) qvl

| QBIT loc bi ->
   
of loc and BI.t | QDISCARD of loc and list qvar_t
      ] in
  irec env qc
 *)

end ;
