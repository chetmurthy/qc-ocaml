open Pa_ppx_base ;
open Ppxutil ;
open Qc_misc ;
open Qlam_misc ;
open Qlam_syntax ;

module GEnv = struct
open SYN ;
type t = {
    gates : QGMap.t (qgateargs_t * (int * int))
  ; machs : IDMap.t SYN.CouplingMap.t
  ; layouts : IDMap.t SYN.Layout.t
  }
;

value mk () = {
  gates = QGMap.empty
; machs = IDMap.empty
; layouts = IDMap.empty
} ;

value mk_for_gate env = { (mk()) with gates = env.gates } ;

value add_gate loc env (gid, glam) =
  if QGMap.mem gid env.gates then
    Fmt.(raise_failwithf loc "add_gate: gate %a already in env" QG.pp_hum gid)
  else
    { (env) with gates = QGMap.add gid glam env.gates }
;

value has_gate loc env id = QGMap.mem id env.gates ;

value find_mach ?{loc=Ploc.dummy} env mid = match IDMap.find mid env.machs with [
  x -> x
| exception Not_found ->
   Fmt.(raise_failwithf loc "find_mach: machine %a not found" ID.pp_hum mid)
] ;

value find_gate ?{loc=Ploc.dummy} env gid = match QGMap.find gid env.gates with [
  x -> x
| exception Not_found ->
   Fmt.(raise_failwithf loc "find_gate: gate %a not found" QG.pp_hum gid)
] ;

end ;
