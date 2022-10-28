open Pa_ppx_base ;
open Ppxutil ;
open Qc_misc ;
open Qlam_misc ;
open Qlam_syntax ;

module GEnv = struct
open SYN ;
type t 'a = {
    gates : QGMap.t 'a
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

value find_gate ?{loc=Ploc.dummy} env gid = match QGMap.find gid env.gates with [
  x -> x
| exception Not_found ->
   Fmt.(raise_failwithf loc "find_gate: gate %a not found" QG.pp_hum gid)
] ;

value add_mach loc env (mid, mach) =
  if IDMap.mem mid env.machs then
    Fmt.(raise_failwithf loc "add_mach: mach %a already in env" ID.pp_hum mid)
  else
    { (env) with machs = IDMap.add mid mach env.machs }
;

value find_mach ?{loc=Ploc.dummy} env mid = match IDMap.find mid env.machs with [
  x -> x
| exception Not_found ->
   Fmt.(raise_failwithf loc "find_mach: machine %a not found" ID.pp_hum mid)
] ;

value add_layout loc env (lid, layout) =
  if IDMap.mem lid env.layouts then
    Fmt.(raise_failwithf loc "add_layout: layout %a already in env" ID.pp_hum lid)
  else
    { (env) with layouts = IDMap.add lid layout env.layouts }
;

value find_layout ?{loc=Ploc.dummy} env lid = match IDMap.find lid env.layouts with [
  x -> x
| exception Not_found ->
   Fmt.(raise_failwithf loc "find_layout: layoutine %a not found" ID.pp_hum lid)
] ;

value mk_of_environ gate_item env_items =
  let env = mk () in
  let rec env_item genv ei = match ei with [
        QINCLUDE loc _ fname l ->
        List.fold_left env_item genv l
      | QGATE loc gitem ->
         let gn = match gitem with [ DEF _ gn _ -> gn | OPAQUE _ gn _ -> gn ] in
         let rv = gate_item genv gitem in
         add_gate loc genv (gn,  rv)
      | QCOUPLING_MAP loc mname cm ->
         add_mach loc genv (mname, cm)
      | QLAYOUT loc lname l ->
         add_layout loc genv (lname, l)
      ] in
  List.fold_left env_item env env_items
;

value upgrade_environ gate_item genv0 env_items =
  let env = mk () in
  let rec env_item genv ei = match ei with [
        QINCLUDE loc _ fname l ->
        List.fold_left env_item genv l
      | QGATE loc gitem ->
         let gn = match gitem with [ DEF _ gn _ -> gn | OPAQUE _ gn _ -> gn ] in
         let rv0 = find_gate ~{loc=loc} genv0 gn in
         let rv = gate_item genv (rv0,gitem) in
         add_gate loc genv (gn,  rv)
      | QCOUPLING_MAP loc mname cm ->
         add_mach loc genv (mname, cm)
      | QLAYOUT loc lname l ->
         add_layout loc genv (lname, l)
      ] in
  List.fold_left env_item env env_items
;

end ;
