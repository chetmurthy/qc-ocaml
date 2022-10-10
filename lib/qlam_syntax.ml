open Misc_functions ;
open Pa_ppx_utils ;
open Pa_ppx_base ;
open Ppxutil ;
open Pa_ppx_base.Pp_MLast ;
open Qc_misc ;

value ident pps x = Fmt.(pf pps "%s" (ID.unmk x)) ;

type loc = Ploc.t ;
value loc_to_yojson (_ : loc) = `String "<loc>" ;
value equal_loc _ _ = True ;
value compare_loc _ _ = 0 ;

module SYN = struct
module PC = struct

type t = [
    REAL of RealNumeral.t
  | NNINT of int
  | PI
  ][@@deriving (to_yojson, show, eq, ord);]
;
value pp pps = fun [
    REAL s -> Fmt.(pf pps "%s" (RealNumeral.unmk s))
  | NNINT n -> Fmt.(pf pps "%d" n)
  | PI -> Fmt.(pf pps "pi")
    ]
;
end ;

type paramvar_t = [ PV of loc and ID.t ][@@deriving (to_yojson, show, eq, ord);] ;
value unPV = fun [ PV _ x -> x ] ;
module PVMap = Map.Make(struct type t= paramvar_t [@@deriving (eq, ord);]; end) ;

value paramvar pps = fun [ (PV _ id) -> ident pps id ] ;

module PE = struct

type binop_t = [ ADD | SUB | MUL | DIV | POW ][@@deriving (to_yojson, show, eq, ord);] ;
type unop_t = [ UMINUS ][@@deriving (to_yojson, show, eq, ord);] ;
type ufun_t = [ SIN | COS | TAN | EXP | LN | SQRT ][@@deriving (to_yojson, show, eq, ord);] ;

value string_of_ufun = fun [
    SIN -> "sin"
  | COS -> "cos"
  | TAN -> "tan"
  | EXP -> "exp"
  | LN -> "ln"
  | SQRT -> "sqrt"
] ;

type t = [
  ID of loc and paramvar_t
| CONST of loc and PC.t
| BINOP of loc and binop_t and t and t
| UNOP of loc and unop_t and t
| UFUN of loc and ufun_t and t
  ][@@deriving (to_yojson, show, eq, ord);]
;
value rec pp0 pps = fun [
  ID _ pv -> Fmt.(pf pps "%a" paramvar pv)
| CONST _ pc ->  PC.pp pps pc
| UFUN _ fsym pe ->
   Fmt.(pf pps "%s(%a)" (string_of_ufun fsym) pp pe)
| x -> Fmt.(pf pps "(%a)" pp x)
]

and pp1 pps = fun [
    UNOP _ UMINUS pe ->
    Fmt.(pf pps "- %a" pp1 pe)
  | x -> pp0 pps x
]

and pp2 pps = fun [
  BINOP _ POW pe1 pe2 ->
   Fmt.(pf pps "%a ** %a" pp1 pe1 pp2 pe2)
| x -> pp1 pps x
    ]

and pp3 pps = fun [
  BINOP _ MUL pe1 pe2 ->
   Fmt.(pf pps "%a * %a" pp3 pe1 pp2 pe2)
| BINOP _ DIV pe1 pe2 ->
   Fmt.(pf pps "%a / %a" pp3 pe1 pp2 pe2)
| x -> pp2 pps x
    ]

and pp4 pps = fun [
  BINOP _ ADD pe1 pe2 ->
   Fmt.(pf pps "%a + %a" pp4 pe1 pp3 pe2)
| BINOP _ SUB pe1 pe2 ->
   Fmt.(pf pps "%a - %a" pp4 pe1 pp3 pe2)
| x -> pp3 pps x
    ]
and pp pps x = pp4 pps x
;
end
;
module QC = struct
open Fmt ;

type qvar_t = [ QV of loc and ID.t ][@@deriving (to_yojson, show, eq, ord);] ;
value unQV = fun [ QV _ x -> x ] ;
module QVMap = Map.Make(struct type t= qvar_t [@@deriving (eq, ord);]; end) ;

type cvar_t = [ CV of loc and ID.t ][@@deriving (to_yojson, show, eq, ord);] ;
value unCV = fun [ QV _ x -> x ] ;
module CVMap = Map.Make(struct type t= cvar_t [@@deriving (eq, ord);]; end) ;

type qgatename_t = [ QG of loc and ID.t ][@@deriving (to_yojson, show, eq, ord);] ;
value unQG = fun [ QG _ x -> x ] ;
module QGMap = Map.Make(struct type t= qgatename_t [@@deriving (eq, ord);]; end) ;

value qvar pps = fun [ (QV _ id) -> ident pps id ] ;
value cvar pps = fun [ (CV _ id) -> ident pps id ] ;
value qgatename pps = fun [ (QG _ id) -> ident pps id ] ;

type qgatelam_t = (qgateargs_t * t)
and qgateargs_t = (list paramvar_t * list qvar_t * list cvar_t)

and qgate_t = [
  QGATELAM of loc and qgatelam_t
| QGATE of loc and qgatename_t
  ]
and t = [
  QLET of loc and list qbinding_t and t
| QWIRES of loc and list qvar_t and list cvar_t
| QGATEAPP of loc and qgate_t and list PE.t and list qvar_t and list cvar_t
| QBARRIER of loc and list qvar_t
| QBIT of loc | QDISCARD of loc and list qvar_t
| QMEASURE of loc and list qvar_t
| QRESET of loc and list qvar_t
  ]
and qbinding_t =
  (loc * list qvar_t * list cvar_t * t)
[@@deriving (to_yojson, show, eq, ord);] ;

value and_sep pps () = Fmt.(pf pps "@ and ") ;

value paren_qvars_cvars pps = fun [
    (qvl, []) ->
     Fmt.(pf pps "(%a)" (list ~{sep=(const string ", ")} qvar) qvl)
  | (qvl, cvl) ->
     Fmt.(pf pps "(%a : %a)" (list ~{sep=(const string ", ")} qvar) qvl  (list ~{sep=(const string ", ")} cvar) cvl)
] ;

value qvars_cvars pps = fun [
    (qvl, []) ->
     Fmt.(pf pps "%a" (list ~{sep=const string " "} qvar) qvl)
  | (qvl, cvl) ->
     Fmt.(pf pps "%a : %a" (list ~{sep=sp} qvar) qvl  (list ~{sep=const string " "} cvar) cvl)
] ;

value comm_nl pps = fun [
  "" -> Fmt.(pf pps "")
| s -> Fmt.(pf pps "%s@." (cleanws ~{lf=True} s))
] ;

value rec qcirc pps = fun [
    QLET loc bl qc ->
    let comm = Ploc.comment loc in
     Fmt.(pf pps "@[<v>%alet @[%a@] in@ %a@]" comm_nl comm (list ~{sep=and_sep} binding) bl qcirc qc)
  | QWIRES _ qvl cvl -> paren_qvars_cvars pps (qvl, cvl)
  | QGATEAPP _ qg [] qvl cvl ->
     Fmt.(pf  pps "%a %a" qgate qg qvars_cvars (qvl, cvl))
  | QGATEAPP _ qg pel qvl cvl ->
     Fmt.(pf  pps "%a (%a) %a" qgate qg (list ~{sep=(const string ", ")} PE.pp) pel qvars_cvars (qvl, cvl))
  | QBARRIER _ qvl -> Fmt.(pf pps "barrier %a" qvars_cvars (qvl, []))
  | QBIT _ -> Fmt.(pf pps "qubit()")
  | QDISCARD _ qvl -> Fmt.(pf pps "qdiscard %a" qvars_cvars (qvl, []))
  | QMEASURE _ qvl -> Fmt.(pf pps "measure %a" qvars_cvars (qvl, []))
  | QRESET _ qvl -> Fmt.(pf pps "reset %a" qvars_cvars (qvl, []))
]

and qgate pps = fun [
  QGATELAM _ ((pvl, qvl,  cvl), qc) ->
    Fmt.(pf pps "gatefun [@[@ (%a)@ %a@ %a@]]"
           (list ~{sep=(const string ", ")} paramvar) pvl
           qvars_cvars (qvl, cvl)
           qcirc qc)

  | QGATE _ qg ->  Fmt.(pf pps "%a" qgatename qg)
]

and binding pps = fun [
    (_, [qv],  [], qc) ->
    Fmt.(pf pps "%a = %a" qvar qv qcirc qc)
  | (_, qvl,  cvl, qc) ->
    Fmt.(pf pps "%a = %a" paren_qvars_cvars (qvl,cvl) qcirc qc)
] ;
end ;

module QEnv = struct

type gate_item = [
  DEF of QC.qgatename_t and QC.qgatelam_t
| OPAQUE of QC.qgatename_t and QC.qgateargs_t
  ][@@deriving (to_yojson, show, eq, ord);] ;

type item = [
  QGATE of loc and gate_item
| QINCLUDE of loc and file_type_t and string and t
]
and t = list item
[@@deriving (to_yojson, show, eq, ord);] ;

value gate_item pps = fun [
    DEF gname ((pvl, qvl, cvl), qc) ->
    Fmt.(pf pps "@[<v 2>gate %a (%a) %a =@ %a@]@,;"
           QC.qgatename gname
           (list ~{sep=(const string ", ")} paramvar) pvl
           QC.qvars_cvars (qvl, cvl)
           QC.qcirc qc)
  | OPAQUE gname (pvl, qvl, cvl) ->
    Fmt.(pf pps "@[gate %a (%a) %a ;@]"
           QC.qgatename gname
           (list ~{sep=(const string ", ")} paramvar) pvl
           QC.qvars_cvars (qvl, cvl))
] ;

value item pps = fun [
    QGATE _ gitem -> gate_item pps gitem
  | QINCLUDE _ _ s _ ->
     Fmt.(pf pps "include %a ;" (quote string) s)
] ;

value newline_sep pps () = Fmt.(pf pps "@.") ;

value pp pps l =
  Fmt.(pf pps "@[<v>%a@]" (list ~{sep=newline_sep} item) l) ;

end ;

type t = (QEnv.t * QC.t)[@@deriving (to_yojson, show, eq, ord);] ;

value pp pps (env, qc) =
  Fmt.(pf pps "%a@.%a%!" QEnv.pp env QC.qcirc qc) ;
end ;

module TYCHK = struct

module Env = struct

type qvar_binding_t = {
    used : mutable bool
  ; loc : Ploc.t
  ; it : option SYN.QC.qbinding_t
  } ;

type cvar_binding_t = option SYN.QC.qbinding_t ;
type pvar_binding_t = unit ;

type t = {
    gates : SYN.QC.QGMap.t (SYN.QC.qgateargs_t * (int * int))
  ; qvars : SYN.QC.QVMap.t qvar_binding_t
  ; cvars : SYN.QC.CVMap.t cvar_binding_t
  ; pvars : SYN.PVMap.t pvar_binding_t
  }
;

value mk () = {
  gates = SYN.QC.QGMap.empty
; qvars = SYN.QC.QVMap.empty
; cvars = SYN.QC.CVMap.empty
; pvars = SYN.PVMap.empty
} ;

value mk_for_gate env = { (mk()) with gates = env.gates } ;

value add_gate loc env (gid, glam) =
  if SYN.QC.QGMap.mem gid env.gates then
    Fmt.(raise_failwithf loc "add_gate: gate %a already in env" SYN.QC.pp_qgatename_t gid)
  else
    { (env) with gates = SYN.QC.QGMap.add gid glam env.gates }
;

value add_qvar loc env (id, qvb) =
  { (env) with qvars = SYN.QC.QVMap.add id qvb env.qvars }
;

value add_cvar loc env (id, cv) =
  { (env) with cvars = SYN.QC.CVMap.add id cv env.cvars }
;

value add_pvar loc env (id, pv) =
  { (env) with pvars = SYN.PVMap.add id pv env.pvars }
;

value has_gate loc env id = SYN.QC.QGMap.mem id env.gates ;
value has_qvar loc env id = SYN.QC.QVMap.mem id env.qvars ;
value has_cvar loc env id = SYN.QC.CVMap.mem id env.cvars ;
value has_pvar loc env id = SYN.PVMap.mem id env.pvars ;

value find_gate loc env gid = match SYN.QC.QGMap.find gid env.gates with [
  x -> x
| exception Not_found ->
   Fmt.(raise_failwithf loc "find_gate: gate %a not found" SYN.QC.pp_qgatename_t gid)
] ;

value find_qvar loc env qid = match SYN.QC.QVMap.find qid env.qvars with [
  x -> x
| exception Not_found ->
   Fmt.(raise_failwithf loc "find_qvar: qvar %a not found" SYN.QC.pp_qvar_t qid)
] ;

value find_cvar loc env cid = match SYN.QC.CVMap.find cid env.cvars with [
  x -> x
| exception Not_found ->
   Fmt.(raise_failwithf loc "find_cvar: cvar %a not found" SYN.QC.pp_cvar_t cid)
] ;

value find_pvar loc env pid = match SYN.PVMap.find pid env.pvars with [
  x -> x
| exception Not_found ->
   Fmt.(raise_failwithf loc "find_pvar: pvar %a not found" SYN.pp_paramvar_t pid)
] ;

end ;

value qvar_find_mark_used loc env qv =
  match Env.find_qvar loc env qv with [
      exception Not_found ->
                Fmt.(raise_failwithf loc "circuit: undeclared qvar %a" SYN.QC.pp_qvar_t qv)
    | x -> if x.used then
             Fmt.(raise_failwithf loc "circuit: qvar %a used more than once" SYN.QC.pp_qvar_t qv)
           else x.used := True ]
;

value cvar_find loc env cv = 
  if not (Env.has_cvar loc env cv) then
    Fmt.(raise_failwithf loc "circuit: undeclared cvar %a" SYN.QC.pp_cvar_t cv)
  else ()
;

value rec circuit env qc = match qc with [
  SYN.QC.QWIRES loc qvl cvl -> do {
    qvl |> List.iter (qvar_find_mark_used loc env) ;
    cvl |> List.iter (cvar_find loc env) ;
    (List.length qvl, List.length cvl)
  }

| QBIT _ -> (1, 0)
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
| QGATEAPP loc (QGATE _ gn) pal qal cal ->
   let ((pfl, qfl, cfl), ty) =
     match Env.find_gate loc env gn with [
         exception Not_found ->
           Fmt.(raise_failwithf loc "gate-application: gate %a not found" SYN.QC.pp_qgatename_t gn)
       | x -> x
       ] in
   if List.length pal <> List.length pfl then
     Fmt.(raise_failwithf loc "circuit: gate-application %a with param-var length mismatch" SYN.QC.pp_qgatename_t gn)
   else if List.length qal <> List.length qal then
     Fmt.(raise_failwithf loc "circuit: gate-application %a with qvar length mismatch" SYN.QC.pp_qgatename_t gn)
   else if List.length cal <> List.length cal then
     Fmt.(raise_failwithf loc "circuit: gate-application %a with cvar length mismatch" SYN.QC.pp_qgatename_t gn)
   else do {
    qal |> List.iter (qvar_find_mark_used loc env) ;
    ty
  }
| QLET loc bl qc ->
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
   let ty = circuit env qc in do {
    qv_bindings
    |> List.iter (fun (qv, qvb) ->
           if not qvb.Env.used then
             Fmt.(raise_failwithf qvb.Env.loc "TYCHK.circuit: qvar %a not used" SYN.QC.pp_qvar_t qv)
           else ()) ;
    ty
  }
] ;

value top_circuit env qc = circuit env qc ;

value gate_item loc env gitem = match gitem with [
  SYN.QEnv.DEF gn (((pvl, qvl, cvl) as glam), qc) ->
  let env' = Env.mk_for_gate env in
  let qvbl = qvl |> List.map (fun qv -> (qv, { Env.used = False ; loc = loc ; it = None })) in
  let env' = List.fold_left (Env.add_qvar loc) env' qvbl in
  let env' = List.fold_left (fun env cv -> Env.add_cvar loc env (cv, None)) env' cvl in
  let env' = List.fold_left (fun env pv -> Env.add_pvar loc env (pv, ())) env' pvl in
  let ty = top_circuit env' qc in
  if not (qvbl |> List.for_all (fun (_, qvb) -> qvb.Env.used)) then
    Fmt.(raise_failwithf loc "gate_item: not all qvars were used (failure of linearity)")
  else
    Env.add_gate loc env (gn, (glam, ty))

| OPAQUE gn ((pvl, qvl, cvl) as glam) ->
   let ty = (List.length qvl, List.length cvl) in
   Env.add_gate loc env (gn,  (glam, ty))
] ;


value rec env_item env ei = match ei with [
  SYN.QEnv.QINCLUDE loc _ fname l ->
   List.fold_left env_item env l
| QGATE loc gitem -> gate_item loc env gitem
] ;

value program (env_items, qc) =
  let env = Env.mk () in
  let env = List.fold_left env_item env env_items in
  let ty = top_circuit env qc in
  (env, ty)
;

end ;
