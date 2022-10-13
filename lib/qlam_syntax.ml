open Misc_functions ;
open Pa_ppx_utils ;
open Pa_ppx_base ;
open Ppxutil ;
open Pa_ppx_base.Pp_MLast ;
open Qc_misc ;
open Qlam_misc ;

value ident pps x = Fmt.(pf pps "%s" (ID.unmk x)) ;

type loc = Ploc.t ;
value loc_to_yojson (_ : loc) = `String "<loc>" ;
value equal_loc _ _ = True ;
value compare_loc _ _ = 0 ;

module SYN = struct

type const_t = [
    REAL of RealNumeral.t
  | NNINT of int
  | PI
  ][@@deriving (to_yojson, show, eq, ord);]
;

type pvar_t = [ PV of loc and ID.t ][@@deriving (to_yojson, show, eq, ord);] ;
module PV = struct
  type t = pvar_t[@@deriving (to_yojson, show, eq, ord);];
  value toID = fun [ PV _ x -> x ] ;
  value ofID x = PV Ploc.dummy x ;
  value to_loc = fun [ PV loc _ -> loc ] ;

  value pvar pps = fun [ (PV _ id) -> ident pps id ] ;
  value pp_hum pps x = Fmt.(pf pps "%a" pvar x) ;
end ;

module PVMap = VarMap(PV) ;
module PVFVS = FreeVarSet(PV) ;


type qvar_t = [ QV of loc and ID.t ][@@deriving (to_yojson, show, eq, ord);] ;
module QV = struct
  type t = qvar_t[@@deriving (to_yojson, show, eq, ord);];
  value toID = fun [ QV _ x -> x ] ;
  value ofID x = QV Ploc.dummy x ;
  value to_loc = fun [ QV loc _ -> loc ] ;
  value qvar pps = fun [ (QV _ id) -> ident pps id ] ;
  value pp_hum pps x = Fmt.(pf pps "%a" qvar x) ;
end ;
module QVMap = VarMap(QV) ;
module QVFVS = FreeVarSet(QV) ;

type cvar_t = [ CV of loc and ID.t ][@@deriving (to_yojson, show, eq, ord);] ;
module CV = struct
  type t = cvar_t[@@deriving (to_yojson, show, eq, ord);];
  value toID = fun [ CV _ x -> x ] ;
  value ofID x = CV Ploc.dummy x ;
  value to_loc = fun [ CV loc _ -> loc ] ;
  value cvar pps = fun [ (CV _ id) -> ident pps id ] ;
  value pp_hum pps x = Fmt.(pf pps "%a" cvar x) ;
end ;
module CVMap = VarMap(CV) ;
module CVFVS = FreeVarSet(CV) ;

type qgn_t = [ QG of loc and ID.t ][@@deriving (to_yojson, show, eq, ord);] ;
module QG = struct
  type t = qgn_t[@@deriving (to_yojson, show, eq, ord);];
  value toID = fun [ QG _ x -> x ] ;
  value ofID x = QG Ploc.dummy x ;
  value to_loc = fun [ QG loc _ -> loc ] ;
  value qgn pps = fun [ (QG _ id) -> ident pps id ] ;
  value pp_hum pps x = Fmt.(pf pps "%a" qgn x) ;
end ;
module QGMap = VarMap(QG) ;
module QGFVS = FreeVarSet(QG) ;

module PE = struct

type binop_t = [ ADD | SUB | MUL | DIV | POW ][@@deriving (to_yojson, show, eq, ord);] ;
type unop_t = [ UMINUS ][@@deriving (to_yojson, show, eq, ord);] ;
type ufun_t = [ SIN | COS | TAN | EXP | LN | SQRT ][@@deriving (to_yojson, show, eq, ord);] ;

type t = [
  ID of loc and pvar_t
| CONST of loc and const_t
| BINOP of loc and binop_t and t and t
| UNOP of loc and unop_t and t
| UFUN of loc and ufun_t and t
  ][@@deriving (to_yojson, show, eq, ord);]
;
end
;

module QC = struct
open Fmt ;

type qgatelam_t = (qgateargs_t * t)
and qgateargs_t = (list pvar_t * list qvar_t * list cvar_t)

and t = [
  QLET of loc and list qbinding_t and t
| QWIRES of loc and list qvar_t and list cvar_t
| QGATEAPP of loc and qgn_t and list PE.t and list qvar_t and list cvar_t
| QBARRIER of loc and list qvar_t
| QBIT of loc | QDISCARD of loc and list qvar_t
| QMEASURE of loc and list qvar_t
| QRESET of loc and list qvar_t
  ]
and qbinding_t =
  (loc * list qvar_t * list cvar_t * t)
[@@deriving (to_yojson, show, eq, ord);] ;

value loc_of_qcirc = fun [
  QLET loc _ _ -> loc
| QWIRES loc _ _ -> loc
| QGATEAPP loc _ _ _ _ -> loc
| QBARRIER loc _ -> loc
| QBIT loc -> loc
| QDISCARD loc _ -> loc
| QMEASURE loc _ -> loc
| QRESET loc _ -> loc
] ;
end ;

module QEnv = struct

type gate_item = [
  DEF of QG.t and QC.qgatelam_t
| OPAQUE of QG.t and QC.qgateargs_t
  ][@@deriving (to_yojson, show, eq, ord);] ;

type item = [
  QGATE of loc and gate_item
| QINCLUDE of loc and file_type_t and string and t
]
and t = list item
[@@deriving (to_yojson, show, eq, ord);] ;

end ;

type t = (QEnv.t * QC.t)[@@deriving (to_yojson, show, eq, ord);] ;
end ;

module PP = struct
open SYN ;

value pconst pps = fun [
    REAL s -> Fmt.(pf pps "%s" (RealNumeral.unmk s))
  | NNINT n -> Fmt.(pf pps "%d" n)
  | PI -> Fmt.(pf pps "pi")
  ]
;

value string_of_ufun = fun [
    PE.SIN -> "sin"
  | COS -> "cos"
  | TAN -> "tan"
  | EXP -> "exp"
  | LN -> "ln"
  | SQRT -> "sqrt"
] ;

value rec pexpr0 pps = fun [
  PE.ID _ pv -> Fmt.(pf pps "%a" PV.pp_hum pv)
| CONST _ pc ->  pconst pps pc
| UFUN _ fsym pe ->
   Fmt.(pf pps "%s(%a)" (string_of_ufun fsym) pexpr pe)
| x -> Fmt.(pf pps "(%a)" pexpr x)
]

and pexpr1 pps = fun [
    PE.UNOP _ UMINUS pe ->
    Fmt.(pf pps "- %a" pexpr1 pe)
  | x -> pexpr0 pps x
]

and pexpr2 pps = fun [
  PE.BINOP _ POW pe1 pe2 ->
   Fmt.(pf pps "%a ** %a" pexpr1 pe1 pexpr2 pe2)
| x -> pexpr1 pps x
    ]

and pexpr3 pps = fun [
  PE.BINOP _ MUL pe1 pe2 ->
   Fmt.(pf pps "%a * %a" pexpr3 pe1 pexpr2 pe2)
| BINOP _ DIV pe1 pe2 ->
   Fmt.(pf pps "%a / %a" pexpr3 pe1 pexpr2 pe2)
| x -> pexpr2 pps x
    ]

and pexpr4 pps = fun [
  PE.BINOP _ ADD pe1 pe2 ->
   Fmt.(pf pps "%a + %a" pexpr4 pe1 pexpr3 pe2)
| BINOP _ SUB pe1 pe2 ->
   Fmt.(pf pps "%a - %a" pexpr4 pe1 pexpr3 pe2)
| x -> pexpr3 pps x
    ]
and pexpr pps x = pexpr4 pps x
;

value and_sep pps () = Fmt.(pf pps "@ and ") ;

value paren_qvars_cvars pps = fun [
    (qvl, []) ->
     Fmt.(pf pps "(%a)" (list ~{sep=(const string ", ")} QV.pp_hum) qvl)
  | (qvl, cvl) ->
     Fmt.(pf pps "(%a : %a)" (list ~{sep=(const string ", ")} QV.pp_hum) qvl  (list ~{sep=(const string ", ")} CV.pp_hum) cvl)
] ;

value qvars_cvars pps = fun [
    (qvl, []) ->
     Fmt.(pf pps "%a" (list ~{sep=const string " "} QV.pp_hum) qvl)
  | (qvl, cvl) ->
     Fmt.(pf pps "%a : %a" (list ~{sep=sp} QV.pp_hum) qvl  (list ~{sep=const string " "} CV.pp_hum) cvl)
] ;

value comm_nl pps = fun [
  "" -> Fmt.(pf pps "")
| s -> Fmt.(pf pps "%s@." (cleanws ~{lf=True} s))
] ;

value rec qcirc pps = fun [
    QC.QLET loc bl qc ->
    let comm = Ploc.comment loc in
     Fmt.(pf pps "@[<v>%alet @[%a@] in@ %a@]" comm_nl comm (list ~{sep=and_sep} binding) bl qcirc qc)
  | QWIRES _ qvl cvl -> paren_qvars_cvars pps (qvl, cvl)
  | QGATEAPP _ qg [] qvl cvl ->
     Fmt.(pf  pps "%a %a" QG.pp_hum qg qvars_cvars (qvl, cvl))
  | QGATEAPP _ qg pel qvl cvl ->
     Fmt.(pf  pps "%a (%a) %a" QG.pp_hum qg (list ~{sep=(const string ", ")} PE.pp) pel qvars_cvars (qvl, cvl))
  | QBARRIER _ qvl -> Fmt.(pf pps "barrier %a" qvars_cvars (qvl, []))
  | QBIT _ -> Fmt.(pf pps "qubit()")
  | QDISCARD _ qvl -> Fmt.(pf pps "qdiscard %a" qvars_cvars (qvl, []))
  | QMEASURE _ qvl -> Fmt.(pf pps "measure %a" qvars_cvars (qvl, []))
  | QRESET _ qvl -> Fmt.(pf pps "reset %a" qvars_cvars (qvl, []))
]

and binding pps = fun [
    (_, [qv],  [], qc) ->
    Fmt.(pf pps "%a = %a" QV.pp_hum qv qcirc qc)
  | (_, qvl,  cvl, qc) ->
    Fmt.(pf pps "%a = %a" paren_qvars_cvars (qvl,cvl) qcirc qc)
] ;

value gate_item pps = fun [
    QEnv.DEF gname ((pvl, qvl, cvl), qc) ->
    Fmt.(pf pps "@[<v 2>gate %a (%a) %a =@ %a@]@,;"
           QG.pp_hum gname
           (list ~{sep=(const string ", ")} PV.pp_hum) pvl
           qvars_cvars (qvl, cvl)
           qcirc qc)
  | OPAQUE gname (pvl, qvl, cvl) ->
    Fmt.(pf pps "@[gate %a (%a) %a ;@]"
           QG.pp_hum gname
           (list ~{sep=(const string ", ")} PV.pp_hum) pvl
           qvars_cvars (qvl, cvl))
] ;

value item pps = fun [
    QEnv.QGATE _ gitem -> gate_item pps gitem
  | QINCLUDE _ _ s _ ->
     Fmt.(pf pps "include %a ;" (quote string) s)
] ;

value newline_sep pps () = Fmt.(pf pps "@.") ;

value env pps l =
  Fmt.(pf pps "@[<v>%a@]" (list ~{sep=newline_sep} item) l) ;


value top pps (env, qc) =
  Fmt.(pf pps "%a@.%a%!" QEnv.pp env qcirc qc) ;

end ;
