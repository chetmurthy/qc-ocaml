
open Qc_misc ;

value ident pps x = Fmt.(pf pps "%s" (ID.unmk x)) ;

module PC = struct

type t = [
    REAL of RealNumeral.t
  | NNINT of int
  | PI
  ][@@deriving (yojson, eq);]
;
value pp pps = fun [
    REAL s -> Fmt.(pf pps "%s" (RealNumeral.unmk s))
  | NNINT n -> Fmt.(pf pps "%d" n)
  | PI -> Fmt.(pf pps "pi")
    ]
;
end ;

type paramvar_t = [ PV of ID.t ][@@deriving (yojson, eq);] ;
value paramvar pps = fun [ (PV id) -> ident pps id ] ;

module PE = struct

type binop_t = [ ADD | SUB | MUL | DIV | POW ][@@deriving (yojson, eq);] ;
type unop_t = [ UMINUS ][@@deriving (yojson, eq);] ;
type ufun_t = [ SIN | COS | TAN | EXP | LN | SQRT ][@@deriving (yojson, eq);] ;

value string_of_ufun = fun [
    SIN -> "sin"
  | COS -> "cos"
  | TAN -> "tan"
  | EXP -> "exp"
  | LN -> "ln"
  | SQRT -> "sqrt"
] ;

type t = [
  ID of paramvar_t
| CONST of PC.t
| BINOP of binop_t and t and t
| UNOP of unop_t and t
| UFUN of ufun_t and t
  ][@@deriving (yojson, eq);]
;
value rec pp0 pps = fun [
  ID pv -> Fmt.(pf pps "%a" paramvar pv)
| CONST pc ->  PC.pp pps pc
| UFUN fsym pe ->
   Fmt.(pf pps "%s(%a)" (string_of_ufun fsym) pp pe)
| x -> Fmt.(pf pps "(%a)" pp x)
]

and pp1 pps = fun [
    UNOP UMINUS pe ->
    Fmt.(pf pps "- %a" pp1 pe)
  | x -> pp0 pps x
]

and pp2 pps = fun [
  BINOP POW pe1 pe2 ->
   Fmt.(pf pps "%a ** %a" pp1 pe1 pp2 pe2)
| x -> pp1 pps x
    ]

and pp3 pps = fun [
  BINOP MUL pe1 pe2 ->
   Fmt.(pf pps "%a * %a" pp3 pe1 pp2 pe2)
| BINOP DIV pe1 pe2 ->
   Fmt.(pf pps "%a / %a" pp3 pe1 pp2 pe2)
| x -> pp2 pps x
    ]

and pp4 pps = fun [
  BINOP ADD pe1 pe2 ->
   Fmt.(pf pps "%a + %a" pp4 pe1 pp3 pe2)
| BINOP SUB pe1 pe2 ->
   Fmt.(pf pps "%a - %a" pp4 pe1 pp3 pe2)
| x -> pp3 pps x
    ]
and pp pps x = pp4 pps x
;
end
;
module QC = struct
open Fmt ;

type qvar_t = [ QV of ID.t ][@@deriving (yojson, eq);] ;
type cvar_t = [ CV of ID.t ][@@deriving (yojson, eq);] ;
type qgatename_t = [ QG of ID.t ][@@deriving (yojson, eq);] ;

value qvar pps = fun [ (QV id) -> ident pps id ] ;
value cvar pps = fun [ (CV id) -> ident pps id ] ;
value qgatename pps = fun [ (QG id) -> ident pps id ] ;

type qgatelam_t = (list paramvar_t * list qvar_t * list cvar_t * t)

and qgate_t = [
  QGATELAM of qgatelam_t
| QGATE of qgatename_t
  ]
and t = [
  QLET of list qbinding_t and t
| QWIRES of list qvar_t and list cvar_t
| QGATEAPP of qgate_t and list PE.t and list qvar_t and list cvar_t
| QBARRIER of list qvar_t
| QBIT | QDISCARD of list qvar_t
| QMEASURE of list qvar_t
| QRESET of list qvar_t
  ]
and qbinding_t =
  (list qvar_t * list cvar_t * t)
[@@deriving (yojson, eq);] ;

value and_sep pps () = Fmt.(pf pps "@ and ") ;

value paren_qvars_cvars pps = fun [
    (qvl, []) ->
     Fmt.(pf pps "(%a)" (list ~{sep=(const string ", ")} qvar) qvl)
  | (qvl, cvl) ->
     Fmt.(pf pps "(%a : %a)" (list ~{sep=(const string ", ")} qvar) qvl  (list ~{sep=(const string ", ")} cvar) cvl)
] ;

value qvars_cvars pps = fun [
    (qvl, []) ->
     Fmt.(pf pps "%a" (list qvar) qvl)
  | (qvl, cvl) ->
     Fmt.(pf pps "%a : %a" (list qvar) qvl  (list cvar) cvl)
] ;

value rec qcirc pps = fun [
    QLET bl qc ->
     Fmt.(pf pps "@[<v>let @[%a@] in@ %a@]" (list ~{sep=and_sep} binding) bl qcirc qc)
  | QWIRES qvl cvl -> paren_qvars_cvars pps (qvl, cvl)
  | QGATEAPP qg [] qvl cvl ->
     Fmt.(pf  pps "%a %a" qgate qg qvars_cvars (qvl, cvl))
  | QGATEAPP qg pel qvl cvl ->
     Fmt.(pf  pps "%a (%a) %a" qgate qg (list ~{sep=(const string ", ")} PE.pp) pel qvars_cvars (qvl, cvl))
  | QBARRIER qvl -> Fmt.(pf pps "barrier %a" qvars_cvars (qvl, []))
  | QBIT -> Fmt.(pf pps "qubit()")
  | QDISCARD qvl -> Fmt.(pf pps "qdiscard %a" qvars_cvars (qvl, []))
  | QMEASURE qvl -> Fmt.(pf pps "measure %a" qvars_cvars (qvl, []))
  | QRESET qvl -> Fmt.(pf pps "reset %a" qvars_cvars (qvl, []))
]

and qgate pps = fun [
  QGATELAM (pvl, qvl,  cvl, qc) ->
    Fmt.(pf pps "gatefun [@[@ (%a)@ %a@ %a@]]"
           (list ~{sep=(const string ", ")} paramvar) pvl
           qvars_cvars (qvl, cvl)
           qcirc qc)

  | QGATE qg ->  Fmt.(pf pps "%a" qgatename qg)
]

and binding pps = fun [
    ([qv],  [], qc) ->
    Fmt.(pf pps "%a = %a" qvar qv qcirc qc)
  | (qvl,  cvl, qc) ->
    Fmt.(pf pps "%a = %a" paren_qvars_cvars (qvl,cvl) qcirc qc)
] ;
end ;

module QEnv = struct
type item = [
  QGATEDEF of QC.qgatename_t and QC.qgatelam_t
| QINCLUDE of string and t
  ]
and t = list item
[@@deriving (yojson, eq);] ;

value item pps = fun [
    QGATEDEF gname (pvl, qvl, cvl, qc) ->
    Fmt.(pf pps "gate %a (%a) %a = %a ;"
           QC.qgatename gname
           (list ~{sep=(const string ", ")} paramvar) pvl
           QC.qvars_cvars (qvl, cvl)
           QC.qcirc qc)
  | QINCLUDE s _ ->
     Fmt.(pf pps "include %a ;" (quote string) s)
] ;

value newline_sep pps () = Fmt.(pf pps "@.") ;

value pp pps l =
  Fmt.(pf pps "%a" (list ~{sep=newline_sep} item) l) ;

end ;

type t = (QEnv.t * QC.t)[@@deriving (yojson, eq);] ;

value pp pps (env, qc) =
  Fmt.(pf pps "%a@.%a%!" QEnv.pp env QC.qcirc qc) ;
