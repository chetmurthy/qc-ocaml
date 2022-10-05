
open Pa_ppx_base.Pp_MLast ;
open Qc_misc ;

value ident pps x = Fmt.(pf pps "%s" (ID.unmk x)) ;

type loc = Ploc.t ;
value loc_to_yojson (_ : loc) = `String "<loc>" ;
value equal_loc _ _ = True ;
value compare_loc _ _ = 0 ;

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
type cvar_t = [ CV of loc and ID.t ][@@deriving (to_yojson, show, eq, ord);] ;
value unCV = fun [ QV _ x -> x ] ;
type qgatename_t = [ QG of loc and ID.t ][@@deriving (to_yojson, show, eq, ord);] ;
value unQG = fun [ QG _ x -> x ] ;

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

value rec qcirc pps = fun [
    QLET _ bl qc ->
     Fmt.(pf pps "@[<v>let @[%a@] in@ %a@]" (list ~{sep=and_sep} binding) bl qcirc qc)
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
type item = [
  QGATEDEF of QC.qgatename_t and QC.qgatelam_t
| QGATEOPAQUE of QC.qgatename_t and QC.qgateargs_t
| QINCLUDE of string and t
  ]
and t = list item
[@@deriving (to_yojson, show, eq, ord);] ;

value item pps = fun [
    QGATEDEF gname ((pvl, qvl, cvl), qc) ->
    Fmt.(pf pps "@[<v 2>gate %a (%a) %a =@ %a@]@,;"
           QC.qgatename gname
           (list ~{sep=(const string ", ")} paramvar) pvl
           QC.qvars_cvars (qvl, cvl)
           QC.qcirc qc)
  | QGATEOPAQUE gname (pvl, qvl, cvl) ->
    Fmt.(pf pps "@[gate %a (%a) %a ;@]"
           QC.qgatename gname
           (list ~{sep=(const string ", ")} paramvar) pvl
           QC.qvars_cvars (qvl, cvl))
  | QINCLUDE s _ ->
     Fmt.(pf pps "include %a ;" (quote string) s)
] ;

value newline_sep pps () = Fmt.(pf pps "@.") ;

value pp pps l =
  Fmt.(pf pps "@[<v>%a@]" (list ~{sep=newline_sep} item) l) ;

end ;

type t = (QEnv.t * QC.t)[@@deriving (to_yojson, show, eq, ord);] ;

value pp pps (env, qc) =
  Fmt.(pf pps "%a@.%a%!" QEnv.pp env QC.qcirc qc) ;
