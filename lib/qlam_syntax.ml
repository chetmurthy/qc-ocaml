open Misc_functions ;
open Pa_ppx_utils ;
open Pa_ppx_base ;
open Ppxutil ;
open Pa_ppx_base.Pp_MLast ;
open Qc_misc ;
open Qlam_misc ;

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
  value ofID ?{loc=Ploc.dummy} x = PV loc x ;
  value to_loc = fun [ PV loc _ -> loc ] ;

  value pvar pps = fun [ (PV _ id) -> ID.pp_hum pps id ] ;
  value pp_hum pps x = Fmt.(pf pps "%a" pvar x) ;
end ;

module PVSet = VarSet(PV) ;
module PVMap = EntityMap(PV)(PVSet) ;

type qvar_t = [ QV of loc and ID.t ][@@deriving (to_yojson, show, eq, ord);] ;
module QV = struct
  type t = qvar_t[@@deriving (to_yojson, show, eq, ord);];
  value toID = fun [ QV _ x -> x ] ;
  value ofID ?{loc=Ploc.dummy} x = QV loc x ;
  value to_loc = fun [ QV loc _ -> loc ] ;
  value qvar pps = fun [ (QV _ id) -> ID.pp_hum pps id ] ;
  value pp_hum pps x = Fmt.(pf pps "%a" qvar x) ;
end ;
module QVSet = VarSet(QV) ;
module QVMap = EntityMap(QV)(QVSet) ;

type cvar_t = [ CV of loc and ID.t ][@@deriving (to_yojson, show, eq, ord);] ;
module CV = struct
  type t = cvar_t[@@deriving (to_yojson, show, eq, ord);];
  value toID = fun [ CV _ x -> x ] ;
  value ofID ?{loc=Ploc.dummy} x = CV loc x ;
  value to_loc = fun [ CV loc _ -> loc ] ;
  value cvar pps = fun [ (CV _ id) -> ID.pp_hum pps id ] ;
  value pp_hum pps x = Fmt.(pf pps "%a" cvar x) ;
end ;
module CVSet = VarSet(CV) ;
module CVMap = EntityMap(CV)(CVSet) ;

type qgn_t = [
    CX of loc | U of loc | SWAP of loc
  | GENGATE of loc and ID.t
 ][@@deriving (to_yojson, show, eq, ord);] ;
module QG = struct
  type t = qgn_t[@@deriving (to_yojson, show, eq, ord);];
  value toID = fun [
    CX _ -> ID.mk0 "CX" (-1)
  | U _ -> ID.mk0 "U" (-1)
  | SWAP _ -> ID.mk0 "SWAP" (-1)
  | GENGATE _ x -> x
  ] ;
  value ofID ?{loc=Ploc.dummy} = fun [
    ("CX",-1) -> CX loc
  | ("U",-1) -> U loc
  | ("SWAP",-1) -> SWAP loc
  | x -> GENGATE loc x
  ] ;
  value to_loc = fun [
    CX loc -> loc | U loc -> loc | SWAP loc -> loc
  | GENGATE loc _ -> loc
  ] ;
  value qgn pps g = ID.pp_hum pps (toID g) ;
  value pp_hum pps x = Fmt.(pf pps "%a" qgn x) ;
end ;
module QGSet = VarSet(QG) ;
module QGMap = EntityMap(QG)(QGSet) ;

type binop_t = [ ADD | SUB | MUL | DIV | POW ][@@deriving (to_yojson, show, eq, ord);] ;
type unop_t = [ UMINUS ][@@deriving (to_yojson, show, eq, ord);] ;
type ufun_t = [ SIN | COS | TAN | EXP | LN | SQRT ][@@deriving (to_yojson, show, eq, ord);] ;

type pexpr_t = [
  ID of loc and pvar_t
| CONST of loc and const_t
| BINOP of loc and binop_t and pexpr_t and pexpr_t
| UNOP of loc and unop_t and pexpr_t
| UFUN of loc and ufun_t and pexpr_t
  ][@@deriving (to_yojson, show, eq, ord);]
;

module Unique = struct
  value ctr = Counter.mk () ;
  type t = int [@@deriving (to_yojson, show, eq, ord);] ;
  value mk () = Counter.next ctr ;
end ;

module BI = struct
type t = [ UNIQUE of Unique.t | EXPLICIT of int ] [@@deriving (to_yojson, show, eq, ord);] ;
value pp_hum pps = fun [
  UNIQUE u -> Fmt.(pf pps "{unique %d}" u)
| EXPLICIT n -> Fmt.(pf pps "#%d" n)
] ;
end ;
module BISet = EntitySet(BI) ;
module BIMap = EntityMap(BI)(BISet) ;
module PQ = struct
  type t = [Physical of int] [@@deriving (to_yojson, show, eq, ord);] ;
  value toInt = fun [ Physical n -> n ] ;
  value ofInt n = Physical n ;
  value pp_hum pps pq = Fmt.(pf pps "{physical %d}" (toInt pq)) ;
end ;
module PQSet = EntitySet(PQ) ;
module PQMap = EntityMap(PQ)(PQSet) ;
module BitIdent = BI ;
module PhysicalQubit = PQ ;
module BI_Phys_BIJ = Bijection(BI)(BISet)(PQ)(PQSet) ;
type qgatelam_t = (qgateargs_t * qcirc_t)
and qgateargs_t = (list pvar_t * list qvar_t * list cvar_t)

and qcirc_t = [
  QLET of loc and list qbinding_t and qcirc_t
| QWIRES of loc and list qvar_t and list cvar_t
| QGATEAPP of loc and qgn_t and list pexpr_t and list qvar_t and list cvar_t
| QBARRIER of loc and list qvar_t
| QCREATE of loc and BI.t
| QDISCARD of loc and qvar_t
| QMEASURE of loc and qvar_t
| QRESET of loc and qvar_t
]
and qbinding_t =
  (loc * list qvar_t * list cvar_t * qcirc_t)
[@@deriving (to_yojson, show, eq, ord);] ;

value qbinding_loc (loc, _, _, _) = loc ;
value qbinding_qvl (_, qvl, _, _) = qvl ;
value qbinding_cvl (_, _, cvl, _) = cvl ;
value qbinding_qc (_, _, _, qc) = qc ;

value loc_of_qcirc = fun [
  QLET loc _ _ -> loc
| QWIRES loc _ _ -> loc
| QGATEAPP loc _ _ _ _ -> loc
| QBARRIER loc _ -> loc
| QCREATE loc _ -> loc
| QDISCARD loc _ -> loc
| QMEASURE loc _ -> loc
| QRESET loc _ -> loc
] ;

value is_qlet = fun [ QLET _ _ _ -> True | _ -> False ] ;

value to_letlist qc =
  let rec torec acc qc = match qc with [
        QLET loc bl qc ->
        torec [(loc, bl) :: acc] qc
      | qc -> (acc, qc)
      ] in
  let (acc, qc) = torec [] qc in
  (List.rev acc, qc)
;

value of_letlist (ll, qc) =
  List.fold_right (fun (loc, bl) qc -> QLET loc bl qc) ll qc ;

module CouplingMap = struct
type direction_t = [ LR | RL | BIDI ] ;

type t =
  {
    edges : list (int * int)
  ; positions : list (int * (int * int))
  }
  [@@deriving (to_yojson, show, eq, ord);]
;
value mk edges positions =
  let edges = edges |> List.concat_map (fun [
          (n,LR,m) -> [(n,m)]
        | (n,RL,m) -> [(m,n)]
        | (n,BIDI,m) -> [(n,m); (m,n)]
        ]) in
  {edges=edges; positions=positions} ;

value pp_hum pps = fun [
  {edges=edges; positions=[]} ->
  Fmt.(pf pps "[ %a ]"
         (list ~{sep=sp} (pair ~{sep=const string "->"} int int)) edges)
| {edges=edges; positions=positions} ->
   Fmt.(pf pps "[ %a ; %a ]"
          (list ~{sep=sp} (pair ~{sep=const string "->"} int int)) edges
          (list ~{sep=const string " "} (pair ~{sep=const string "@"} int (parens (pair ~{sep=const string ","} int int)))) positions
   )
] ;

value from_core_config cc =
  let open Qrpc_types.CoreConfig in
  let m = match cc.coupling_map  with [
        None -> Fmt.(failwithf "CouplingMap.from_core_config: CoreConfig did not contain coupling map")
      | Some m -> m
      ] in
  let edges = m |> List.map (fun [
              [n;m] -> (n,m)
            | l -> Fmt.(failwithf "CouplingMap.from_core_config: invalid edge spec %a"
                          (brackets (list ~{sep=const string "; "} int)) l)
            ]) in
  (edges, []) ;

end ;

module Layout = struct
  type t = { it : list (BI.t * PQ.t) }[@@deriving (to_yojson, show, eq, ord);] ;
  value mk l = { it = l } ;

  value pp_hum pps { it = it } =
    Fmt.(pf pps "[ %a ]" (list ~{sep=const string ", "} (pair ~{sep=const string " : "} BI.pp_hum PQ.pp_hum)) it)
  ;

end ;

type gate_item = [
  DEF of loc and qgn_t and qgatelam_t
| OPAQUE of loc and qgn_t and qgateargs_t
  ][@@deriving (to_yojson, show, eq, ord);] ;

type item = [
  QGATE of loc and gate_item
| QINCLUDE of loc and file_type_t and string and environ_t
| QCOUPLING_MAP of loc and ID.t and CouplingMap.t
| QLAYOUT of loc and ID.t and Layout.t
]
and environ_t = list item
[@@deriving (to_yojson, show, eq, ord);] ;

value loc_of_gate_item = fun [
  DEF loc _ _ -> loc
| OPAQUE loc _ _ -> loc
] ;

value args_of_gate_item = fun [
  DEF _ _ (args, _) -> args
| OPAQUE _ _ args -> args
] ;

value name_of_gate_item = fun [
  DEF _ gn _ -> gn
| OPAQUE _ gn _ -> gn
] ;

type program_t = (environ_t * qcirc_t)[@@deriving (to_yojson, show, eq, ord);] ;

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
    SIN -> "sin"
  | COS -> "cos"
  | TAN -> "tan"
  | EXP -> "exp"
  | LN -> "ln"
  | SQRT -> "sqrt"
] ;

value rec pexpr0 pps = fun [
  ID _ pv -> Fmt.(pf pps "%a" PV.pp_hum pv)
| CONST _ pc ->  pconst pps pc
| UFUN _ fsym pe ->
   Fmt.(pf pps "%s(%a)" (string_of_ufun fsym) pexpr pe)
| x -> Fmt.(pf pps "(%a)" pexpr x)
]

and pexpr1 pps = fun [
    UNOP _ UMINUS pe ->
    Fmt.(pf pps "- %a" pexpr1 pe)
  | x -> pexpr0 pps x
]

and pexpr2 pps = fun [
  BINOP _ POW pe1 pe2 ->
   Fmt.(pf pps "%a ** %a" pexpr1 pe1 pexpr2 pe2)
| x -> pexpr1 pps x
    ]

and pexpr3 pps = fun [
  BINOP _ MUL pe1 pe2 ->
   Fmt.(pf pps "%a * %a" pexpr3 pe1 pexpr2 pe2)
| BINOP _ DIV pe1 pe2 ->
   Fmt.(pf pps "%a / %a" pexpr3 pe1 pexpr2 pe2)
| x -> pexpr2 pps x
    ]

and pexpr4 pps = fun [
  BINOP _ ADD pe1 pe2 ->
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
    QLET loc bl qc ->
    let comm = Ploc.comment loc in
     Fmt.(pf pps "@[<v>%alet @[%a@] in@ %a@]" comm_nl comm (list ~{sep=and_sep} qbinding) bl qcirc qc)
  | QWIRES _ qvl cvl -> paren_qvars_cvars pps (qvl, cvl)
  | QGATEAPP _ qg [] qvl cvl ->
     Fmt.(pf  pps "%a %a" QG.pp_hum qg qvars_cvars (qvl, cvl))
  | QGATEAPP _ qg pel qvl cvl ->
     Fmt.(pf  pps "%a (%a) %a" QG.pp_hum qg (list ~{sep=(const string ", ")} pexpr) pel qvars_cvars (qvl, cvl))
  | QBARRIER _ qvl -> Fmt.(pf pps "barrier %a" qvars_cvars (qvl, []))
  | QCREATE _ (EXPLICIT n) -> Fmt.(pf pps "qubit #%d ()" n)
  | QCREATE _ (UNIQUE _) -> Fmt.(pf pps "qubit ()")
  | QDISCARD _ qv -> Fmt.(pf pps "qdiscard %a" QV.pp_hum qv)
  | QMEASURE _ qv -> Fmt.(pf pps "measure %a" QV.pp_hum qv)
  | QRESET _ qv -> Fmt.(pf pps "reset %a" QV.pp_hum qv)
]

and qbinding pps = fun [
    (_, [qv],  [], qc) ->
    Fmt.(pf pps "%a = %a" QV.pp_hum qv qcirc qc)
  | (_, qvl,  cvl, qc) ->
    Fmt.(pf pps "%a = %a" paren_qvars_cvars (qvl,cvl) qcirc qc)
] ;

value gate_item pps = fun [
    DEF _ gname ((pvl, qvl, cvl), qc) ->
    Fmt.(pf pps "@[<v>gate %a (%a) %a =@ @[<v 2>%a@]@]@,;"
           QG.pp_hum gname
           (list ~{sep=(const string ", ")} PV.pp_hum) pvl
           qvars_cvars (qvl, cvl)
           qcirc qc)
  | OPAQUE _ gname (pvl, qvl, cvl) ->
    Fmt.(pf pps "@[gate %a (%a) %a ;@]"
           QG.pp_hum gname
           (list ~{sep=(const string ", ")} PV.pp_hum) pvl
           qvars_cvars (qvl, cvl))
] ;

value item pps = fun [
    QGATE _ gitem -> gate_item pps gitem
  | QINCLUDE _ _ s _ ->
     Fmt.(pf pps "include %a ;" (quote string) s)
  | QCOUPLING_MAP _ mname cm ->
     Fmt.(pf pps "coupling_map %a %a ;"
            ID.pp_hum mname
            CouplingMap.pp_hum cm)
  | QLAYOUT _ mname l ->
     Fmt.(pf pps "layout %a %a ;"
            ID.pp_hum mname
            Layout.pp_hum l)
] ;

value newline_sep pps () = Fmt.(pf pps "@.") ;

value environ pps l =
  Fmt.(pf pps "@[<v>%a@]" (list ~{sep=newline_sep} item) l) ;


value program pps (l, qc) =
  Fmt.(pf pps "%a@.%a%!" environ l qcirc qc) ;

end ;
