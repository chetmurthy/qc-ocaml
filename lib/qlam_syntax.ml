
open Qc_misc

let ident pps x = Fmt.(pf pps "%s" (ID.unmk x))

module PC = struct

type t =
  REAL of RealNumeral.t
| NNINT of int
| PI

let pp pps = function
    REAL s -> Fmt.(pf pps "%s" (RealNumeral.unmk s))
  | NNINT n -> Fmt.(pf pps "%d" n)
  | PI -> Fmt.(pf pps "pi")

end

type paramvar_t = PV of ID.t
let paramvar pps (PV id) = ident pps id

module PE = struct

type binop_t = ADD | SUB | MUL | DIV | POW
type unop_t = UMINUS
type ufun_t = SIN | COS | TAN | EXP | LN | SQRT

let string_of_ufun = function
    SIN -> "sin"
  | COS -> "cos"
  | TAN -> "tan"
  | EXP -> "exp"
  | LN -> "ln"
  | SQRT -> "sqrt"

type t =
  ID of paramvar_t
| CONST of PC.t
| BINOP of binop_t * t * t
| UNOP of unop_t * t
| UFUN of ufun_t * t

let rec pp0 pps = function
  ID pv -> Fmt.(pf pps "%a" paramvar pv)
| CONST pc ->  PC.pp pps pc
| UFUN (fsym, pe) ->
   Fmt.(pf pps "%s(%a)" (string_of_ufun fsym) pp pe)
| x -> Fmt.(pf pps "(%a)" pp x)

and pp1 pps = function
    UNOP (UMINUS, pe) ->
    Fmt.(pf pps "- %a" pp1 pe)
  | x -> pp0 pps x

and pp2 pps = function
  BINOP (POW,pe1, pe2) ->
   Fmt.(pf pps "%a ** %a" pp1 pe1 pp2 pe2)
| x -> pp1 pps x

and pp3 pps = function
  BINOP (MUL, pe1, pe2) ->
   Fmt.(pf pps "%a * %a" pp3 pe1 pp2 pe2)
| BINOP (DIV, pe1, pe2) ->
   Fmt.(pf pps "%a / %a" pp3 pe1 pp2 pe2)
| x -> pp2 pps x

and pp4 pps = function
  BINOP (ADD, pe1, pe2) ->
   Fmt.(pf pps "%a + %a" pp4 pe1 pp3 pe2)
| BINOP (SUB, pe1, pe2) ->
   Fmt.(pf pps "%a - %a" pp4 pe1 pp3 pe2)
| x -> pp3 pps x

and pp pps x = pp4 pps x

end

module QC = struct
open Fmt

type qvar_t = QV of ID.t
type cvar_t = CV of ID.t
type qgatename_t = QG of ID.t

let qvar pps (QV id) = ident pps id
let cvar pps (CV id) = ident pps id
let qgatename pps (QG id) = ident pps id

type qgatelam_t = paramvar_t list * qvar_t list * cvar_t list * t

and qgate_t =
  QGATELAM of qgatelam_t
| QGATE of qgatename_t
| QBARRIER of qvar_t list
| QBIT | QDISCARD of qvar_t list
| QMEASURE of qvar_t list
| QRESET of qvar_t list

and t =
  QLET of qbinding_t list * t
| QWIRES of qvar_t list * cvar_t list
| QGATEAPP of qgate_t * PE.t list * qvar_t list * cvar_t list

and qbinding_t =
  qvar_t list * cvar_t list * t

let and_sep pps () = Fmt.(pf pps "@ and ")

let paren_qvars_cvars pps = function
    (qvl, []) ->
     Fmt.(pf pps "(%a)" (list ~sep:(const string ", ") qvar) qvl)
  | (qvl, cvl) ->
     Fmt.(pf pps "(%a / %a)" (list ~sep:(const string ", ") qvar) qvl  (list ~sep:(const string ", ") cvar) cvl)

let qvars_cvars pps = function
    ([qv], []) ->
     Fmt.(pf pps "%a" qvar qv)
  | x ->  paren_qvars_cvars pps x

let rec qcirc pps = function
    QLET (bl, qc) ->
     Fmt.(pf pps "let @[%a@]@ in %a" (list ~sep:and_sep binding) bl qcirc qc)
  | QWIRES (qvl, cvl) -> paren_qvars_cvars pps (qvl, cvl)
  | QGATEAPP (qg, pel, qvl, cvl) ->
     Fmt.(pf  pps "%a (%a) %a" qgate qg (list ~sep:(const string ", ") PE.pp) pel qvars_cvars (qvl, cvl))

and qgate pps = function
  QGATELAM (pvl, qvl,  cvl, qc) ->
    Fmt.(pf pps "gatefun [@[@ (%a)@ %a@ %a@]]"
           (list ~sep:(const string ", ") paramvar) pvl
           qvars_cvars (qvl, cvl)
           qcirc qc)

  | QGATE qg ->  Fmt.(pf pps "%a" qgatename qg)
  | QBARRIER qvl -> Fmt.(pf pps "barrier %a" qvars_cvars (qvl, []))
  | QBIT -> Fmt.(pf pps "qubit()")
  | QDISCARD qvl -> Fmt.(pf pps "qdiscard %a" qvars_cvars (qvl, []))
  | QMEASURE qvl -> Fmt.(pf pps "measure %a" qvars_cvars (qvl, []))
  | QRESET qvl -> Fmt.(pf pps "reset %a" qvars_cvars (qvl, []))

and binding pps = function
    (qvl,  cvl, qc) ->
    Fmt.(pf pps "%a = %a" qvars_cvars (qvl,cvl) qcirc qc)
end

module QEnv = struct
type item =
  QGATEDEF of QC.qgatename_t * QC.qgatelam_t
| QINCLUDE of string * t
and t = item list

let item pps = function
    QGATEDEF (gname, (pvl, qvl, cvl, qc)) ->
    Fmt.(pf pps "gate %a (%a) %a = %a ;"
           QC.qgatename gname
           (list ~sep:(const string ", ") paramvar) pvl
           QC.qvars_cvars (qvl, cvl)
           QC.qcirc qc)
  | QINCLUDE (s, _) ->
     Fmt.(pf pps "include %a" (quote string) s)

let newline_sep pps () = Fmt.(pf pps "@.")

let pp pps l =
  Fmt.(pf pps "%a" (list ~sep:newline_sep item) l)

end

type t = QEnv.t * QC.t
