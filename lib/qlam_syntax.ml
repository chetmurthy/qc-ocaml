
open Qc_misc

module PC = struct

type t =
  REAL of RealNumeral.t
| NNINT of int
| PI

end

module PE = struct

type binop_t = ADD | SUB | MUL | DIV | POW
type unop_t = UMINUS
type ufun_t = SIN | COS | TAN | EXP | LN | SQRT

type var_t = PV of string * int

type t =
  ID of var_t
| CONST of PC.t
| BINOP of binop_t * t * t
| UNOP of binop_t * t
| UFUN of ufun_t * t
end

module QC = struct

type qvar_t = QV of string * int
type cvar_t = CV of string * int
type qgatename_t = QGATE of string * int

type qgate_t =
  QGATELAM of PE.var_t list * qvar_t list * cvar_t list * t
| QGATE of qgatename_t
| QBARRIER
| QBIT | QDISCARD
| QMEASURE
| QRESET

and t =
  QLET of qbinding_t list * t
| QWIRES of qvar_t list * cvar_t list
| QGATEAPP of qgate_t * PE.t list * qvar_t list * cvar_t list

and qbinding_t =
  qvar_t list * cvar_t list * t
end
