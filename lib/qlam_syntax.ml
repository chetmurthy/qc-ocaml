
open Qc_misc

type rawtoken =
  | T_EOF
  (* special symbols *)
  | T_OPENQASM of RealNumeral.t
  | T_INCLUDE of string

  (* special characters *)
  | T_SEMICOLON
  | T_LBRACE
  | T_RBRACE
  | T_LBRACKET
  | T_RBRACKET
  | T_LPAREN
  | T_RPAREN
  | T_EQEQ
  | T_COMMA
  | T_DASH
  | T_PLUS
  | T_STAR
  | T_STARSTAR
  | T_SLASH
  | T_CARET
  | T_DASHGT

  (* reserved words *)
  | T_BARRIER
  | T_COS
  | T_CREG
  | T_CX
  | T_EXP
  | T_GATE
  | T_IF
  | T_LN
  | T_PI
  | T_QREG
  | T_SIN
  | T_SQRT
  | T_TAN
  | T_U
  | T_MEASURE
  | T_OPAQUE
  | T_RESET
  | T_LET | T_IN | T_AND

  | T_INTEGER of int
  | T_REAL of RealNumeral.t
  | T_ID of string

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
| UNOP of unop_t * t
| UFUN of ufun_t * t
end

module QC = struct

type qvar_t = QV of string * int
type cvar_t = CV of string * int
type qgatename_t = QGATE of string * int

type qgate_t =
  QGATELAM of PE.var_t list * qvar_t list * cvar_t list * t
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
end
