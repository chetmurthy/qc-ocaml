(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open Misc
open Qasmsyntax

let pa_header = parser
| [< '(_, T_OPENQASM r) >] -> r
                                         
(*
         mainprogram: "OPENQASM" real ";" program
         program: statement | program statement
         statement: decl
                                                :| gatedecl goplist }
                                                :| gatedecl }
                                                :| "opaque" id idlist ";"
                                                :| "opaque" id "( )" idlist ";"
                                                :| "opaque" id "(" idlist ")" idlist ";"
                                                :| qop
                                                :| "if (" id "==" nninteger ")" qop
                                                :| "barrier" anylist ";"
         decl: "qreg" id [ nninteger ] ";" | "creg" id [ nninteger ] ";"
         gatedecl: "gate" id idlist {
                                         :| "gate" id "( )" idlist {
                                         :| "gate" id "(" idlist ")" idlist {
         goplist: uop
                                        :| "barrier" idlist ";"
                                        :| goplist uop
                                        :| goplist "barrier" idlist ";"
         qop: uop
                        :| "measure" argument "->" argument ";"
                        :| "reset" argument ";"
         uop: "U (" explist ")" argument ";"
                        :| "CX" argument "," argument ";"
                        :| id anylist ";" | id "( )" anylist ";"
                        :| id "(" explist ")" anylist ";"
         anylist: idlist | mixedlist
         idlist: id | idlist "," id
         mixedlist: id [ nninteger ] | mixedlist "," id
                                                :| mixedlist "," id [ nninteger ]
                                                :| idlist "," id [ nninteger ]
         argument: id | id [ nninteger ]
         explist: exp | explist "," exp
         exp: real | nninteger | "pi" | id
                        :| exp + exp | exp - exp | exp * exp
                        :| exp / exp | -exp | exp ^ exp
                        :| "(" exp ")" | unaryop "(" exp ")"
         unaryop: "sin" | "cos" | "tan" | "exp" | "ln" | "sqrt"

 *)

module Ast = struct
  type expr =
    ID of string
  | REAL of RealNumeral.t
  | NNINT of int
  | PI
  | ADD of expr * expr
  | SUB of expr * expr
  | MUL of expr * expr
  | DIV of expr * expr
  | UMINUS of expr
  | XOR of expr * expr
  | SIN of expr
  | COS of expr
  | TAN of expr
  | EXP of expr
  | LN of expr
  | SQRT of expr


  type bit_or_reg_t =
    REG of string
  | BIT of string * int

  type instruction_t =
    U of expr list * bit_or_reg_t
  | CX of bit_or_reg_t * bit_or_reg_t
  | COMPOSITE_GATE of string * expr list * bit_or_reg_t list
  | MEASURE of bit_or_reg_t * bit_or_reg_t
  | RESET of bit_or_reg_t

  type gate_op_t =
    GATE_INSTRUCTION of instruction_t
  | GATE_BARRIER of string list

  type stmt_t =
    | STMT_GATEDECL of string * string list * string list * gate_op_t list
    | STMT_OPAQUEDECL of string * string list * string list
    | STMT_INSTRUCTION of instruction_t
    | STMT_IF of string * int * instruction_t
    | STMT_BARRIER of string list
    | STMT_QREG of string * int
    | STMT_CREG of string * int

end

let rec expr0 = parser
| [< '(_, T_ID id) >] -> Ast.ID id
| [< '(_, T_INTEGER n) >] ->
   if n < 0 then raise (SyntaxError "negative integer not valid in expression")
   else Ast.NNINT n
| [< '(_, T_REAL r) >] -> Ast.REAL r
| [< '(_, T_PI) >] -> Ast.PI
| [< '(_, T_LPAREN) ; e = expr ; '(_, T_RPAREN) >] -> e
| [< '(_, T_SIN) ; '(_, T_LPAREN) ; e = expr ; '(_, T_RPAREN) >] -> Ast.SIN e
| [< '(_, T_COS) ; '(_, T_LPAREN) ; e = expr ; '(_, T_RPAREN) >] -> Ast.COS e
| [< '(_, T_TAN) ; '(_, T_LPAREN) ; e = expr ; '(_, T_RPAREN) >] -> Ast.TAN e
| [< '(_, T_EXP) ; '(_, T_LPAREN) ; e = expr ; '(_, T_RPAREN) >] -> Ast.EXP e
| [< '(_, T_LN) ; '(_, T_LPAREN) ; e = expr ; '(_, T_RPAREN) >] -> Ast.LN e
| [< '(_, T_SQRT) ; '(_, T_LPAREN) ; e = expr ; '(_, T_RPAREN) >] -> Ast.SQRT e

and expr1 = parser
| [< e1=expr0 ;
   rv=(parser
       | [< '(_, T_CARET) ; e2=expr1 >] -> Ast.XOR(e1, e2)
      | [< >] -> e1) >] -> rv

and expr2 = parser
| [< '(_, T_DASH) ; e=expr2 >] -> Ast.UMINUS e
| [< '(_, T_PLUS) ; e=expr2 >] -> e
| [< e=expr1 >] -> e                                           

and expr3 = parser
| [< rv=ne_plist_with_sep_function
          (parser
           | [< '(_, T_STAR) >] -> (fun e1 e2 -> Ast.MUL(e1, e2))
          | [< '(_, T_SLASH) >] -> (fun e1 e2 -> Ast.DIV(e1, e2)))
          expr2 >] -> rv

and expr4 = parser
| [< rv=ne_plist_with_sep_function
          (parser
           | [< '(_, T_PLUS) >] -> (fun e1 e2 -> Ast.ADD(e1, e2))
          | [< '(_, T_DASH) >] -> (fun e1 e2 -> Ast.SUB(e1, e2)))
          expr3 >] -> rv

and expr = parser
| [< e=expr4 >] -> e

let bit_or_reg = parser
| [< '(_, T_ID id) ; rv=(parser
                        | [< '(_, T_LBRACKET); '(_, T_INTEGER n); '(_, T_RBRACKET) >] ->
                        if n < 0 then raise (SyntaxError "negative integer not valid in register index")
                        else Ast.BIT(id, n)
                        | [< >] -> Ast.REG id
                       ) >] -> rv

let id = parser
| [< '(_, T_ID id) >] -> id

let comma = parser [< '(_, T_COMMA) >] -> ()

let explist strm = ne_plist_with_sep comma expr strm

let instruction = parser
| [< '(_, T_U) ; '(_, T_LPAREN) ; el=explist ; '(_, T_RPAREN) ; a=bit_or_reg ; '(_, T_SEMICOLON) >] ->
   Ast.U(el, a)
| [< '(_,T_CX) ; a1=bit_or_reg ; '(_, T_COMMA) ; a2=bit_or_reg ; '(_, T_SEMICOLON) >] ->
   Ast.CX(a1, a2)
| [< '(_, T_ID gateid) ;
   params=(parser
             [< '(_, T_LPAREN); l=plist_with_sep comma expr ; '(_, T_RPAREN) >] -> l
          | [< >] -> []
          ) ;
   regs=ne_plist_with_sep comma bit_or_reg ;
   '(_, T_SEMICOLON) >] -> Ast.COMPOSITE_GATE(gateid, params, regs)

let gop = parser
| [< i=instruction >] -> Ast.GATE_INSTRUCTION i
| [< '(_, T_BARRIER) ; l=ne_plist_with_sep comma id; '(_, T_SEMICOLON) >] -> Ast.GATE_BARRIER l

let gatedecl = parser
| [< '(_, T_GATE) ; '(_, T_ID gateid) ;
   formal_params=(parser
                    [< '(_, T_LPAREN); l=plist_with_sep comma id ; '(_, T_RPAREN) >] -> l
                 | [< >] -> []
                 ) ;
   formal_bits=ne_plist_with_sep comma id ;
   '(_, T_LBRACE) ;
   gopl=plist gop ;
   '(_, T_RBRACE) >] -> Ast.STMT_GATEDECL(gateid, formal_params, formal_bits, gopl)

let opaquedecl = parser
| [< '(_, T_OPAQUE) ; '(_, T_ID gateid) ;
   formal_params=(parser
                    [< '(_, T_LPAREN); l=plist_with_sep comma id ; '(_, T_RPAREN) >] -> l
                 | [< >] -> []
                 ) ;
   formal_bits=ne_plist_with_sep comma id ;
   '(_, T_SEMICOLON)
 >] -> Ast.STMT_OPAQUEDECL(gateid, formal_params, formal_bits)

let reg_decl=parser
| [< '(_, T_QREG) ; '(_, T_ID id); '(_, T_LBRACKET) ; '(_, T_INTEGER n) ; '(_, T_RBRACKET) ; '(_, T_SEMICOLON) >] ->
   if n < 0 then raise (SyntaxError "negative integer not valid in qreg statement")
   else Ast.STMT_QREG(id, n) ;
| [< '(_, T_CREG) ; '(_, T_ID id); '(_, T_LBRACKET) ; '(_, T_INTEGER n) ; '(_, T_RBRACKET) ; '(_, T_SEMICOLON) >] ->
   if n < 0 then raise (SyntaxError "negative integer not valid in creg statement")
   else Ast.STMT_CREG(id, n)


let statement = parser
| [< d=reg_decl >] -> d
| [< d=gatedecl >] -> d
| [< d=opaquedecl >] -> d
| [< '(_, T_IF) ; '(_, T_LPAREN) ; '(_, T_ID id) ; '(_, T_EQEQ) ; '(_, T_INTEGER n) ; '(_, T_RPAREN) ; i=instruction >] ->
   if n < 0 then raise (SyntaxError "negative integer not valid in if statment")
   else Ast.STMT_IF(id, n, i)
| [< '(_, T_BARRIER) ; l=ne_plist_with_sep comma id; '(_, T_SEMICOLON) >] -> Ast.STMT_BARRIER l

let program strm = ne_plist statement strm
