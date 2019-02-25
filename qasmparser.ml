(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open Misc_functions
open Qasmsyntax

let expand_include strm =
  let rec exprec =
    parser
  | [< '(_, T_INCLUDE fname) ; strm >] ->
     let ic = open_in fname in
     [< exprec (Qasmlex.make_body_lexer_from_channel ~fname ic) ; exprec strm >]
  | [< 'tok ; strm >] -> [< 'tok ; exprec strm >]
  | [< >] -> [< >]
  in
  exprec strm

let full_parse pfun ?(fname="") buf =
  let tokstrm = Qasmlex.make_lexer ~fname buf in
  pfun (expand_include tokstrm)

let full_parse_from_file pfun fname =
let ic = open_in fname in
  let tokstrm = Qasmlex.make_lexer_from_channel ~fname ic in
  pfun (expand_include tokstrm)

let body_parse pfun ?(fname="") buf =
  let tokstrm = Qasmlex.make_body_lexer ~fname buf in
  pfun (expand_include tokstrm)
                                         
let body_parse_from_file pfun fname =
let ic = open_in fname in
  let tokstrm = Qasmlex.make_body_lexer_from_channel ~fname ic in
  pfun (expand_include tokstrm)

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

module TA = TokenAux

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
    | REG of string
    | BIT of string * int

  type raw_instruction_t =
    | U of expr list * bit_or_reg_t
    | CX of bit_or_reg_t * bit_or_reg_t
    | COMPOSITE_GATE of string * expr list * bit_or_reg_t list
    | MEASURE of bit_or_reg_t * bit_or_reg_t
    | RESET of bit_or_reg_t

  type instruction_t =
    TA.t * raw_instruction_t

  type raw_gate_op_t =
    GATE_INSTRUCTION of raw_instruction_t
  | GATE_BARRIER of string list

  type gate_op_t =
    TA.t * raw_gate_op_t

  type raw_stmt_t =
    | STMT_GATEDECL of string * string list * string list * gate_op_t list
    | STMT_OPAQUEDECL of string * string list * string list
    | STMT_INSTRUCTION of raw_instruction_t
    | STMT_IF of string * int * raw_instruction_t
    | STMT_BARRIER of string list
    | STMT_QREG of string * int
    | STMT_CREG of string * int

  type stmt_t = TA.t * raw_stmt_t

  type program_t = stmt_t list

end


module PA = struct

let header = parser
| [< '(_, T_OPENQASM r) >] -> r

let rec expr0 = parser
| [< '(aux, T_ID id) >] -> (aux, Ast.ID id)
| [< '(aux, T_INTEGER n) >] ->
   if n < 0 then raise (SyntaxError "negative integer not valid in expression")
   else (aux, Ast.NNINT n)
| [< '(aux, T_REAL r) >] -> (aux, Ast.REAL r)
| [< '(aux, T_PI) >] -> (aux, Ast.PI)
| [< '(aux1, T_LPAREN) ; (aux2, e) = expr ; '(aux3, T_RPAREN) >] ->
   (TA.appendlist [aux1; aux2; aux3], e)

| [< '(aux1, T_SIN) ; '(aux2, T_LPAREN) ; (aux3, e) = expr ; '(aux4, T_RPAREN) >] ->
   (TA.appendlist [aux1; aux2; aux3; aux4], Ast.SIN e)

| [< '(aux1, T_COS) ; '(aux2, T_LPAREN) ; (aux3, e) = expr ; '(aux4, T_RPAREN) >] ->
   (TA.appendlist [aux1; aux2; aux3; aux4], Ast.COS e)

| [< '(aux1, T_TAN) ; '(aux2, T_LPAREN) ; (aux3, e) = expr ; '(aux4, T_RPAREN) >] ->
   (TA.appendlist [aux1; aux2; aux3; aux4], Ast.TAN e)

| [< '(aux1, T_EXP) ; '(aux2, T_LPAREN) ; (aux3, e) = expr ; '(aux4, T_RPAREN) >] ->
   (TA.appendlist [aux1; aux2; aux3; aux4], Ast.EXP e)

| [< '(aux1, T_LN) ; '(aux2, T_LPAREN) ; (aux3, e) = expr ; '(aux4, T_RPAREN) >] ->
   (TA.appendlist [aux1; aux2; aux3; aux4], Ast.LN e)

| [< '(aux1, T_SQRT) ; '(aux2, T_LPAREN) ; (aux3, e) = expr ; '(aux4, T_RPAREN) >] ->
   (TA.appendlist [aux1; aux2; aux3; aux4], Ast.SQRT e)

and expr1 = parser
| [< (aux1, e1)=expr0 ;
   rv=(parser
       | [< '(aux2, T_CARET) ; (aux3, e2)=expr1 >] -> (TA.appendlist [aux1; aux2; aux3], Ast.XOR(e1, e2))
      | [< >] -> (aux1, e1)) >] -> rv

and expr2 = parser
| [< '(aux1, T_DASH) ; (aux2, e)=expr2 >] -> (TA.append aux1 aux2, Ast.UMINUS e)
| [< '(aux1, T_PLUS) ; (aux2, e)=expr2 >] -> (TA.append aux1 aux2, e)
| [< e=expr1 >] -> e

and expr3 = parser
| [< rv=ne_plist_with_sep_function
          (parser
           | [< '(aux2, T_STAR) >] ->
           (fun (aux1, e1) (aux3, e2) -> (TA.appendlist [aux1; aux2; aux3], Ast.MUL(e1, e2)))
          | [< '(aux2, T_SLASH) >] ->
             (fun (aux1, e1) (aux3, e2) -> (TA.appendlist [aux1; aux2; aux3], Ast.DIV(e1, e2))))
          expr2 >] -> rv

and expr4 = parser
| [< rv=ne_plist_with_sep_function
          (parser
           | [< '(aux2, T_PLUS) >] -> (fun (aux1, e1) (aux3, e2) -> (TA.appendlist [aux1; aux2; aux3], Ast.ADD(e1, e2)))
          | [< '(aux2, T_DASH) >] -> (fun (aux1, e1) (aux3, e2) -> (TA.appendlist [aux1; aux2; aux3], Ast.SUB(e1, e2))))
          expr3 >] -> rv

and expr = parser
| [< e=expr4 >] -> e

let bit_or_reg = parser
| [< '(aux1, T_ID id) ; rv=(parser
                        | [< '(aux2, T_LBRACKET); '(aux3, T_INTEGER n); '(aux4, T_RBRACKET) >] ->
                        if n < 0 then raise (SyntaxError "negative integer not valid in register index")
                        else (TA.appendlist [aux1; aux2; aux3; aux4], Ast.BIT(id, n))
                        | [< >] -> (aux1, Ast.REG id)
                       ) >] -> rv

let id = parser
| [< '(aux, T_ID id) >] -> (aux, id)

let aux_comma f = parser
| [< '(aux2, T_COMMA) >] ->
   (fun (aux1, lhs) (aux3, rhs) -> (TA.appendlist [aux1; aux2; aux3], f lhs rhs))

let as_list_lift_aux pfun strm = (parser [< (a,rv)=pfun >] -> (a, [rv])) strm

let ne_explist strm = ne_plist_with_sep_function (aux_comma (fun h t -> h@t)) (as_list_lift_aux expr) strm

let possibly_empty pfun = parser
| [< l=pfun >] -> l
| [< >] -> (TA.mt, [])

let ne_bit_or_reg_list strm = ne_plist_with_sep_function (aux_comma (fun h t -> h@t)) (as_list_lift_aux bit_or_reg) strm

let ne_id_list strm = ne_plist_with_sep_function (aux_comma (fun h t -> h@t)) (as_list_lift_aux id) strm

let instruction = parser
| [< '(aux1, T_U) ; '(aux2, T_LPAREN) ; (aux3, el)=ne_explist ; '(aux4, T_RPAREN) ; (aux5, a)=bit_or_reg ; '(aux6, T_SEMICOLON) >] ->
   (TA.appendlist [aux1; aux2; aux3; aux4; aux5; aux6], Ast.U(el, a))
| [< '(aux1,T_CX) ; (aux2, a1)=bit_or_reg ; '(aux3, T_COMMA) ; (aux4, a2)=bit_or_reg ; '(aux5, T_SEMICOLON) >] ->
   (TA.appendlist [aux1; aux2; aux3; aux4; aux5], Ast.CX(a1, a2))
| [< '(aux1, T_ID gateid) ;
   (aux2, params)=(parser
                     [< '(paux1, T_LPAREN); (paux2, l)=possibly_empty ne_explist; '(paux3, T_RPAREN) >] ->
                   (TA.appendlist [paux1; paux2; paux3], l)
                  | [< >] -> (TA.mt, [])
                  ) ;
   (aux3, regs)=ne_bit_or_reg_list ;
    '(aux4, T_SEMICOLON) >] ->
   (TA.appendlist [aux1; aux2; aux3; aux4], Ast.COMPOSITE_GATE(gateid, params, regs))
| [< '(aux1, T_MEASURE) ; (aux2, l)=bit_or_reg ; '(aux3, T_DASHGT) ; (aux4, r)=bit_or_reg ; '(aux5, T_SEMICOLON) >] ->
   (TA.appendlist [aux1; aux2; aux3; aux4; aux5], Ast.MEASURE(l, r))
| [< '(aux1, T_RESET) ; (aux2, l)=bit_or_reg ; '(aux3, T_SEMICOLON) >] ->
   (TA.appendlist [aux1; aux2; aux3], Ast.RESET(l))

let gop = parser
| [< (aux, i)=instruction >] -> (aux, Ast.GATE_INSTRUCTION i)
| [< '(aux1, T_BARRIER) ; (aux2, l)=ne_id_list; '(aux3, T_SEMICOLON) >] ->
   (TA.appendlist [aux1; aux2; aux3], Ast.GATE_BARRIER l)

let as_list pfun strm = (parser [< rv=pfun >] -> (TA.mt, [rv])) strm
let ne_gop_list strm = ne_plist_with_sep_function (aux_comma (fun h t -> h@t)) (as_list gop) strm
let gop_list strm = possibly_empty ne_gop_list strm

let gatedecl = parser
| [< '(aux1, T_GATE) ; '(aux2, T_ID gateid) ;
   (aux2, formal_params)=(parser
                            [< '(paux1, T_LPAREN); (paux2, l)=ne_id_list ; '(paux3, T_RPAREN) >] ->
                          (TA.appendlist [paux1; paux2; paux3], l)
                         | [< >] -> (TA.mt, [])
                         ) ;
   (aux3, formal_bits)=ne_id_list ;
   '(aux4, T_LBRACE) ;
   (aux5, gopl)=gop_list ;
   '(aux6, T_RBRACE) >] ->
   (TA.appendlist [aux1; aux2; aux3; aux4; aux5; aux6],
    Ast.STMT_GATEDECL(gateid, formal_params, formal_bits, gopl))

let opaquedecl = parser
| [< '(aux1, T_OPAQUE) ; '(aux2, T_ID gateid) ;
   (aux3, formal_params)=(parser
                            [< '(paux1, T_LPAREN); (paux2, l)=possibly_empty ne_id_list ; '(paux3, T_RPAREN) >] ->
                          (TA.appendlist [paux1; paux2; paux3], l)
                 | [< >] -> (TA.mt, [])
                 ) ;
   (aux4, formal_bits)=ne_id_list ;
   '(aux5, T_SEMICOLON)
 >] -> (TA.appendlist [aux1; aux2; aux3; aux4; aux5], Ast.STMT_OPAQUEDECL(gateid, formal_params, formal_bits))

let reg_decl=parser
| [< '(aux1, T_QREG) ; '(aux2, T_ID id); '(aux3, T_LBRACKET) ; '(aux4, T_INTEGER n) ; '(aux5, T_RBRACKET) ; '(aux6, T_SEMICOLON) >] ->
   if n < 0 then raise (SyntaxError "negative integer not valid in qreg statement")
   else (TA.appendlist [aux1; aux2; aux3; aux4; aux5; aux6], Ast.STMT_QREG(id, n)) ;
| [< '(aux1, T_CREG) ; '(aux2, T_ID id); '(aux3, T_LBRACKET) ; '(aux4, T_INTEGER n) ; '(aux5, T_RBRACKET) ; '(aux6, T_SEMICOLON) >] ->
   if n < 0 then raise (SyntaxError "negative integer not valid in creg statement")
   else (TA.appendlist [aux1; aux2; aux3; aux4; aux5; aux6], Ast.STMT_CREG(id, n))


let statement = parser
| [< d=reg_decl >] -> d
| [< d=gatedecl >] -> d
| [< d=opaquedecl >] -> d
| [< (aux, i)=instruction >] -> (aux, Ast.STMT_INSTRUCTION i)
| [< '(aux1, T_IF) ; '(aux2, T_LPAREN) ; '(aux3, T_ID id) ; '(aux4, T_EQEQ) ; '(aux5, T_INTEGER n) ; '(aux6, T_RPAREN) ; (aux7, i)=instruction >] ->
   if n < 0 then raise (SyntaxError "negative integer not valid in if statment")
   else (TA.appendlist [aux1; aux2; aux3; aux4; aux5; aux6; aux7], Ast.STMT_IF(id, n, i))
| [< '(aux1, T_BARRIER) ; (aux2, l)=ne_id_list; '(aux3, T_SEMICOLON) >] ->
   (TA.appendlist [aux1; aux2; aux3], Ast.STMT_BARRIER l)

let ne_statement_list strm = let (h, t) = ne_plist statement strm in h::t

let program strm = ne_statement_list strm

let mainprogram = parser
| [< vers=header ; l=program >] -> (vers, l)

end
