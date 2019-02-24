(* Copyright 2019 Chetan Murthy, All rights reserved. *)

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

end

let rec expr0 = parser
| [< '(_, T_ID id) >] -> Ast.ID id
| [< '(_, T_INTEGER n) >] ->
   if n < 0 then raise (SyntaxError "negative integer not valid in expression")
   else Ast.NNINT n
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
| [< rv=ne_plist_with_sep
          (parser
           | [< '(_, T_STAR) >] -> (fun e1 e2 -> Ast.MUL(e1, e2))
          | [< '(_, T_SLASH) >] -> (fun e1 e2 -> Ast.DIV(e1, e2)))
          expr2 >] -> rv

and expr4 = parser
| [< rv=ne_plist_with_sep
          (parser
           | [< '(_, T_PLUS) >] -> (fun e1 e2 -> Ast.ADD(e1, e2))
          | [< '(_, T_DASH) >] -> (fun e1 e2 -> Ast.SUB(e1, e2)))
          expr3 >] -> rv

and expr = parser
| [< >] -> failwith "unimplemented"
