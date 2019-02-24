(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open Qasmsyntax

let pa_header = parser
| [< 'T_OPENQASM r >] -> r
                                         
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
| [< '(T_ID id, _) >] -> Ast.ID id
| [< '(T_INTEGER n, _) >] ->
   if n < 0 then raise (SyntaxError "negative integer not valid in expression")
   else Ast.NNINT n
| [< '(T_PI, _) >] -> Ast.PI
| [< '(T_LPAREN, _) ; e = expr ; '(T_RPAREN, _) >] -> e
| [< '(T_SIN, _) ; '(T_LPAREN, _) ; e = expr ; '(T_RPAREN, _) >] -> Ast.SIN e
| [< '(T_COS, _) ; '(T_LPAREN, _) ; e = expr ; '(T_RPAREN, _) >] -> Ast.COS e
| [< '(T_TAN, _) ; '(T_LPAREN, _) ; e = expr ; '(T_RPAREN, _) >] -> Ast.TAN e
| [< '(T_EXP, _) ; '(T_LPAREN, _) ; e = expr ; '(T_RPAREN, _) >] -> Ast.EXP e
| [< '(T_LN, _) ; '(T_LPAREN, _) ; e = expr ; '(T_RPAREN, _) >] -> Ast.LN e
| [< '(T_SQRT, _) ; '(T_LPAREN, _) ; e = expr ; '(T_RPAREN, _) >] -> Ast.SQRT e

and expr1 = parser
| [< e1=expr0 ; '(T_CARET, _) ; e2=expr1 >] -> Ast.XOR(e1, e2)

and expr = parser
| [< >] -> failwith "unimplemented"
