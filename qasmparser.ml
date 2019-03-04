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

let catch_parse_error pfun tokstrm =
  try pfun tokstrm
  with Stream.Error _ ->
        match Stream.peek tokstrm with
        | None -> raise (SyntaxError("parsing error at EOF"))
        | Some(aux, tok) ->
           let p = TokenAux.startpos aux in
           raise (SyntaxError (Printf.sprintf "parse error in file \"%s\" at char %d" p.Lexing.pos_fname p.Lexing.pos_cnum))


let full_parse pfun ?(fname="") buf =
  let tokstrm = Qasmlex.make_lexer ~fname buf in
  let tokstrm = expand_include tokstrm in
  catch_parse_error pfun tokstrm

let full_parse_from_file pfun fname =
let ic = open_in fname in
  let tokstrm = Qasmlex.make_lexer_from_channel ~fname ic in
  let tokstrm = expand_include tokstrm in
  catch_parse_error pfun tokstrm

let body_parse pfun ?(fname="") buf =
  let tokstrm = Qasmlex.make_body_lexer ~fname buf in
  let tokstrm = expand_include tokstrm in
  catch_parse_error pfun tokstrm

let body_parse_from_file pfun fname =
let ic = open_in fname in
  let tokstrm = Qasmlex.make_body_lexer_from_channel ~fname ic in
  catch_parse_error pfun (expand_include tokstrm)

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

module PA = struct

let header = parser
| [< '(_, T_OPENQASM r) >] -> r

let rec expr0 = parser
| [< '(aux, T_ID id) >] -> (aux, CST.ID id)
| [< '(aux, T_INTEGER n) >] ->
   if n < 0 then raise (SyntaxError "negative integer not valid in expression")
   else (aux, CST.NNINT n)
| [< '(aux, T_REAL r) >] -> (aux, CST.REAL r)
| [< '(aux, T_PI) >] -> (aux, CST.PI)
| [< '(aux1, T_LPAREN) ; (aux2, e) = expr ; '(aux3, T_RPAREN) >] ->
   (TA.appendlist [aux1; aux2; aux3], e)

| [< '(aux1, T_SIN) ; '(aux2, T_LPAREN) ; (aux3, e) = expr ; '(aux4, T_RPAREN) >] ->
   (TA.appendlist [aux1; aux2; aux3; aux4], CST.SIN e)

| [< '(aux1, T_COS) ; '(aux2, T_LPAREN) ; (aux3, e) = expr ; '(aux4, T_RPAREN) >] ->
   (TA.appendlist [aux1; aux2; aux3; aux4], CST.COS e)

| [< '(aux1, T_TAN) ; '(aux2, T_LPAREN) ; (aux3, e) = expr ; '(aux4, T_RPAREN) >] ->
   (TA.appendlist [aux1; aux2; aux3; aux4], CST.TAN e)

| [< '(aux1, T_EXP) ; '(aux2, T_LPAREN) ; (aux3, e) = expr ; '(aux4, T_RPAREN) >] ->
   (TA.appendlist [aux1; aux2; aux3; aux4], CST.EXP e)

| [< '(aux1, T_LN) ; '(aux2, T_LPAREN) ; (aux3, e) = expr ; '(aux4, T_RPAREN) >] ->
   (TA.appendlist [aux1; aux2; aux3; aux4], CST.LN e)

| [< '(aux1, T_SQRT) ; '(aux2, T_LPAREN) ; (aux3, e) = expr ; '(aux4, T_RPAREN) >] ->
   (TA.appendlist [aux1; aux2; aux3; aux4], CST.SQRT e)

and expr1 = parser
| [< (aux1, e1)=expr0 ;
   rv=(parser
       | [< '(aux2, T_CARET) ; (aux3, e2)=expr1 >] -> (TA.appendlist [aux1; aux2; aux3], CST.POW(e1, e2))
      | [< >] -> (aux1, e1)) >] -> rv

and expr2 = parser
| [< '(aux1, T_DASH) ; (aux2, e)=expr2 >] -> (TA.append aux1 aux2, CST.UMINUS e)
| [< '(aux1, T_PLUS) ; (aux2, e)=expr2 >] -> (TA.append aux1 aux2, e)
| [< e=expr1 >] -> e

and expr3 = parser
| [< rv=ne_plist_with_sep_function
          (parser
           | [< '(aux2, T_STAR) >] ->
           (fun (aux1, e1) (aux3, e2) -> (TA.appendlist [aux1; aux2; aux3], CST.MUL(e1, e2)))
          | [< '(aux2, T_SLASH) >] ->
             (fun (aux1, e1) (aux3, e2) -> (TA.appendlist [aux1; aux2; aux3], CST.DIV(e1, e2))))
          expr2 >] -> rv

and expr4 = parser
| [< rv=ne_plist_with_sep_function
          (parser
           | [< '(aux2, T_PLUS) >] -> (fun (aux1, e1) (aux3, e2) -> (TA.appendlist [aux1; aux2; aux3], CST.ADD(e1, e2)))
          | [< '(aux2, T_DASH) >] -> (fun (aux1, e1) (aux3, e2) -> (TA.appendlist [aux1; aux2; aux3], CST.SUB(e1, e2))))
          expr3 >] -> rv

and expr = parser
| [< e=expr4 >] -> e

let id_or_indexed = parser
| [< '(aux1, T_ID id) ; rv=(parser
                        | [< '(aux2, T_LBRACKET); '(aux3, T_INTEGER n); '(aux4, T_RBRACKET) >] ->
                        if n < 0 then raise (SyntaxError "negative integer not valid in register index")
                        else (TA.appendlist [aux1; aux2; aux3; aux4], CST.BIT(id, n))
                        | [< >] -> (aux1, CST.REG id)
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

let ne_id_or_indexed_list strm = ne_plist_with_sep_function (aux_comma (fun h t -> h@t)) (as_list_lift_aux id_or_indexed) strm

let ne_id_list strm = ne_plist_with_sep_function (aux_comma (fun h t -> h@t)) (as_list_lift_aux id) strm

let uop = parser
| [< '(aux1, T_U) ; '(aux2, T_LPAREN) ; (aux3, el)=ne_explist ; '(aux4, T_RPAREN) ; (aux5, a)=id_or_indexed ; '(aux6, T_SEMICOLON) >] ->
   (TA.appendlist [aux1; aux2; aux3; aux4; aux5; aux6], CST.U(el, a))
| [< '(aux1,T_CX) ; (aux2, a1)=id_or_indexed ; '(aux3, T_COMMA) ; (aux4, a2)=id_or_indexed ; '(aux5, T_SEMICOLON) >] ->
   (TA.appendlist [aux1; aux2; aux3; aux4; aux5], CST.CX(a1, a2))
| [< '(aux1, T_ID gateid) ;
   (aux2, params)=(parser
                     [< '(paux1, T_LPAREN); (paux2, l)=possibly_empty ne_explist; '(paux3, T_RPAREN) >] ->
                   (TA.appendlist [paux1; paux2; paux3], l)
                  | [< >] -> (TA.mt, [])
                  ) ;
   (aux3, regs)=ne_id_or_indexed_list ;
    '(aux4, T_SEMICOLON) >] ->
   (TA.appendlist [aux1; aux2; aux3; aux4], CST.COMPOSITE_GATE(gateid, params, regs))

let qop = parser
| [< (aux,u)=uop >] -> (aux, CST.UOP u)

| [< '(aux1, T_MEASURE) ; (aux2, l)=id_or_indexed ; '(aux3, T_DASHGT) ; (aux4, r)=id_or_indexed ; '(aux5, T_SEMICOLON) >] ->
   (TA.appendlist [aux1; aux2; aux3; aux4; aux5], CST.MEASURE(l, r))
| [< '(aux1, T_RESET) ; (aux2, l)=id_or_indexed ; '(aux3, T_SEMICOLON) >] ->
   (TA.appendlist [aux1; aux2; aux3], CST.RESET(l))

let gop = parser
| [< (aux, i)=uop >] -> (aux, CST.GATE_UOP i)
| [< '(aux1, T_BARRIER) ; (aux2, l)=ne_id_list; '(aux3, T_SEMICOLON) >] ->
   (TA.appendlist [aux1; aux2; aux3], CST.GATE_BARRIER l)

let as_list pfun strm = (parser [< rv=pfun >] -> (TA.mt, [rv])) strm
let ne_gop_list strm =
  let h,t = ne_plist gop strm in h::t

let gatedecl = parser
| [< '(aux1, T_GATE) ; '(aux2, T_ID gateid) ;
   (aux2, formal_params)=(parser
                            [< '(paux1, T_LPAREN); (paux2, l)=ne_id_list ; '(paux3, T_RPAREN) >] ->
                          (TA.appendlist [paux1; paux2; paux3], l)
                         | [< >] -> (TA.mt, [])
                         ) ;
   (aux3, formal_bits)=ne_id_list ;
   '(aux4, T_LBRACE) ;
   gopl=ne_gop_list ;
   '(aux6, T_RBRACE) >] ->
   (TA.appendlist [aux1; aux2; aux3; aux4; aux6],
    CST.STMT_GATEDECL(gateid, formal_params, formal_bits, gopl))

let opaquedecl = parser
| [< '(aux1, T_OPAQUE) ; '(aux2, T_ID gateid) ;
   (aux3, formal_params)=(parser
                            [< '(paux1, T_LPAREN); (paux2, l)=possibly_empty ne_id_list ; '(paux3, T_RPAREN) >] ->
                          (TA.appendlist [paux1; paux2; paux3], l)
                 | [< >] -> (TA.mt, [])
                 ) ;
   (aux4, formal_bits)=ne_id_list ;
   '(aux5, T_SEMICOLON)
 >] -> (TA.appendlist [aux1; aux2; aux3; aux4; aux5], CST.STMT_OPAQUEDECL(gateid, formal_params, formal_bits))

let reg_decl=parser
| [< '(aux1, T_QREG) ; '(aux2, T_ID id); '(aux3, T_LBRACKET) ; '(aux4, T_INTEGER n) ; '(aux5, T_RBRACKET) ; '(aux6, T_SEMICOLON) >] ->
   if n < 0 then raise (SyntaxError "negative integer not valid in qreg statement")
   else (TA.appendlist [aux1; aux2; aux3; aux4; aux5; aux6], CST.STMT_QREG(id, n)) ;
| [< '(aux1, T_CREG) ; '(aux2, T_ID id); '(aux3, T_LBRACKET) ; '(aux4, T_INTEGER n) ; '(aux5, T_RBRACKET) ; '(aux6, T_SEMICOLON) >] ->
   if n < 0 then raise (SyntaxError "negative integer not valid in creg statement")
   else (TA.appendlist [aux1; aux2; aux3; aux4; aux5; aux6], CST.STMT_CREG(id, n))


let statement = parser
| [< d=reg_decl >] -> d
| [< d=gatedecl >] -> d
| [< d=opaquedecl >] -> d
| [< (aux, i)=qop >] -> (aux, CST.STMT_QOP i)
| [< '(aux1, T_IF) ; '(aux2, T_LPAREN) ; '(aux3, T_ID id) ; '(aux4, T_EQEQ) ; '(aux5, T_INTEGER n) ; '(aux6, T_RPAREN) ; (aux7, i)=qop >] ->
   if n < 0 then raise (SyntaxError "negative integer not valid in if statment")
   else (TA.appendlist [aux1; aux2; aux3; aux4; aux5; aux6; aux7], CST.STMT_IF(id, n, i))
| [< '(aux1, T_BARRIER) ;
   (aux2, l)=ne_id_or_indexed_list ;
   '(aux3, T_SEMICOLON) >] ->
   (TA.appendlist [aux1; aux2; aux3], CST.STMT_BARRIER l)

let ne_statement_list strm = let (h, t) = ne_plist statement strm in h::t

let program strm = ne_statement_list strm

let mainprogram = parser
| [< vers=header ; l=program >] -> (vers, l)

end
