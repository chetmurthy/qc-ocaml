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

module TA = TokenAux

module CST = struct
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


  type id_or_indexed_t =
    | REG of string
    | BIT of string * int

  type raw_uop_t =
    | U of expr list * id_or_indexed_t
    | CX of id_or_indexed_t * id_or_indexed_t
    | COMPOSITE_GATE of string * expr list * id_or_indexed_t list

  type raw_qop_t =
    | UOP of raw_uop_t
    | MEASURE of id_or_indexed_t * id_or_indexed_t
    | RESET of id_or_indexed_t

  type 'aux qop_t =
    'aux * raw_qop_t

  type raw_gate_op_t =
    GATE_UOP of raw_uop_t
  | GATE_BARRIER of string list

  type 'aux gate_op_t =
    'aux * raw_gate_op_t

  type 'aux raw_stmt_t =
    | STMT_GATEDECL of string * string list * string list * 'aux gate_op_t list
    | STMT_OPAQUEDECL of string * string list * string list
    | STMT_QOP of raw_qop_t
    | STMT_IF of string * int * raw_qop_t
    | STMT_BARRIER of id_or_indexed_t list
    | STMT_QREG of string * int
    | STMT_CREG of string * int

  type 'aux stmt_t = 'aux * 'aux raw_stmt_t

  type 'aux program_t = 'aux stmt_t list

  module AuxMap = struct
    type ('a, 'b) mappers_t = {
        stmt : 'a -> 'a raw_stmt_t -> 'b ;
        gop : 'a -> raw_gate_op_t -> 'b ;
      }

    let gop mappers (aux, raw_gop) =
      let aux' = mappers.gop aux raw_gop in
      (aux', raw_gop)

    let raw_stmt mappers = function
      | STMT_GATEDECL(gateid, formal_params, formal_qregs, gopl) ->
         STMT_GATEDECL(gateid, formal_params, formal_qregs,
                           List.map (gop mappers) gopl)

      | STMT_OPAQUEDECL(a, b, c) -> STMT_OPAQUEDECL(a, b, c)
      | STMT_QOP q -> STMT_QOP q
      | STMT_IF(a, b, c) -> STMT_IF(a, b, c)
      | STMT_BARRIER l -> STMT_BARRIER l
      | STMT_QREG (a,b) -> STMT_QREG (a,b)
      | STMT_CREG (a, b) -> STMT_CREG (a, b)

    let stmt mappers (aux, raw_stmt0) =
      let aux' = mappers.stmt aux raw_stmt0 in
      let raw_stmt' = raw_stmt mappers raw_stmt0 in
      (aux', raw_stmt')

    let program mappers l = List.map (stmt mappers) l
  end

end

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
       | [< '(aux2, T_CARET) ; (aux3, e2)=expr1 >] -> (TA.appendlist [aux1; aux2; aux3], CST.XOR(e1, e2))
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

module AST = struct
  type param_var_t =
    | CPARAM of string

  type gate_bit_t =
    | QUBIT of string

  type main_var_t =
    | CREG of string
    | QREG of string

  type 'paramvar expr =
    | ID of 'paramvar
    | REAL of RealNumeral.t
    | NNINT of int
    | PI
    | ADD of 'paramvar expr * 'paramvar expr
    | SUB of 'paramvar expr * 'paramvar expr
    | MUL of 'paramvar expr * 'paramvar expr
    | DIV of 'paramvar expr * 'paramvar expr
    | UMINUS of 'paramvar expr
    | XOR of 'paramvar expr * 'paramvar expr
    | SIN of 'paramvar expr
    | COS of 'paramvar expr
    | TAN of 'paramvar expr
    | EXP of 'paramvar expr
    | LN of 'paramvar expr
    | SQRT of 'paramvar expr

  type cparamvar_t = CPARAMVAR of string
  type qubit_t = QUBIT of string
  type qreg_t = QREG of string
  type creg_t = CREG of string

  type 'a or_indexed =
    | IT of 'a
    | INDEXED of 'a * int

  type empty_t

  type ('paramvar, 'qregvar) raw_uop_t =
    | U of 'paramvar expr list * 'qregvar
    | CX of 'qregvar * 'qregvar
    | COMPOSITE_GATE of string * 'paramvar expr list * 'qregvar list

  type raw_qop_t =
    | UOP of (empty_t, qreg_t or_indexed) raw_uop_t
    | MEASURE of qreg_t or_indexed * creg_t or_indexed
    | RESET of qreg_t or_indexed


  type  'aux qop_t =
    'aux * raw_qop_t

  type  raw_gate_op_t =
    GATE_UOP of (cparamvar_t, qubit_t) raw_uop_t
  | GATE_BARRIER of qubit_t list

  type 'aux gate_op_t =
    'aux * raw_gate_op_t

  type 'aux gatedecl_t = string * string list * string list * 'aux gate_op_t list

  type 'aux raw_stmt_t =
    | STMT_GATEDECL of 'aux gatedecl_t
    | STMT_OPAQUEDECL of string * string list * string list
    | STMT_QOP of raw_qop_t
    | STMT_IF of creg_t * int * raw_qop_t
    | STMT_BARRIER of qreg_t or_indexed list
    | STMT_QREG of string * int
    | STMT_CREG of string * int

  type 'aux stmt_t = 'aux * 'aux raw_stmt_t

  type 'aux program_t = 'aux stmt_t list

  module AuxMap = struct
    type ('a, 'b) mappers_t = {
        stmt : 'a -> 'a raw_stmt_t -> 'b ;
        gop : 'a -> raw_gate_op_t -> 'b ;
      }

    let gop mappers (aux, raw_gop) =
      let aux' = mappers.gop aux raw_gop in
      (aux', raw_gop)

    let raw_stmt mappers = function
      | STMT_GATEDECL(gateid, formal_params, formal_qregs, gopl) ->
         STMT_GATEDECL(gateid, formal_params, formal_qregs,
                           List.map (gop mappers) gopl)

      | STMT_OPAQUEDECL(a, b, c) -> STMT_OPAQUEDECL(a, b, c)
      | STMT_QOP q -> STMT_QOP q
      | STMT_IF(a, b, c) -> STMT_IF(a, b, c)
      | STMT_BARRIER l -> STMT_BARRIER l
      | STMT_QREG (a,b) -> STMT_QREG (a,b)
      | STMT_CREG (a, b) -> STMT_CREG (a, b)

    let stmt mappers (aux, raw_stmt0) =
      let aux' = mappers.stmt aux raw_stmt0 in
      let raw_stmt' = raw_stmt mappers raw_stmt0 in
      (aux', raw_stmt')

    let program mappers l = List.map (stmt mappers) l
  end

end

module TYCHK = struct
  open Coll

  exception TypeError of bool * string

  module Env = struct
    type 'a t = {
        gates: (string, 'a AST.gatedecl_t) LM.t ;
        qregs: (string, int) LM.t ;
        cregs: (string, int) LM.t ;
      }

    let equal e1 e2 =
      let canon x = List.sort Pervasives.compare x in
      (e1.gates |> LM.toList |> canon) = (e2.gates |> LM.toList |> canon) &&
      (e1.qregs |> LM.toList |> canon) = (e2.qregs |> LM.toList |> canon) &&
      (e1.cregs |> LM.toList |> canon) = (e2.cregs |> LM.toList |> canon)

    let mk () = { gates = LM.mk() ; qregs = LM.mk() ; cregs = LM.mk() }

    let add_gate envs (id, v) = { envs with gates = LM.add envs.gates (id,v) }
    let add_qreg envs (id, v) = { envs with qregs = LM.add envs.qregs (id,v) }
    let add_creg envs (id, v) = { envs with cregs = LM.add envs.cregs (id,v) }

    let has_gate envs id = LM.in_dom envs.gates id

    let must_have_gate envs id =
      if not (has_gate envs id) then
        raise (TypeError (false, Printf.sprintf "gate %s not declared" id))

    let must_not_have_gate envs id =
      if has_gate envs id then
        raise (TypeError (false, Printf.sprintf "gate %s already declared" id))

    let has_creg envs id = LM.in_dom envs.cregs id
    let intern_creg envs id =
      if has_creg envs id then AST.CREG id
      else raise (TypeError(false, Printf.sprintf "creg %s not declared" id))

    let must_not_have_creg envs id =
      if has_creg envs id then
        raise (TypeError(false, Printf.sprintf "creg %s already declared" id))

    let has_qreg envs id = LM.in_dom envs.qregs id
    let intern_qreg envs id =
      if has_qreg envs id then AST.QREG id
      else raise (TypeError(false, Printf.sprintf "qreg %s not declared" id))
    let lookup_qreg envs id =
      if has_qreg envs id then LM.map envs.qregs id
      else raise (TypeError(false, Printf.sprintf "qreg %s not declared" id))

    let must_not_have_qreg envs id =
      if has_qreg envs id then
        raise (TypeError(false, Printf.sprintf "qreg %s already declared" id))

    let auxmap mappers env =
      let map_gatedecl (a,b,c,gopl) =
        let rst = AST.STMT_GATEDECL(a,b,c,gopl) in
        match AST.AuxMap.raw_stmt mappers rst with
        | AST.STMT_GATEDECL(a,b,c,gopl) -> (a,b,c,gopl)
        | _ -> assert false in

      let gates =
        env.gates
        |> LM.toList
        |> List.map (fun (k,v) -> (k, map_gatedecl v))
        |> LM.ofList () in

      { env with gates = gates }

  end

  let _expr cparam (cst : CST.expr) =
    let rec erec = function
      | CST.ID id -> AST.ID (cparam id)
      | REAL r -> AST.REAL r
      | NNINT n -> AST.NNINT n
      | PI -> AST.PI
      | ADD(e1, e2) -> AST.ADD(erec e1, erec e2)
      | SUB(e1, e2) -> AST.SUB(erec e1, erec e2)
      | MUL(e1, e2) -> AST.MUL(erec e1, erec e2)
      | DIV(e1, e2) -> AST.DIV(erec e1, erec e2)
      | UMINUS e1 -> AST.UMINUS(erec e1)
      | XOR(e1, e2) -> AST.XOR(erec e1, erec e2)
      | SIN e1 -> AST.SIN(erec e1)
      | COS e1 -> AST.COS(erec e1)
      | TAN e1 -> AST.TAN(erec e1)
      | EXP e1 -> AST.EXP(erec e1)
      | LN e1 -> AST.LN(erec e1)
      | SQRT e1 -> AST.SQRT(erec e1)
    in erec cst

  let intern_or_indexed f envs arg =
    match arg with
    | CST.REG id -> AST.IT (f envs id)
    | BIT (id, n) -> AST.INDEXED(f envs id, n)

  let raw_uop envs cparam qargs = function
    | CST.U(el, qr) -> AST.U(List.map (_expr cparam) el, [qr] |> qargs |> List.hd)
    | CX(qr1, qr2) ->
       let [qr1; qr2] = qargs [qr1; qr2] in
       AST.CX(qr1, qr2)
    | COMPOSITE_GATE(gateid, cparam_actuals, qal) ->
       Env.must_have_gate envs gateid ;
       AST.COMPOSITE_GATE(gateid, List.map (_expr cparam) cparam_actuals,
                          qargs qal)

  let raw_gate_op envs cparam qargs (cst: CST.raw_gate_op_t) =
    match cst with
    | CST.GATE_UOP u ->
       AST.GATE_UOP (raw_uop envs cparam qargs u)
    | GATE_BARRIER l -> AST.GATE_BARRIER(l |> List.map (fun id -> CST.REG id) |> qargs)

  let gate_op envs cparam qargs (aux, cst) =
    try
      (aux, raw_gate_op envs cparam qargs cst)
    with TypeError (false, msg) ->
      let spos = TokenAux.startpos aux in
      let epos = TokenAux.endpos aux in
      let msg = Printf.sprintf "Error file \"%s\", chars %d-%d: %s"
                  spos.Lexing.pos_fname
                  spos.Lexing.pos_cnum
                  epos.Lexing.pos_cnum
                  msg in
      raise (TypeError (true, msg))

    (* to convert and typecheck a list of qargs (register OR bit):

       (0) check they exist in envs.qregs and convert

       (1) check that all qargs are distinct

           --> so all registers are distinct
           --> all bits are distinct

       (2) check that all registers are of identical dimension

       (3) check that all indexes of BITs are 0 <= index < {dimension of their register}

       (4) check that no register-of-a-BIT is also a register qarg

     *)
  let convert_qargs envs l =
    let _qarg x = intern_or_indexed Env.intern_qreg envs x in
    (*0*)
    let conv_qargs = List.map _qarg l in
    (*1*)
    if not (distinct conv_qargs) then
      raise (TypeError(false, "qargs are not distinct")) ;

    (*2*)
    let registers = filter (function (AST.IT _) -> true | _ -> false) conv_qargs  in
    let register_ids = List.map (fun (AST.IT (AST.QREG id)) -> id) registers in
    let register_dims = List.map (Env.lookup_qreg envs) register_ids in
    begin match register_dims with
    | [] -> ()
    | h::t ->
       if not (for_all ((=) h) t) then
         raise (TypeError(false, "registers with different dimensions in qargs"))
    end ;

    (*3*)
    let bits = filter (function (AST.INDEXED _) -> true | _ -> false) conv_qargs  in
    let bits_data = List.map (function (AST.INDEXED (AST.QREG id, n)) ->
                                ((id, n), Env.lookup_qreg envs id)) bits in
    List.iter (fun ((id, n), dimension) ->
        if not (0 <= n && n < dimension) then
          raise (TypeError(false, Printf.sprintf "bit %s[%d] out of dimension [0..%d)" id n dimension))
      ) bits_data ;

    (*4*)
    List.iter (fun ((id, n), dimension) ->
        if List.mem id register_ids then
          raise (TypeError(false, Printf.sprintf "bit %s[%d] conflicts with register of same name" id n)) ;
      ) bits_data ;

    conv_qargs

  let raw_qop envs (cst: CST.raw_qop_t) =
    let cparam id =
      raise (TypeError(false, Printf.sprintf "cparams not permitted here (\"%s\")" id)) in
    let carg x = intern_or_indexed Env.intern_creg envs x in
    match cst with
    | CST.UOP u ->
       AST.UOP(raw_uop envs cparam (convert_qargs envs) u)

    | MEASURE(q, c) ->
       AST.MEASURE([q] |> convert_qargs envs |> List.hd, carg c)

    | RESET q -> AST.RESET ([q] |> convert_qargs envs |> List.hd)

  let qop envs (aux, cst) =
    try
      (aux, raw_qop envs cst)
    with TypeError (false, msg) ->
      let spos = TokenAux.startpos aux in
      let epos = TokenAux.endpos aux in
      let msg = Printf.sprintf "Error file \"%s\", chars %d-%d: %s"
                  spos.Lexing.pos_fname
                  spos.Lexing.pos_cnum
                  epos.Lexing.pos_cnum
                  msg in
      raise (TypeError (true, msg))

  let raw_stmt envs (cst: 'aux CST.raw_stmt_t) =
    match cst with
    | CST.STMT_GATEDECL(gateid, param_formals, qubit_formals, gopl) ->
       Env.must_not_have_gate envs gateid ;
       let cparam id =
         if List.mem id param_formals then AST.CPARAMVAR id
         else raise (TypeError(false, Printf.sprintf "cparam %s not declared" id)) in
       let _qarg = function
         | CST.REG id ->
            if List.mem id qubit_formals then AST.QUBIT id
            else raise (TypeError(false, Printf.sprintf "qarg %s not declared" id))
         | BIT (id, n) ->
            raise (TypeError(false, Printf.sprintf "qarg %s[%n] not permitted in composite-gate definition body" id n)) in
       let qargs l =
         if not (distinct l) then
           raise (TypeError(false, "qargs are not distinct")) ;
         List.map _qarg l
       in

       AST.STMT_GATEDECL(gateid, param_formals, qubit_formals,
                         List.map (gate_op envs cparam qargs) gopl)
      
    | STMT_OPAQUEDECL(gateid, param_formals, qubit_formals) ->
       AST.STMT_OPAQUEDECL(gateid, param_formals, qubit_formals)

    | STMT_QOP q -> AST.STMT_QOP(raw_qop envs q)

    | STMT_IF(id, n, q) ->
       AST.STMT_IF(Env.intern_creg envs id, n, raw_qop envs q)

    | STMT_BARRIER l ->
       AST.STMT_BARRIER(convert_qargs envs l)

    | STMT_QREG(id, n) ->
       Env.must_not_have_qreg envs id ;
       AST.STMT_QREG(id, n)

    | STMT_CREG(id, n) ->
       Env.must_not_have_creg envs id ;
       AST.STMT_CREG(id, n)

  let stmt envs (aux, rst) =
    try
      (aux,raw_stmt envs rst)
    with TypeError (false, msg) ->
      let spos = TokenAux.startpos aux in
      let epos = TokenAux.endpos aux in
      let msg = Printf.sprintf "Error file \"%s\", chars %d-%d: %s"
                  spos.Lexing.pos_fname
                  spos.Lexing.pos_cnum
                  epos.Lexing.pos_cnum
                  msg in
      raise (TypeError (true, msg))

  let program l =
    let stmt1 envs cst =
      let (aux, rst as stmt) = stmt envs cst in
      match rst with
      | STMT_QREG(id, n) -> (Env.add_qreg envs (id, n), stmt)
      | STMT_CREG(id, n) -> (Env.add_creg envs (id, n), stmt)
      | STMT_GATEDECL(gateid, param_formals, qubit_formals, gopl) ->
         (Env.add_gate envs (gateid, (gateid, param_formals, qubit_formals, gopl)),
          stmt)
      | _ -> (envs, stmt)
    in
    let (envs, l) =
      List.fold_left (fun (envs,acc) stmt ->
          let (envs, stmt) = stmt1 envs stmt in
          (envs, stmt::acc))
        (Env.mk(), []) l
    in (envs, List.rev l)
end
