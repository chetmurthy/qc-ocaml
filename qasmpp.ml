(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open Misc_functions
open Qasmsyntax
open Qasmparser

module PP = struct

  let header vers = [< 'Printf.sprintf "OPENQASM %s;\n" vers >]

  let rec expr0 = function
    | Ast.ID id -> [< 'id >]
    | Ast.NNINT n -> [< 'string_of_int n >]
    | Ast.REAL r -> [< 'r >]
    | Ast.PI -> [< '"pi" >]
    | Ast.SIN e -> [< '"sin(" ; expr e ; '")" >]
    | Ast.COS e -> [< '"cos(" ; expr e ; '")" >]
    | Ast.TAN e -> [< '"tan(" ; expr e ; '")" >]
    | Ast.EXP e -> [< '"exp(" ; expr e ; '")" >]
    | Ast.LN e -> [< '"ln(" ; expr e ; '")" >]
    | Ast.SQRT e -> [< '"sqrt(" ; expr e ; '")" >]
    | e -> [< '"(" ; expr e ; '")" >]

  and expr1 = function
    | Ast.XOR(e1, e2) -> [< expr0 e1 ; '" ^ " ; expr1 e2 >]
    | e -> expr0 e

  and expr2 = function
    | Ast.UMINUS e -> [< '"-" ; expr2 e >]
    | e -> expr1 e

  and expr3 = function
    | Ast.MUL(e1, e2) -> [< expr3 e1 ; '" * " ; expr2 e2 >]
    | Ast.DIV(e1, e2) -> [< expr3 e1 ; '" / " ; expr2 e2 >]
    | e -> expr2 e

  and expr4 = function
    | Ast.ADD(e1, e2) -> [< expr4 e1 ; '" + " ; expr3 e2 >]
    | Ast.SUB(e1, e2) -> [< expr4 e1 ; '" - " ; expr3 e2 >]
    | e -> expr3 e

  and expr e = expr4 e

  let bit_or_reg = function
    | Ast.BIT (id, n) -> [< 'id ; '"[" ; 'string_of_int n ; '"]" >]
    | Ast.REG id -> [< 'id >]

let pr_comma = (fun () -> [< '", " >])

  let raw_instruction ?(prefix="") = function
    | Ast.U(el, a) -> [< 'prefix ; '"U(" ; prlist_with_sep pr_comma expr el ; '") " ; bit_or_reg a ; '";\n" >]
    | Ast.CX(l, r) -> [< 'prefix ; '"CX " ; bit_or_reg l ; '", "; bit_or_reg r ; '";\n"; >]
    | Ast.COMPOSITE_GATE(gateid, params, regs) ->
       [< 'prefix ; 'gateid ; '" (" ; prlist_with_sep pr_comma expr params  ; '") " ; prlist_with_sep pr_comma bit_or_reg regs ; '";\n" >]
    | Ast.MEASURE(l, r) -> [< 'prefix ; '"measure " ; bit_or_reg l ; '", "; bit_or_reg r ; '";\n" >]
    | Ast.RESET l -> [< 'prefix ; '"reset " ; bit_or_reg l ; '";\n" >]

  let instruction ~prefix (aux, i) =
    let commentstring = TA.comment_string aux in
    match commentstring with
    | "" -> raw_instruction ~prefix i
    | _ -> [< 'commentstring ; raw_instruction ~prefix i >]

  let pr_id s = [< 's >]

  let raw_gate_op = function
    | Ast.GATE_INSTRUCTION i -> raw_instruction ~prefix:"  " i
    | Ast.GATE_BARRIER l -> [< '"  " ; '"barrier "; prlist_with_sep pr_comma pr_id l ; '";\n" >]

  let gate_op (aux, gop) =
    let commentstring = TA.comment_string aux in
    match commentstring with
    | "" -> raw_gate_op gop
    | _ -> [< 'commentstring ; raw_gate_op gop >]

  let raw_stmt = function
    | Ast.STMT_GATEDECL(gateid, formal_params, formal_bits, gopl) ->
       [< '"gate " ; 'gateid ; '" (" ; prlist_with_sep pr_comma pr_id formal_params ; '") " ;
        prlist_with_sep pr_comma pr_id formal_bits ; '" {\n" ;
        prlist gate_op gopl ;
        '"}\n" >]
    | Ast.STMT_OPAQUEDECL(gateid, formal_params, formal_bits) ->
       [< '"opaque " ; 'gateid ; '" (" ; prlist_with_sep pr_comma pr_id formal_params ; '") " ;
        prlist_with_sep pr_comma pr_id formal_bits ; '";\n" >]
    | Ast.STMT_INSTRUCTION i -> raw_instruction i
    | Ast.STMT_IF(reg, n, i) ->
       [< '"if (" ; pr_id reg ; '"=="; 'string_of_int n ; '") "; raw_instruction i >]
    | Ast.STMT_BARRIER l ->
       [< '"barrier " ; prlist_with_sep pr_comma pr_id l ; '";\n" >]
    | Ast.STMT_QREG(id, n) ->
       [< '"qreg " ; 'id ; '"[" ; 'string_of_int n ; '"];\n" >]
    | Ast.STMT_CREG(id, n) ->
       [< '"creg " ; 'id ; '"[" ; 'string_of_int n ; '"];\n" >]
      
  let stmt (aux, s) =
    let commentstring = TA.comment_string aux in
    match commentstring with
    | "" -> raw_stmt s
    | _ -> [< 'commentstring ; raw_stmt s >]

  let program l =
    prlist stmt l

  let main (vers, l) =
    [< '"OPENQASM " ; 'vers ; '";\n" ;
     program l >]

  let pp ppfun arg =
    let strm = ppfun arg in
    let l = list_of_stream strm in
    String.concat "" l

end
