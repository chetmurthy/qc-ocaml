(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open Misc_functions
open Qasmsyntax
open Qasmparser

module CSTPP = struct

  let header vers = [< 'Printf.sprintf "OPENQASM %s;\n" vers >]

  let rec expr0 = function
    | CST.ID id -> [< 'id >]
    | CST.NNINT n -> [< 'string_of_int n >]
    | CST.REAL r -> [< 'r >]
    | CST.PI -> [< '"pi" >]
    | CST.SIN e -> [< '"sin(" ; expr e ; '")" >]
    | CST.COS e -> [< '"cos(" ; expr e ; '")" >]
    | CST.TAN e -> [< '"tan(" ; expr e ; '")" >]
    | CST.EXP e -> [< '"exp(" ; expr e ; '")" >]
    | CST.LN e -> [< '"ln(" ; expr e ; '")" >]
    | CST.SQRT e -> [< '"sqrt(" ; expr e ; '")" >]
    | e -> [< '"(" ; expr e ; '")" >]

  and expr1 = function
    | CST.XOR(e1, e2) -> [< expr0 e1 ; '" ^ " ; expr1 e2 >]
    | e -> expr0 e

  and expr2 = function
    | CST.UMINUS e -> [< '"-" ; expr2 e >]
    | e -> expr1 e

  and expr3 = function
    | CST.MUL(e1, e2) -> [< expr3 e1 ; '" * " ; expr2 e2 >]
    | CST.DIV(e1, e2) -> [< expr3 e1 ; '" / " ; expr2 e2 >]
    | e -> expr2 e

  and expr4 = function
    | CST.ADD(e1, e2) -> [< expr4 e1 ; '" + " ; expr3 e2 >]
    | CST.SUB(e1, e2) -> [< expr4 e1 ; '" - " ; expr3 e2 >]
    | e -> expr3 e

  and expr e = expr4 e

  let id_or_indexed = function
    | CST.BIT (id, n) -> [< 'id ; '"[" ; 'string_of_int n ; '"]" >]
    | CST.REG id -> [< 'id >]

let pr_comma = (fun () -> [< '", " >])

  let raw_uop ?(prefix="") = function
    | CST.U(el, a) -> [< 'prefix ; '"U(" ; prlist_with_sep pr_comma expr el ; '") " ; id_or_indexed a ; '";\n" >]
    | CST.CX(l, r) -> [< 'prefix ; '"CX " ; id_or_indexed l ; '", "; id_or_indexed r ; '";\n"; >]
    | CST.COMPOSITE_GATE(gateid, params, regs) ->
       [< 'prefix ; 'gateid ; '" (" ; prlist_with_sep pr_comma expr params  ; '") " ; prlist_with_sep pr_comma id_or_indexed regs ; '";\n" >]

  let raw_qop ?(prefix="") = function
    | CST.UOP u -> raw_uop ~prefix u
    | CST.MEASURE(l, r) -> [< 'prefix ; '"measure " ; id_or_indexed l ; '", "; id_or_indexed r ; '";\n" >]
    | CST.RESET l -> [< 'prefix ; '"reset " ; id_or_indexed l ; '";\n" >]

  let qop ~prefix (aux, i) =
    let commentstring = TA.comment_string aux in
    match commentstring with
    | "" -> raw_qop ~prefix i
    | _ -> [< 'commentstring ; raw_qop ~prefix i >]

  let pr_id s = [< 's >]

  let raw_gate_op = function
    | CST.GATE_UOP i -> raw_uop ~prefix:"  " i
    | CST.GATE_BARRIER l -> [< '"  " ; '"barrier "; prlist_with_sep pr_comma pr_id l ; '";\n" >]

  let gate_op (aux, gop) =
    let commentstring = TA.comment_string aux in
    match commentstring with
    | "" -> raw_gate_op gop
    | _ -> [< 'commentstring ; raw_gate_op gop >]

  let raw_stmt = function
    | CST.STMT_GATEDECL(gateid, formal_params, formal_bits, gopl) ->
       [< '"gate " ; 'gateid ; '" (" ; prlist_with_sep pr_comma pr_id formal_params ; '") " ;
        prlist_with_sep pr_comma pr_id formal_bits ; '" {\n" ;
        prlist gate_op gopl ;
        '"}\n" >]
    | CST.STMT_OPAQUEDECL(gateid, formal_params, formal_bits) ->
       [< '"opaque " ; 'gateid ; '" (" ; prlist_with_sep pr_comma pr_id formal_params ; '") " ;
        prlist_with_sep pr_comma pr_id formal_bits ; '";\n" >]
    | CST.STMT_QOP i -> raw_qop i
    | CST.STMT_IF(reg, n, i) ->
       [< '"if (" ; pr_id reg ; '"=="; 'string_of_int n ; '") "; raw_qop i >]
    | CST.STMT_BARRIER l ->
       [< '"barrier " ; prlist_with_sep pr_comma id_or_indexed l ; '";\n" >]
    | CST.STMT_QREG(id, n) ->
       [< '"qreg " ; 'id ; '"[" ; 'string_of_int n ; '"];\n" >]
    | CST.STMT_CREG(id, n) ->
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
