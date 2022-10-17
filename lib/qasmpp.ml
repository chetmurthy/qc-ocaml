(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open Pa_ppx_utils
open Std
open Misc_functions
open Qasm2syntax
open Qasm2_parser
open Qc_environment

module CSTPP = struct

  let header pps vers = Fmt.(pf pps "OPENQASM %s;@." vers)

  let rec expr0 pps = function
    | CST.ID x -> Fmt.(pf pps "%s" x)
    | CST.NNINT n -> Fmt.(pf pps "%d" n)
    | CST.REAL r -> Fmt.(pf pps "%s" r)
    | CST.PI -> Fmt.(pf pps "pi")
    | CST.SIN e -> Fmt.(pf pps "sin(%a)" expr e)
    | CST.COS e -> Fmt.(pf pps "cos(%a)" expr e)
    | CST.TAN e -> Fmt.(pf pps "tan(%a)" expr e)
    | CST.EXP e -> Fmt.(pf pps "exp(%a)" expr e)
    | CST.LN e -> Fmt.(pf pps "ln(%a)" expr e)
    | CST.SQRT e -> Fmt.(pf pps "sqrt(%a)" expr e)
    | e -> Fmt.(pf pps "(%a)" expr e)

  and expr1 pps = function
    | CST.POW(e1, e2) -> Fmt.(pf pps "%a ^ %a" expr0 e1 expr1 e2)
    | e -> expr0 pps e

  and expr2 pps = function
    | CST.UMINUS e -> Fmt.(pf pps "- %a" expr2 e)
    | e -> expr1 pps e

  and expr3 pps = function
    | CST.MUL(e1, e2) -> Fmt.(pf pps "%a * %a" expr3 e1 expr2 e2)
    | CST.DIV(e1, e2) -> Fmt.(pf pps "%a / %a" expr3 e1 expr2 e2)
    | e -> expr2 pps e

  and expr4 pps = function
    | CST.ADD(e1, e2) -> Fmt.(pf pps "%a + %a" expr4 e1 expr3 e2)
    | CST.SUB(e1, e2) -> Fmt.(pf pps "%a - %a" expr4 e1 expr3 e2)
    | e -> expr3 pps e

  and expr pps e = expr4 pps e

  let id_or_indexed pps = function
    | CST.BIT (x, n) -> Fmt.(pf pps "%s[%d]" x n)
    | CST.REG x -> Fmt.(pf pps "%s" x)

  let raw_uop pps = function
    | CST.U(el, a) -> Fmt.(pf pps "U(%a) %a;@." (list ~sep:(const string ", ") expr) el id_or_indexed a)
    | CST.CX(l, r) -> Fmt.(pf pps "CX %a, %a;@." id_or_indexed l id_or_indexed r)
    | CST.COMPOSITE_GATE(gateid, [], regs) ->
       Fmt.(pf pps "%s %a;@." gateid (list ~sep:(const string ", ") id_or_indexed) regs)
    | CST.COMPOSITE_GATE(gateid, params, regs) ->
       Fmt.(pf pps "%s (%a) %a;@." gateid
              (list ~sep:(const string ", ") expr) params
              (list ~sep:(const string ", ") id_or_indexed) regs)

  let raw_qop pps = function
    | CST.UOP u -> raw_uop pps u
    | CST.MEASURE(l, r) -> Fmt.(pf pps "measure %a -> %a;@." id_or_indexed l id_or_indexed r)
    | CST.RESET l -> Fmt.(pf pps "reset %a;@." id_or_indexed l)

  let qop pps (aux, i) =
    let commentstring = cleanws (Ploc.comment aux) in
    match commentstring with
    | "" -> raw_qop pps i
    | _ -> Fmt.(pf pps "%s%a" commentstring raw_qop i)

  let pr_id pps s = Fmt.(pf pps "%s" s)

  let raw_gate_op pps = function
    | CST.GATE_UOP i -> raw_uop pps i
    | CST.GATE_BARRIER l -> Fmt.(pf pps "barrier %a;@." (list ~sep:(const string ", ") pr_id) l)

  let gate_op pps (aux, gop) =
    let commentstring = cleanws (Ploc.comment aux) in
    match commentstring with
    | "" -> raw_gate_op pps gop
    | _ -> Fmt.(pf pps "%s%a" commentstring raw_gate_op gop)

  let raw_stmt pps = function
    | CST.STMT_INCLUDE (_, fname, _) -> Fmt.(pf pps "include %a;@." (quote string) fname)
    | CST.STMT_GATEDECL(gateid, formal_params, formal_bits, gopl) ->
       Fmt.(pf pps "gate %s (%a) %a @[<2>{@.%a@.}@]@."
              gateid
              (list ~sep:(const string ", ") pr_id) formal_params
              (list ~sep:(const string ", ") pr_id) formal_bits
              (list gate_op) gopl)
    | CST.STMT_OPAQUEDECL(gateid, formal_params, formal_bits) ->
       Fmt.(pf pps "opaque %s (%a) %a;@."
              gateid
              (list ~sep:(const string ", ") pr_id) formal_params
              (list ~sep:(const string ", ") pr_id) formal_bits)
    | CST.STMT_QOP i -> raw_qop pps i
    | CST.STMT_IF(reg, n, i) ->
       Fmt.(pf pps "if (%a == %d) %a" pr_id reg n raw_qop i)
    | CST.STMT_BARRIER l ->
       Fmt.(pf pps "barrier %a;@." (list ~sep:(const string ", ") id_or_indexed) l)
    | CST.STMT_QREG(x, n) ->
       Fmt.(pf pps "qreg %s[%d];@." x n)
    | CST.STMT_CREG(x, n) ->
       Fmt.(pf pps "creg %s[%d];@." x n)

  let stmt pps (aux, s) =
    let commentstring = cleanws (Ploc.comment aux) in
    match commentstring with
    | "" -> raw_stmt pps s
    | _ -> Fmt.(pf pps "%s%a" commentstring raw_stmt s)

  let program pps l =
    Fmt.(pf pps "%a" (list stmt) l)

  let main pps (vers, l) =
    Fmt.(pf pps "OPENQASM %s;@.%a" vers program l)

end

module ASTPP = struct

  let header pps vers = Fmt.(pf pps "OPENQASM %s;@." vers)

  let expr pr_id pps e =
    let rec expr0 pps = function
      | AST.ID x -> Fmt.(pf pps "%a" pr_id x)
      | AST.NNINT n -> Fmt.(pf pps "%d" n)
      | AST.REAL r -> Fmt.(pf pps "%s" r)
      | AST.PI -> Fmt.(pf pps "pi")
      | AST.SIN e -> Fmt.(pf pps "sin(%a)" expr e)
      | AST.COS e -> Fmt.(pf pps "cos(%a)" expr e)
      | AST.TAN e -> Fmt.(pf pps "tan(%a)" expr e)
      | AST.EXP e -> Fmt.(pf pps "exp(%a)" expr e)
      | AST.LN e -> Fmt.(pf pps "ln(%a)" expr e)
      | AST.SQRT e -> Fmt.(pf pps "sqrt(%a)" expr e)
      | e -> Fmt.(pf pps "(%a)" expr e)

    and expr1 pps = function
      | AST.POW(e1, e2) -> Fmt.(pf pps "%a ^ %a" expr0 e1 expr1 e2)
      | e -> expr0 pps e

    and expr2 pps = function
      | AST.UMINUS e -> Fmt.(pf pps "- %a" expr2 e)
      | e -> expr1 pps e

    and expr3 pps = function
      | AST.MUL(e1, e2) -> Fmt.(pf pps "%a * %a" expr3 e1 expr2 e2)
      | AST.DIV(e1, e2) -> Fmt.(pf pps "%a / %a" expr3 e1 expr2 e2)
      | e -> expr2 pps e

    and expr4 pps = function
      | AST.ADD(e1, e2) -> Fmt.(pf pps "%a + %a" expr4 e1 expr3 e2)
      | AST.SUB(e1, e2) -> Fmt.(pf pps "%a - %a" expr4 e1 expr3 e2)
      | e -> expr3 pps e

    and expr pps e = expr4 pps e
  in expr pps e

  let or_indexed  pr_it pps = function
    | AST.INDEXED (x, n) -> Fmt.(pf pps "%a[%d]" pr_it x n)
    | AST.IT x -> pr_it pps x

  let pr_qreg pps (AST.QREG x) = Fmt.(pf pps "%s" x)
  let pr_creg pps (AST.CREG x) = Fmt.(pf pps "%s" x)

  let raw_uop ~pr_cparamvar ~pr_qregvar pps = function
    | AST.U(el, a) -> Fmt.(pf pps "U(%a) %a;@."
                             (list ~sep:(const string ", ") (expr pr_cparamvar)) el pr_qregvar a)
    | AST.CX(l, r) -> Fmt.(pf pps "CX %a, %a;@." pr_qregvar l pr_qregvar r)
    | AST.COMPOSITE_GATE(gateid, [], regs) ->
       Fmt.(pf pps "%s %a;@." gateid (list ~sep:(const string ", ") pr_qregvar) regs)
    | AST.COMPOSITE_GATE(gateid, params, regs) ->
       Fmt.(pf pps "%s (%a) %a;@."
              gateid
              (list ~sep:(const string ", ") (expr pr_cparamvar)) params
              (list ~sep:(const string ", ") pr_qregvar) regs)

  let pr_empty pps _ = assert false
  let raw_qop pps = function
    | AST.UOP u -> raw_uop ~pr_cparamvar:pr_empty ~pr_qregvar:(or_indexed pr_qreg) pps u
    | AST.MEASURE(l, r) -> Fmt.(pf pps "measure %a -> %a;@." (or_indexed pr_qreg) l (or_indexed pr_creg) r)
    | AST.RESET l -> Fmt.(pf pps "reset %a;@." (or_indexed pr_qreg) l)

  let qop pps (aux, i) =
    let commentstring = cleanws (Ploc.comment aux) in
    match commentstring with
    | "" -> raw_qop pps i
    | _ -> Fmt.(pf pps "%s%a" commentstring raw_qop i)

  let pr_cparamvar pps (AST.CPARAMVAR s) = Fmt.(pf pps "%s" s)

  let pr_qubit pps (AST.QUBIT x) = Fmt.(pf pps "%s" x)

  let raw_gate_op pps = function
    | AST.GATE_UOP i -> raw_uop ~pr_cparamvar ~pr_qregvar:pr_qubit pps i
    | AST.GATE_BARRIER l -> Fmt.(pf pps "barrier %a;@." (list ~sep:(const string ",") pr_qubit) l)

  let gate_op pps (aux, gop) =
    let commentstring = cleanws (Ploc.comment aux) in
    match commentstring with
    | "" -> raw_gate_op pps gop
    | _ -> Fmt.(pf pps "%s%a" commentstring raw_gate_op gop)

  let pr_id pps s = Fmt.(pf pps "%s" s)

  let raw_stmt pps = function
    | AST.STMT_INCLUDE(_, fname, _) -> Fmt.(pf pps "include \"%s\";@." fname)
    | AST.STMT_GATEDECL(gateid, formal_params, formal_bits, gopl) ->
       Fmt.(pf pps "gate %s (%a) %a @[<2>{@.%a}@]@."
              gateid
              (list ~sep:(const string ", ") pr_id) formal_params
              (list ~sep:(const string ", ") pr_id) formal_bits
              (list gate_op) gopl)
    | AST.STMT_OPAQUEDECL(gateid, formal_params, formal_bits) ->
       Fmt.(pf pps "opaque %s (%a) %a@."
              gateid
              (list ~sep:(const string ", ") pr_id) formal_params
              (list ~sep:(const string ", ") pr_id) formal_bits)
    | AST.STMT_QOP i -> raw_qop pps i
    | AST.STMT_IF(reg, n, i) ->
       Fmt.(pf pps "if (%a == %d) %a" pr_creg reg n raw_qop i)
    | AST.STMT_BARRIER l ->
       Fmt.(pf pps "barrier %a;@." (list ~sep:(const string ", ") (or_indexed pr_qreg)) l)
    | AST.STMT_QREG(x, n) ->
       Fmt.(pf pps "qreg %s[%d];@." x n)
    | AST.STMT_CREG(x, n) ->
       Fmt.(pf pps "creg %s[%d];@." x n)

  let stmt pps (aux, s) =
    let commentstring = cleanws (Ploc.comment aux) in
    match commentstring with
    | "" -> raw_stmt pps s
    | _ -> Fmt.(pf pps "%s%a" commentstring raw_stmt s)

  let program pps l =
    Fmt.(pf pps "@[<v>%a@]" (list stmt) l)

  let main pps (vers, l) =
    Fmt.(pf pps "OPENQASM %s;@.%a" vers program l)

end
                 
