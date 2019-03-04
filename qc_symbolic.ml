(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open Qasmsyntax

module Expr = struct

  let rec expr = function
    | AST.ID _ -> assert false
    | REAL n -> float_of_string n
    | NNINT n -> float_of_int n
    | PI -> Float.pi
    | ADD (e1, e2) -> (expr e1) +. (expr e2)
    | SUB (e1, e2) -> (expr e1) -. (expr e2)

    | MUL (e1, e2) -> (expr e1) *. (expr e2)

    | DIV (e1, e2) -> (expr e1) /. (expr e2)

    | UMINUS e1 -> -. (expr e1)

    | POW (e1, e2) -> Float.pow (expr e1) (expr e2)

    | SIN e1 -> Float.sin (expr e1)

    | COS e1 -> Float.cos (expr e1)

    | TAN e1 -> Float.tan (expr e1)

    | EXP e1 -> Float.exp (expr e1)

    | LN e1 -> Float.log (expr e1)

    | SQRT e1 -> Float.sqrt (expr e1)

end

module Latex = struct

  let rec expr0 = function
    | AST.ID _ -> assert false
    | AST.NNINT n -> string_of_int n
    | AST.REAL r -> r
    | AST.PI -> "\\pi"
    | AST.SIN e -> Printf.sprintf"\\sin{\\left (%s \\right )}" (expr e)
    | AST.COS e -> Printf.sprintf"\\cos{\\left (%s \\right )}" (expr e)
    | AST.TAN e -> Printf.sprintf"\\tan{\\left (%s \\right )}" (expr e)
    | AST.EXP e -> Printf.sprintf"e^{%s}" (expr e)
    | AST.LN e -> Printf.sprintf"\\log{\\left (%s \\right )}" (expr e)
    | AST.SQRT e -> Printf.sprintf"\\sqrt{\\left (%s \\right )}" (expr e)
    | e -> Printf.sprintf"\\left (%s \\right )" (expr e)

  and expr1 = function
    | AST.POW(e1, e2) -> Printf.sprintf "%s^{%s}" (expr0 e1) (expr1 e2)
    | e -> expr0 e

  and expr2 = function
    | AST.UMINUS e -> Printf.sprintf "- %s" (expr1 e)
    | e -> expr1 e

  and expr3 = function
    | AST.MUL(e1, e2) -> Printf.sprintf "%s * %s" (expr3 e1) (expr2 e2)
    | AST.DIV(e1, e2) -> Printf.sprintf "\\frac{%s}{%s}" (expr3 e1) (expr2 e2)
    | e -> expr2 e

  and expr4 = function
    | AST.ADD(e1, e2) -> Printf.sprintf "%s + %s" (expr4 e1) (expr3 e2)
    | AST.SUB(e1, e2) -> Printf.sprintf "%s - %s" (expr4 e1) (expr3 e2)
    | e -> expr3 e

  and expr e = expr4 e

end
