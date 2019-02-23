(* Copyright 2019 Chetan Murthy *)

open Qasmsyntax

let pa_header = parser
| [< 'T_OPENQASM r >] -> r
                                         
