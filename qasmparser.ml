(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open Qasmsyntax

let pa_header = parser
| [< 'T_OPENQASM r >] -> r
                                         
