(* Copyright 2019 Chetan Murthy, All rights reserved. *)
{
open Lexing
open Pa_ppx_utils
open Std
open Misc_functions
open Qasm0_tokens

let locate ~comments lb v =
  let loc = Ploc.make_unlined (Lexing.lexeme_start lb, Lexing.lexeme_end lb) in
  let loc = Ploc.with_comment loc comments in
  (v, loc)

}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let lc_alpha = ['a'-'z']
let uc_alpha = ['A'-'Z']
let alpha = (lc_alpha | uc_alpha)
let digit = ['0'-'9']
let number = digit+
let id = alpha (alpha | digit | '_' | '-')*
let sqstring = "'" [^ '\'']+ "'"
let comma_sep_ids = (id ",")* id

rule line = parse
  ("#" [^ '\n' '\r'] newline) as line { locate lexbuf (Comment line, line) }
| (white? "qubit" white (id as id) white? newline) as line { locate lexbuf (Qubit id, line) }
| (white? "cbit" white (id as id) white? newline) as line { locate lexbuf (Cbit id, line) }
| (white? "def" white (id as id)","(number as num)","(sqstring as tex) white? newline) as line
  { locate lexbuf (Def(id,int_of_string num,tex), line) }
| (white? "defbox" white (id as id)","(number as num1)","(number as num2)","(sqstring as tex) white? newline) as line
  { locate lexbuf (Defbox(id,int_of_string num1,int_of_string num2,tex), line) }
| (white? (id as id) white (comma_sep_ids as ids) white? newline) as line
  { locate lexbuf (Gate(id,Pcre.split ~pat:"," ids), line) }
| eof { locate lexbuf (EOF, "") }
