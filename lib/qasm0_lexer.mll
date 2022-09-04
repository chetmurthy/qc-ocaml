(* Copyright 2019 Chetan Murthy, All rights reserved. *)
{
open Lexing
open Pa_ppx_utils
open Std
open Misc_functions
open Qasm0_tokens

let locate lb v =
  let loc = Ploc.make_unlined (Lexing.lexeme_start lb, Lexing.lexeme_end lb) in
  (loc, v)

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
let comment = "#" [^ '\n' '\r']*

rule line = parse
  (comment newline) as line { locate lexbuf (Comment line, line) }
| (white? newline) { line lexbuf }
| (white? "qubit" white (id as id) white? comment? newline) as line { locate lexbuf (Qubit id, line) }
| (white? "cbit" white (id as id) white? comment? newline) as line { locate lexbuf (Cbit id, line) }
| (white? "def" white (id as id)","(number as num)","(sqstring as tex) white? comment? newline) as line
  { locate lexbuf (Def(id,int_of_string num,tex), line) }
| (white? "defbox" white (id as id)","(number as num1)","(number as num2)","(sqstring as tex) white? comment? newline) as line
  { locate lexbuf (Defbox(id,int_of_string num1,int_of_string num2,tex), line) }
| (white? (id as id) white (comma_sep_ids as ids) white? comment? newline) as line
  { locate lexbuf (Gate(id,Pcre.split ~pat:"," ids), line) }
| eof { locate lexbuf (EOF, "") }

{
  let token lexbuf =
    try
      line lexbuf
    with Failure _ ->
      let p = Lexing.lexeme_start_p lexbuf in
      raise (Qasmsyntax.SyntaxError (Printf.sprintf "lexing: failed in file \"%s\" at char %d" p.Lexing.pos_fname p.Lexing.pos_cnum))

  let make_lexer ?(fname="") buf =
    let lb = Lexing.from_string buf in
    lb.lex_start_p <- { lb.lex_start_p with pos_fname = fname } ;
    lb.lex_curr_p <- { lb.lex_curr_p with pos_fname = fname } ;
    stream_of_lexer_eof_function (fun (_, (e,_)) -> e = EOF) token lb

  let make_lexer_from_channel ?(fname="") ic =
    let lb = Lexing.from_channel ic in
    lb.lex_start_p <- { lb.lex_start_p with pos_fname = fname } ;
    lb.lex_curr_p <- { lb.lex_curr_p with pos_fname = fname } ;
    stream_of_lexer_eof_function (fun (_, (e,_)) -> e = EOF) token lb

}
