(* Copyright 2019 Chetan Murthy, All rights reserved. *)
{
open Lexing
open Pa_ppx_utils
open Std
open Misc_functions
open Qc_misc
open Qasm2syntax

let locate ~comments lb v =
  let comments = cleanws comments in
  let spos = Lexing.lexeme_start_p lb in
  let epos = Lexing.lexeme_end_p lb in
  let open Lexing in
  let loc = Ploc.make_loc spos.pos_fname spos.pos_lnum spos.pos_bol (spos.pos_cnum, epos.pos_cnum) comments in
  (loc, v)

}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let lc_alpha = ['a'-'z']
let uc_alpha = ['A'-'Z']
let digit = ['0'-'9']
let id_regexp  = lc_alpha (lc_alpha|digit|uc_alpha|'_')*
let mantissa = ((digit+ '.' digit*) | (digit* '.' digit+))
let exponent = ['e' 'E'] ['+' '-']? digit+
let real_regexp = mantissa exponent?
let integer_regexp = digit+

let comment_regexp = "//" [^ '\r' '\n']* newline

rule header =
  parse
| (comment_regexp | white | newline)* "OPENQASM" white { grab_real lexbuf }

and grab_real =
  parse
| real_regexp { eat_header_suffix (locate ~comments:"" lexbuf (T_OPENQASM (RealNumeral.mk (Lexing.lexeme lexbuf)))) lexbuf }

and eat_header_suffix rv =
  parse
| white? ';' white? newline { rv }

and grab_comment_suffix st tok =
  parse
| (white? comment_regexp)* { (tok, Lexing.lexeme lexbuf) }

and grab_include wscom =
  parse
| [^ '"']+ { eat_include_suffix_1 (locate ~comments:wscom lexbuf (T_INCLUDE (Lexing.lexeme lexbuf))) lexbuf }

and eat_include_suffix_1 rv =
  parse
| '"' white? ";" { rv }

and body_token0 wscom =
  parse
| white+ { assert (wscom = "") ; body_token0 "" lexbuf }
| newline { assert (wscom = "") ; body_token0 "" lexbuf }
| (comment_regexp (white|newline)*)+ { body_token0 (Lexing.lexeme lexbuf) lexbuf }

| "include" white+ '"' { grab_include wscom lexbuf }
| ';' { locate ~comments:wscom lexbuf T_SEMICOLON }
| '{' { locate ~comments:wscom lexbuf T_LBRACE }
| '}' { locate ~comments:wscom lexbuf T_RBRACE }
| '[' { locate ~comments:wscom lexbuf T_LBRACKET }
| ']' { locate ~comments:wscom lexbuf T_RBRACKET }
| '(' { locate ~comments:wscom lexbuf T_LPAREN }
| ')' { locate ~comments:wscom lexbuf T_RPAREN }
| "==" { locate ~comments:wscom lexbuf T_EQEQ }
| ',' { locate ~comments:wscom lexbuf T_COMMA }
| '-' { locate ~comments:wscom lexbuf T_DASH }
| '+' { locate ~comments:wscom lexbuf T_PLUS }
| '*' { locate ~comments:wscom lexbuf T_STAR }
| "**" { locate ~comments:wscom lexbuf T_STARSTAR }
| '/' { locate ~comments:wscom lexbuf T_SLASH }
| '^' { locate ~comments: wscom lexbuf T_CARET }
| "->" { locate ~comments:wscom lexbuf T_DASHGT }

| "barrier" { locate ~comments:wscom lexbuf T_BARRIER }
| "cos" { locate ~comments:wscom lexbuf T_COS }
| "creg" { locate ~comments:wscom lexbuf T_CREG }
| "CX" { locate ~comments:wscom lexbuf T_CX }
| "exp" { locate ~comments:wscom lexbuf T_EXP }
| "gate" { locate ~comments:wscom lexbuf T_GATE }
| "if" { locate ~comments:wscom lexbuf T_IF }
| "ln" { locate ~comments:wscom lexbuf T_LN }
| "pi" { locate ~comments:wscom lexbuf T_PI }
| "qreg" { locate ~comments:wscom lexbuf T_QREG }
| "sin" { locate ~comments:wscom lexbuf T_SIN }
| "sqrt" { locate ~comments:wscom lexbuf T_SQRT }
| "tan" { locate ~comments:wscom lexbuf T_TAN }
| "U" { locate ~comments:wscom lexbuf T_U }
| "SWAP" { locate ~comments:wscom lexbuf (T_SWAPGATE (Lexing.lexeme lexbuf)) }
| "measure" { locate ~comments:wscom lexbuf T_MEASURE }
| "opaque" { locate ~comments:wscom lexbuf T_OPAQUE }
| "reset" { locate ~comments:wscom lexbuf T_RESET }
| eof { locate ~comments:wscom lexbuf T_EOF }

| real_regexp { locate ~comments:wscom lexbuf (T_REAL (Lexing.lexeme lexbuf)) }
| integer_regexp { locate ~comments:wscom lexbuf (T_INTEGER (int_of_string (Lexing.lexeme lexbuf))) }
| id_regexp { locate ~comments:wscom lexbuf (T_ID (Lexing.lexeme lexbuf)) }

{
  let token st lexbuf =
    try
      if LexState.is_at_head st then begin
          LexState.past_head st ;
          header lexbuf
        end
      else body_token0 "" lexbuf
    with Failure _ ->
      let p = Lexing.lexeme_start_p lexbuf in
      raise (SyntaxError (Printf.sprintf "lexing: failed in file \"%s\" at char %d" p.Lexing.pos_fname p.Lexing.pos_cnum))

  let body_token coms lexbuf =
    try
      body_token0 coms lexbuf
    with Failure _ ->
      let p = Lexing.lexeme_start_p lexbuf in
      raise (SyntaxError (Printf.sprintf "lexing: failed in file \"%s\" at char %d" p.Lexing.pos_fname p.Lexing.pos_cnum))

  let make_lexer ?(fname="") buf =
    let st = LexState.mk () in
    let lb = Lexing.from_string buf in
    lb.lex_start_p <- { lb.lex_start_p with pos_fname = fname } ;
    lb.lex_curr_p <- { lb.lex_curr_p with pos_fname = fname } ;
    stream_of_lexer_eof_function (fun (_, e) -> e = T_EOF) (token st) lb

  let make_lexer_from_channel ?(fname="") ic =
    let st = LexState.mk () in
    let lb = Lexing.from_channel ic in
    lb.lex_start_p <- { lb.lex_start_p with pos_fname = fname } ;
    lb.lex_curr_p <- { lb.lex_curr_p with pos_fname = fname } ;
    stream_of_lexer_eof_function (fun (_, e) -> e = T_EOF) (token st) lb

  let make_body_lexer ?(fname="") buf =
    let lb = Lexing.from_string buf in
    lb.lex_start_p <- { lb.lex_start_p with pos_fname = fname } ;
    lb.lex_curr_p <- { lb.lex_curr_p with pos_fname = fname } ;
    stream_of_lexer_eof_function (fun (_, e) -> e = T_EOF) (body_token "") lb

  let make_body_lexer_from_channel ?(fname="") ic =
    let lb = Lexing.from_channel ic in
    lb.lex_start_p <- { lb.lex_start_p with pos_fname = fname } ;
    lb.lex_curr_p <- { lb.lex_curr_p with pos_fname = fname } ;
    stream_of_lexer_eof_function (fun (_, e) -> e = T_EOF) (body_token "") lb
}
