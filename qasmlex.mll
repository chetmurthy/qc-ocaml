(* Copyright 2019 Chetan Murthy, All rights reserved. *)
{
open Lexing
open Misc_functions
open Qasmsyntax

module TA = TokenAux

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
let integer_regexp = '-'? digit+

let comment_regexp = "//" [^ '\r' '\n']* newline

rule header =
  parse
| "OPENQASM" white { grab_real lexbuf }

and grab_real =
  parse
| real_regexp { eat_header_suffix (TA.mk "" lexbuf, T_OPENQASM (RealNumeral.mk (Lexing.lexeme lexbuf))) lexbuf }

and eat_header_suffix rv =
  parse
| white? ';' white? newline { rv }

and grab_comment_suffix st tok =
  parse
| (white? comment_regexp)* { (tok, Lexing.lexeme lexbuf) }

and grab_include wscom =
  parse
| [^ '"']+ { eat_include_suffix_1 (TA.mk wscom lexbuf, T_INCLUDE (Lexing.lexeme lexbuf)) lexbuf }

and eat_include_suffix_1 rv =
  parse
| '"' white? ";" { rv }

and body_token wscom =
  parse
| white+ { assert (wscom = "") ; body_token "" lexbuf }
| newline { assert (wscom = "") ; body_token "" lexbuf }
| (comment_regexp (white|newline)*)+ { body_token (Lexing.lexeme lexbuf) lexbuf }

| "include" white+ '"' { grab_include wscom lexbuf }
| ';' { (TA.mk wscom lexbuf, T_SEMICOLON) }
| '{' { (TA.mk wscom lexbuf, T_LBRACE) }
| '}' { (TA.mk wscom lexbuf, T_RBRACE) }
| '[' { (TA.mk wscom lexbuf, T_LBRACKET) }
| ']' { (TA.mk wscom lexbuf, T_RBRACKET) }
| '(' { (TA.mk wscom lexbuf, T_LPAREN) }
| ')' { (TA.mk wscom lexbuf, T_RPAREN) }
| "==" { (TA.mk wscom lexbuf, T_EQEQ) }
| ',' { (TA.mk wscom lexbuf, T_COMMA) }
| '-' { (TA.mk wscom lexbuf, T_DASH) }
| '+' { (TA.mk wscom lexbuf, T_PLUS) }
| '*' { (TA.mk wscom lexbuf, T_STAR) }
| "**" { (TA.mk wscom lexbuf, T_STARSTAR) }
| '/' { (TA.mk wscom lexbuf, T_SLASH) }
| '^' { (TA.mk wscom lexbuf, T_CARET) }
| "->" { (TA.mk wscom lexbuf, T_DASHGT) }

| "barrier" { (TA.mk wscom lexbuf, T_BARRIER) }
| "cos" { (TA.mk wscom lexbuf, T_COS) }
| "creg" { (TA.mk wscom lexbuf, T_CREG) }
| "CX" { (TA.mk wscom lexbuf, T_CX) }
| "exp" { (TA.mk wscom lexbuf, T_EXP) }
| "gate" { (TA.mk wscom lexbuf, T_GATE) }
| "if" { (TA.mk wscom lexbuf, T_IF) }
| "ln" { (TA.mk wscom lexbuf, T_LN) }
| "pi" { (TA.mk wscom lexbuf, T_PI) }
| "qreg" { (TA.mk wscom lexbuf, T_QREG) }
| "sin" { (TA.mk wscom lexbuf, T_SIN) }
| "sqrt" { (TA.mk wscom lexbuf, T_SQRT) }
| "tan" { (TA.mk wscom lexbuf, T_TAN) }
| "U" { (TA.mk wscom lexbuf, T_U) }
| "measure" { (TA.mk wscom lexbuf, T_MEASURE) }
| "opaque" { (TA.mk wscom lexbuf, T_OPAQUE) }
| "reset" { (TA.mk wscom lexbuf, T_RESET) }
| eof { (TA.mk wscom lexbuf, T_EOF) }

| real_regexp { (TA.mk wscom lexbuf, T_REAL (Lexing.lexeme lexbuf)) }
| integer_regexp { (TA.mk wscom lexbuf, T_INTEGER (int_of_string (Lexing.lexeme lexbuf))) }
| id_regexp { (TA.mk wscom lexbuf, T_ID (Lexing.lexeme lexbuf)) }

and token st = parse
| "" { try
      if LexState.is_at_head st then begin
          LexState.past_head st ;
          header lexbuf
        end
      else body_token "" lexbuf
    with Failure _ ->
      let p = Lexing.lexeme_start_p lexbuf in
      raise (SyntaxError (Printf.sprintf "lexing: failed in file \"%s\" at char %d" p.Lexing.pos_fname p.Lexing.pos_cnum))
}

{
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
