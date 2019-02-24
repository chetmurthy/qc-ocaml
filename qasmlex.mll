(* Copyright 2019 Chetan Murthy, All rights reserved. *)
{
open Lexing
open Qasmsyntax

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
| real_regexp { eat_header_suffix ("", T_OPENQASM (RealNumeral.mk (Lexing.lexeme lexbuf))) lexbuf }

and eat_header_suffix rv =
  parse
| white? ';' white? newline { rv }

and grab_comment_suffix st tok =
  parse
| (white? comment_regexp)* { (tok, Lexing.lexeme lexbuf) }

and grab_include wscom =
  parse
| [^ '"']+ { eat_include_suffix_1 (wscom, T_INCLUDE (Lexing.lexeme lexbuf)) lexbuf }

and eat_include_suffix_1 rv =
  parse
| '"' white? ";" { rv }

and body_token wscom =
  parse
| white+ { assert (wscom = "") ; body_token "" lexbuf }
| newline { assert (wscom = "") ; body_token "" lexbuf }
| (comment_regexp (white|newline)*)+ { body_token (Lexing.lexeme lexbuf) lexbuf }

| "include" white+ '"' { grab_include wscom lexbuf }
| ';' { (wscom, T_SEMICOLON) }
| '{' { (wscom, T_LBRACE) }
| '}' { (wscom, T_RBRACE) }
| '[' { (wscom, T_LBRACKET) }
| ']' { (wscom, T_RBRACKET) }
| '(' { (wscom, T_LPAREN) }
| ')' { (wscom, T_RPAREN) }
| "==" { (wscom, T_EQEQ) }
| ',' { (wscom, T_COMMA) }
| '-' { (wscom, T_DASH) }
| '+' { (wscom, T_PLUS) }
| '*' { (wscom, T_STAR) }
| "**" { (wscom, T_STARSTAR) }
| '/' { (wscom, T_SLASH) }
| '^' { (wscom, T_CARET) }
| "->" { (wscom, T_DASHGT) }

| "barrier" { (wscom, T_BARRIER) }
| "cos" { (wscom, T_COS) }
| "creg" { (wscom, T_CREG) }
| "CX" { (wscom, T_CX) }
| "exp" { (wscom, T_EXP) }
| "gate" { (wscom, T_GATE) }
| "if" { (wscom, T_IF) }
| "ln" { (wscom, T_LN) }
| "pi" { (wscom, T_PI) }
| "qreg" { (wscom, T_QREG) }
| "sin" { (wscom, T_SIN) }
| "sqrt" { (wscom, T_SQRT) }
| "tan" { (wscom, T_TAN) }
| "U" { (wscom, T_U) }
| "measure" { (wscom, T_MEASURE) }
| "opaque" { (wscom, T_OPAQUE) }
| "reset" { (wscom, T_RESET) }
| eof { (wscom, T_EOF) }

| real_regexp { (wscom, T_REAL (Lexing.lexeme lexbuf)) }
| integer_regexp { (wscom, T_INTEGER (int_of_string (Lexing.lexeme lexbuf))) }
| id_regexp { (wscom, T_ID (Lexing.lexeme lexbuf)) }

and token st = parse
| "" { if LexState.is_at_head st then begin
           LexState.past_head st ;
           header lexbuf
         end
       else body_token "" lexbuf
}

{
  let make_lexer buf =
    let st = LexState.mk () in
    let lb = Lexing.from_string buf in
    stream_of_lexer_eof ("", T_EOF) (token st) lb

  let make_body_lexer buf =
    let lb = Lexing.from_string buf in
    stream_of_lexer_eof ("", T_EOF) (body_token "") lb
}
