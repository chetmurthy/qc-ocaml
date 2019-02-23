(* Copyright 2019 Chetan Murthy *)
{
open Lexing
open Qasmsyntax

module LexState = struct
  type t = {
      mutable bol : bool ;
    }

  let mk () = { bol = true }
  let is_bol st = st.bol
  let is_notbol st = not st.bol

  let notbol_to_bol st =
    assert (is_notbol st) ;
    st.bol <- true

  let bol_to_notbol st =
    assert (is_bol st) ;
    st.bol <- false

end

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

rule header st =
  parse
| "OPEMQASM" white { grab_real st lexbuf }

and grab_real st =
  parse
| real_regexp { eat_header_suffix st (T_OPENQASM (RealNumeral.mk (Lexing.lexeme lexbuf))) lexbuf }

and eat_header_suffix st tok =
  parse
| white? ';' white? newline { grab_comment_suffix st tok lexbuf }

and grab_comment_suffix st tok =
  parse
| (white? comment_regexp)* { LexState.notbol_to_bol st ; (tok, Lexing.lexeme lexbuf) }

and grab_include st =
  parse
| [^ '"']+ { eat_include_suffix_1 st (T_INCLUDE (Lexing.lexeme lexbuf)) lexbuf }

and eat_include_suffix_1 st tok =
  parse
| '"' white? { eat_include_suffix_2 st tok lexbuf }

and eat_include_suffix_2 st tok =
  parse
| (newline | (white? comment_regexp)+) { (tok, Lexing.lexeme lexbuf) }

and token_notbol st =
  parse
| white { token_notbol st lexbuf }
| newline { LexState.notbol_to_bol st ; token_bol st lexbuf }

| ';' { grab_comment_suffix st T_SEMICOLON lexbuf }
| '{' { grab_comment_suffix st T_LBRACE lexbuf }
| '}' { grab_comment_suffix st T_RBRACE lexbuf }
| '[' { grab_comment_suffix st T_LBRACKET lexbuf }
| ']' { grab_comment_suffix st T_RBRACKET lexbuf }
| '(' { grab_comment_suffix st T_LPAREN lexbuf }
| ')' { grab_comment_suffix st T_RPAREN lexbuf }
| "==" { grab_comment_suffix st T_EQEQ lexbuf }
| ',' { grab_comment_suffix st T_COMMA lexbuf }
| '-' { grab_comment_suffix st T_DASH lexbuf }
| '+' { grab_comment_suffix st T_PLUS lexbuf }
| '*' { grab_comment_suffix st T_STAR lexbuf }
| "**" { grab_comment_suffix st T_STARSTAR lexbuf }
| '/' { grab_comment_suffix st T_SLASH lexbuf }
| '^' { grab_comment_suffix st T_CARET lexbuf }
| "->" { grab_comment_suffix st T_DASHGT lexbuf }

| "barrier" { grab_comment_suffix st T_BARRIER lexbuf }
| "cos" { grab_comment_suffix st T_COS lexbuf }
| "creg" { grab_comment_suffix st T_CREG lexbuf }
| "cx" { grab_comment_suffix st T_CX lexbuf }
| "exp" { grab_comment_suffix st T_EXP lexbuf }
| "gate" { grab_comment_suffix st T_GATE lexbuf }
| "if" { grab_comment_suffix st T_IF lexbuf }
| "ln" { grab_comment_suffix st T_LN lexbuf }
| "pi" { grab_comment_suffix st T_PI lexbuf }
| "qreg" { grab_comment_suffix st T_QREG lexbuf }
| "sin" { grab_comment_suffix st T_SIN lexbuf }
| "sqrt" { grab_comment_suffix st T_SQRT lexbuf }
| "tan" { grab_comment_suffix st T_TAN lexbuf }
| "u" { grab_comment_suffix st T_U lexbuf }
| "measure" { grab_comment_suffix st T_MEASURE lexbuf }
| "opaque" { grab_comment_suffix st T_OPAQUE lexbuf }
| "reset" { grab_comment_suffix st T_RESET lexbuf }

| integer_regexp { grab_comment_suffix st (T_INTEGER (int_of_string (Lexing.lexeme lexbuf))) lexbuf }
| id_regexp { grab_comment_suffix st (T_ID (Lexing.lexeme lexbuf)) lexbuf }

and token_bol st =
  parse
| white { LexState.bol_to_notbol st ; token_notbol st lexbuf }
| "#include" white '"' { LexState.bol_to_notbol st ; grab_include st lexbuf }
| "" { LexState.bol_to_notbol st ; token_notbol st lexbuf }

and token st =
  parse
| "" { if (LexState.is_bol st) then token_bol st lexbuf else token_notbol st lexbuf }
