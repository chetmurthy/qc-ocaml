(* Copyright 2019 Chetan Murthy, All rights reserved. *)

exception SyntaxError of string

(* a string that matches 

real      := ([0-9]+\.[0-9]*|[0-9]*\.[0-9]+)([eE][-+]?[0-9]+)?

*)

module RealNumeral = struct
  type t = string
  let mk s =
    s
end

type rawtoken =
  | T_EOF
  (* special symbols *)
  | T_OPENQASM of RealNumeral.t
  | T_INCLUDE of string

  (* special characters *)
  | T_SEMICOLON
  | T_LBRACE
  | T_RBRACE
  | T_LBRACKET
  | T_RBRACKET
  | T_LPAREN
  | T_RPAREN
  | T_EQEQ
  | T_COMMA
  | T_DASH
  | T_PLUS
  | T_STAR
  | T_STARSTAR
  | T_SLASH
  | T_CARET
  | T_DASHGT

  (* reserved words *)
  | T_BARRIER
  | T_COS
  | T_CREG
  | T_CX
  | T_EXP
  | T_GATE
  | T_IF
  | T_LN
  | T_PI
  | T_QREG
  | T_SIN
  | T_SQRT
  | T_TAN
  | T_U
  | T_MEASURE
  | T_OPAQUE
  | T_RESET

  | T_INTEGER of int
  | T_ID of string

type token = string * rawtoken

module LexState = struct
  type t = {
      mutable at_head : bool ;
    }

  let mk () = { at_head = true }
  let is_at_head st = st.at_head

  let past_head st =
    assert(is_at_head st) ;
    st.at_head <- false
end

let stream_of_lexer_eof eoftok lexer lexbuf =
let rec strec () =
    let tok = lexer lexbuf
    in if (eoftok = tok) then [< >]
       else [< 'tok; strec() >]
in [< strec () >]

let stream_of_lexer lexer lexbuf =
let rec strec () =
    try
    let tok = lexer lexbuf
    in [< 'tok; strec() >]
    with Failure _ -> [< >]
in [< strec () >]

let list_of_stream strm =
let rec listrec acc = parser
  [< 't ; strm >] -> listrec (t::acc) strm
| [< >] -> List.rev acc
in listrec [] strm

let ne_plist_with_sep sep elem = 
 let rec do_rec accumf = parser
  [< e = elem; l = (parser [< f = sep; l = do_rec (f (accumf e)) >] -> l | [< >] -> e) >] -> l
 in do_rec (fun e -> e)
