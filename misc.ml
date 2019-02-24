
let stream_of_lexer_eof_function eof_function lexer lexbuf =
let rec strec () =
    let tok = lexer lexbuf
    in if (eof_function tok) then [< >]
       else [< 'tok; strec() >]
in [< strec () >]

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

let plist elem = 
  let rec plist_rec accum = parser
     [< e = elem; strm >] -> plist_rec (e::accum) strm
   | [< >]                         -> (List.rev accum)
  in plist_rec []

let ne_plist elem = parser
  [< e = elem; l = (plist elem) >] -> (e,l)

let ne_plist_with_sep sep elem = 
 let rec do_rec = parser
  [< e = elem; l = (parser [< () = sep; l = do_rec >] -> l | [< >] -> []) >] -> e::l
 in do_rec

let plist_with_sep sep elem = parser
    [< l = (ne_plist_with_sep sep elem) >] -> l
  | [< >] -> []

let ne_plist_with_sep_function sep elem = 
 let rec do_rec accumf = parser
  [< e = elem; l = (parser [< f = sep; l = do_rec (f (accumf e)) >] -> l | [< >] -> e) >] -> l
 in do_rec (fun e -> e)

let clean_left_re = Pcre.regexp "^\\s*(\\S.*)?$"
let clean_right_re = Pcre.regexp "^(.*\\S)?\\s*$"

let cleanws s =
  let rv1 = Pcre.extract ~rex:clean_left_re s in
  let s = rv1.(1) in
  let rv2 = Pcre.extract ~rex:clean_right_re s in
  rv2.(1)
