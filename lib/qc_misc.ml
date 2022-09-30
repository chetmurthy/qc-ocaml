
open Misc_functions

exception SyntaxError of string

module RealNumeral = struct
  type t = string
  let mk s =
    s
end

module TokenAux = struct
  type t = {
      comments: string list ;
      startpos : Lexing.position ;
      endpos : Lexing.position ;
    }

  let mk coms lb = {
      comments = [cleanws coms] ;
      startpos = Lexing.lexeme_start_p lb ;
      endpos = Lexing.lexeme_end_p lb ;
    }

  let mt = {
      comments = [] ;
      startpos = Lexing.dummy_pos ;
      endpos = Lexing.dummy_pos ;
    }

  let append a1 a2 =
    {
      comments = a1.comments @ a2.comments ;
      startpos = a1.startpos ;
      endpos = a2.endpos ;
    }
  let appendlist l =
    assert (l <> []) ;
    List.fold_left append (List.hd l) (List.tl l)

  let comment_string a =
    String.concat "" a.comments

  let startpos a = a.startpos
  let endpos a = a.endpos
end
