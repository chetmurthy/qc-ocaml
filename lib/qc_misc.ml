
open Pa_ppx_utils
open Std
open Misc_functions

type Pa_ppx_runtime_fat.Exceptions.t +=
    SyntaxError of string[@name "SyntaxError"]
[@@deriving show, sexp, yojson, eq]

module RealNumeral = struct
  type t = string[@@deriving (yojson, show, eq,  ord)]
  let mk s =
    s
  let unmk s = s
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

module ID = struct
  type t = string * int[@@deriving (yojson, show, eq, ord)]
  let id_re = Pcre.regexp "^([A-Za-z](?:[A-Za-z_0-9_]*[A-Za-z])?)([0-9]*)$"
  let mk0 s n = (s,n)
  let mk s =
    let ss = Pcre.extract ~rex:id_re s in
    let strn = ss.(2) in
    let n = if strn = "" then -1 else int_of_string strn in
    (ss.(1),  n)

  let unmk (s,n) =
    if n = -1 then s else
      Printf.sprintf  "%s%d" s n

end

module IDMap = Map.Make(ID)

let find_file_from ~path fname =
  try
    try_find (fun dir ->
        let fname = Printf.sprintf "%s/%s" dir fname in
        if Sys.file_exists fname then fname
        else failwith "caught") path
  with Failure _ ->
    Exc.die (Printf.sprintf "Qc_misc.open_file_from: cannot open file %s for read on path [%s]"
               fname
           (String.concat "; " path))
