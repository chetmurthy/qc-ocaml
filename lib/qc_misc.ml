(** -syntax camlp5o *)
open Pa_ppx_base.Pp_MLast
open Pa_ppx_utils
open Std
open Misc_functions

type loc = Ploc.t [@@deriving show]
let loc_to_yojson (_ : loc) = `String "<loc>"
let equal_loc _ _ = true
let compare_loc _ _ = 0

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
  let id_re = Pcre.regexp "^([A-Za-z](?:[A-Za-z_'0-9]*[A-Za-z_'])?)([0-9]*)$"
  let mk0 s n = (s,n)
  let mk s =
    let ss = Pcre.extract ~rex:id_re s in
    let strn = ss.(2) in
    let n = if strn = "" then -1 else int_of_string strn in
    (ss.(1),  n)

  let unmk (s,n) =
    if n = -1 then s else
      Printf.sprintf  "%s%d" s n

  let pp_hum pps x = Fmt.(pf pps "%s" (unmk x))

  let ofID ?(loc=Ploc.dummy) x = x
  let toID x = x
end

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

type file_type_t = QASM2 | QLAM [@@deriving (to_yojson, show, eq, ord)]

let include_path = ref []
let add_include (s : string) = Std.push include_path s

let with_include_path ~path f arg =
  let oinclude_path = !include_path in
  include_path := path ;
  try let rv = f arg in include_path := oinclude_path ; rv
  with exc ->
        include_path := oinclude_path ;
        raise exc

module Counter = struct
type t = {
    it : int ref
  }
let mk ?(base=0) () = { it = ref base }
let next { it=it } = 
  let n = !it in
  it := n+1 ;
  n

end

let sorted (l : int list) =
  let rec srec = function
      n::(m::_ as t) ->
       n <= m && srec t
    | [_] | [] -> true in
  srec l

let consecutive_ints (l : int list) =
  sorted l &&
    let min = List.hd l in
    let max = fst (Std.sep_last l) in
    l = Std.interval min max

let collapse_intervals l =
  let _ = assert (sorted l) in
  let rec cinterval (n,m) = function
        [] ->  ((n,m),[])
      | h::t as l ->
         if m+1 = h then cinterval (n,m+1) t
         else ((n,m), l)
  and crec acc = function
        [] -> List.rev acc
      | h::t ->
         let (p,t) = cinterval (h,h) t in
         crec (p::acc) t
      in
  crec [] l

let in_tmp_dir ?(preserve=false) f arg =
  let open Rresult in
  let open Bos in
  let ( let* ) x f = Rresult.(>>=) x f in
    if preserve then
      let* dir = OS.Dir.tmp ~mode:0o755 "tmp%s" in
      let* r = f dir arg in
      Ok (Some (dir,  r))
    else
      let* r = OS.Dir.with_tmp ~mode:0o755 "tmp%s"
                 f arg in
      let* dir = r in
      Ok None
