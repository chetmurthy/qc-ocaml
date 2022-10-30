
open Pa_ppx_base.Pp_MLast
open Pa_ppx_utils
open Std
open Misc_functions

type loc = Ploc.t
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

module Latex = struct
  open Rresult
  open Bos
  let (let*) x f = Rresult.(>>=) x f

let latex2png_cmd = Cmd.v "latex2png"
let latex2png f =
  let doit = Cmd.(latex2png_cmd % (Fpath.to_string f)) in
  OS.Cmd.(run_out doit |> to_string)

let imv_cmd = Cmd.v "imv-x11"
let imv f =
  let doit = Cmd.(imv_cmd % (Fpath.to_string f)) in
  OS.Cmd.(run_out doit |> to_string)

  let dolatex dir txt =
    let texf = Fpath.(dir // v "circuit.tex") in
    let pngf = Fpath.(dir // v "circuit.png") in
    let* () = OS.File.write texf txt in
    let* txt = latex2png texf in
    let* _ = imv pngf in
    Ok ()

  let latex ?(preserve=false) txt =
    if preserve then
      let* dir = OS.Dir.tmp ~mode:0o755 "latex%s" in
      let* r = dolatex dir txt in
      Ok (Some dir)
    else
      let* r = OS.Dir.with_tmp ~mode:0o755 "latex%s"
                 dolatex txt in
      let* dir = r in
      Ok None

  let latex_file ?(preserve=false) texf =
    let* txt = OS.File.read texf in
    latex ~preserve txt

end
