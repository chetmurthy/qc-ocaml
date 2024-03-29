open Pa_ppx_base
open Ppxutil
open Qc_misc

module ME = struct

type t =
  QW
| QWA
| QWX of int
| CW
| CWA
| CDOTS
| LSTICK of string option
| RSTICK of string option
| DSTICK of string * int
| CGHOST of string
| NGHOST of string
| GHOST of string * int option
| GATE of string
| MULTIGATE of int * string * int option
| CTRL of int
| TARG
| METER
| SWAP of int option
| CWIDTH of int
| BARRIER of int
| L of t list
[@@deriving to_yojson, show, eq, ord]

let rec tolatex pps me =
  let s = match me with
      QW -> {|\qw|}
    | QWA  -> {|\qwa|}
    | QWX n -> Fmt.(str {|\qw[%d]|} n)
    | CW -> {|\cw|}
    | CWA -> {|\cwa|}
    | CDOTS -> {|\cdots|}
    | LSTICK None -> {|\lstick{}|}
    | LSTICK (Some s) -> Fmt.(str {|\lstick{%s}|} s)
    | RSTICK None -> {|\rstick{}|}
    | RSTICK (Some s) -> {|\rstick{%s}|}
    | DSTICK (s,n) -> Fmt.(str {|\dstick{_{_{\hspace{0.0em}%s}}} \cw \ar @@{<=} [%d,0]|} s n)
    | CGHOST s -> Fmt.(str {|\cghost{%s}|} s)
    | NGHOST s -> Fmt.(str {|\nghost{%s}|} s)
    | GHOST (s, None) -> Fmt.(str {|\ghost{%s}|} s)
    | GHOST (s, Some n) -> Fmt.(str {|\ghost{%s}_<<<{%d} |} s n)
    | GATE s -> Fmt.(str {|\gate{%s}|} s)
    | MULTIGATE (n, s, None) -> Fmt.(str {|\multigate{%d}{%s}|} n s)
    | MULTIGATE (n, s, Some m) -> Fmt.(str {|\multigate{%d}{%s}_<<<{%d}|} n s m)
    | CTRL n -> Fmt.(str {|\ctrl{%d}|} n)
    | TARG -> {|\targ|}
    | SWAP None -> {|\qswap|}
    | SWAP (Some n) -> Fmt.(str {|\qswap \qwx[%d]|} n)
    | METER -> {|\meter|} 
    | CWIDTH n -> Fmt.(str {|\lstick{/_{_{%d}}} \cw |} n)
    | BARRIER n -> Fmt.(str {|\barrier[0em]{%d}|} n)
    | L l -> Fmt.(str "%a" (list ~sep:(const string " ") tolatex) l) in
  Fmt.(pf pps "%s" s)
end

module MatrixElement = ME

module Matrix = struct
  type t = { it : ME.t array array ; rows : int ; cols : int }
  let mk rows cols =
    if not (rows > 0) then
      Fmt.(failwithf "Matrix.mk: rows must be > 0")
    else if not (cols > 0) then
      Fmt.(failwithf "Matrix.mk: cols must be > 0")
    else
    { it = Array.make_matrix rows cols ME.QW ; rows ; cols }

  let ofList ll =
    let rows = List.length ll in
    if rows = 0 then
      Fmt.(failwithf "Matrix.ofList: rows must be > 0")
    else
    let la = List.map Array.of_list ll in
    let cols = Array.length (List.hd la) in
    if cols = 0 then
      Fmt.(failwithf "Matrix.ofList: cols must be > 0")
    else if not (la |> List.for_all (fun a -> cols = Array.length a)) then
      Fmt.(failwithf "Matrix.ofList: all rows must have same length (# cols)")
    else
    let it = Array.of_list la in
    { it ; rows ; cols }

  let rec set m i j v =
    if i < 0 then set m (i+m.rows) j v
    else if j < 0 then set m i (j+m.cols) v
    else m.it.(i).(j) <- v

  let rec set_row m i v =
    if i < 0 then set_row m (i+m.rows) v
    else
    for j = 0 to m.cols - 1 do
      set m i j v
    done

let pp_cols ~from_col ~to_col pps m =
  let a = m.it in
  let la = Array.to_list a in
  let la = List.map (fun a -> Array.sub a from_col (to_col - from_col)) la in
  let ll = List.map Array.to_list la in
  Fmt.(pf pps "%a"
         (list ~sep:(const string " \\\\\n") (list ~sep:(const string " & ") ME.tolatex)) ll
  )

let pp pps m = pp_cols ~from_col:0 ~to_col:m.cols pps m

let prolog = {|
\documentclass[border=2px]{standalone}

\usepackage[braket, qm]{qcircuit}
\usepackage{graphicx}

\begin{document}
\scalebox{1.0}{
\Qcircuit @C=1.0em @R=0.2em @!R { \\
|}

let epilog = {|
\\ }}
\end{document}
|}

let tolatex ?from_col ?to_col m =
  let from_col = match from_col with None -> 0 | Some n -> n in
  let to_col = match to_col with None -> m.cols | Some n -> n in
  Fmt.(str "%s%a%s" prolog (pp_cols ~from_col ~to_col) m epilog)

end

module Exec = struct
  open Rresult
  open Bos
  let ( let* ) x f = Rresult.(>>=) x f

let latex2png_cmd = Cmd.v "latex2png"
let latex2png f =
  let doit = Cmd.(latex2png_cmd % (Fpath.to_string f)) in
  OS.Cmd.(run_out doit |> to_string)

let imv_cmd = Cmd.v "imv-x11"
let imv f =
  let doit = Cmd.(imv_cmd % (Fpath.to_string f)) in
  OS.Cmd.(run_out doit |> to_string)

  let generate dir txt =
    let texf = Fpath.(dir // v "circuit.tex") in
    let pngf = Fpath.(dir // v "circuit.png") in
    let* () = OS.File.write texf txt in
    let* txt = latex2png texf in
    Ok pngf

  let display pngf =
    let* _ = imv pngf in
    Ok ()

  let generate_and_display dir txt =
    let* texf = generate dir txt in
    let* () = display texf in
    Ok texf

  let latex ?(preserve=false) ?(display=true) txt =
    in_tmp_dir ~preserve (if display then generate_and_display else generate) txt

  let latex_file ?(preserve=false) ?(display=true) texf =
    let* txt = OS.File.read texf in
    latex ~preserve ~display txt

end
